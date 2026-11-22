import std/[streams, strutils, unicode, tables, sugar]
export tables

type
  SystemdEntry* = object
    case
    of BlankLine:
      discard
    of Comment:
      text*: string
    of Section:
      name*: string
    of Directive:
      key*, value*: string

  SystemdFileKind* = enum
    Service = "service"
    Socket = "socket"
    Target = "target"
    Timer = "timer"
    Path = "path"
    Mount = "mount"
    Automount = "automount"
    Swap = "swap"
    Device = "device"
    Slice = "slice"
    Scope = "scope"
  SystemdFile* = object
    kind*: SystemdFileKind
    path*: string
    systemFile*: bool
    entries*: seq[SystemdEntry]

  EntrySpan* = tuple[first, pastLast: int]

  SystemdSyntaxError* = object of ValueError
    line*: int

const
  MaxLineLength = 1024 * 1024
  Bom = "\xEF\xBB\xBF"
  CommentMarkers = {'#', ';'}
  LineBreaks = {'\r', '\n'}

let
  SystemdFileKindRepr* = collect:
    for kind in low(SystemdFileKind) .. high(SystemdFileKind):
      {$kind: kind}

func syntaxError(line: int, message: string): ref SystemdSyntaxError =
  result = newException(SystemdSyntaxError, "line " & $line & ": " & message)
  result.line = line

func isComment(line: string): bool =
  result = line.len > 0 and line[0] in CommentMarkers

func isSectionName(name: string): bool =
  result = name.len > 0
  for ch in name:
    if ch < ' ' or ch in {'\x7F', '\\', '"', '\''}:
      result = false
      break

func hasContinuation(line: string): bool =
  var index = line.high
  while index >= 0 and line[index] == '\\':
    dec index
  result = (line.high - index) mod 2 != 0

func validateLine(line: string, number: int) {.raises: [SystemdSyntaxError].} =
  if '\0' in line or validateUtf8(line) >= 0:
    raise syntaxError(number, "invalid UTF-8 text")

func parseEntry(line: string, number: int): SystemdEntry
    {.raises: [SystemdSyntaxError].} =
  let text = strutils.strip(line)
  if text.len == 0:
    result = BlankLine()
  elif text[0] == '[':
    if text.len < 2 or text[^1] != ']':
      raise syntaxError(number, "invalid section header")
    let name = text[1 ..< text.high]
    if not name.isSectionName:
      raise syntaxError(number, "invalid section name")
    result = Section(name: name)
  else:
    let separator = text.find('=')
    if separator < 0:
      raise syntaxError(number, "expected a directive with '='")
    let key = strutils.strip(text[0 ..< separator])
    if key.len == 0:
      raise syntaxError(number, "missing directive name")
    result = Directive(key: key, value: strutils.strip(text[separator + 1 .. ^1]))

iterator physicalLines(stream: Stream): tuple[text: string, span: EntrySpan]
    {.raises: [IOError, OSError, SystemdSyntaxError].} =
  var buffer: array[8192, char]
  var line: string
  var number = 1
  var previousBreak: char
  var offset, first: int
  while true:
    let count = stream.readData(addr buffer[0], buffer.len)
    if count == 0:
      break
    for index in 0 ..< count:
      let ch = buffer[index]
      inc offset
      if ch in LineBreaks:
        if previousBreak in LineBreaks and previousBreak != ch:
          previousBreak = '\0'
          first = offset
          continue
        yield (line, (first, offset - 1))
        first = offset
        line.setLen(0)
        inc number
        previousBreak = ch
      else:
        if line.len == MaxLineLength:
          raise syntaxError(number, "line exceeds 1 MiB")
        line.add(ch)
        previousBreak = '\0'
  if line.len > 0:
    yield (line, (first, offset))

proc parseSystemdFile(stream: Stream, spans: ptr seq[EntrySpan]): SystemdFile
    {.raises: [IOError, OSError, SystemdSyntaxError].} =
  var logicalLine: string
  var number, start: int
  var span: EntrySpan
  for (physicalLine, bounds) in stream.physicalLines:
    inc number
    let line =
      if number == 1 and physicalLine.startsWith(Bom): physicalLine[Bom.len .. ^1]
      else: physicalLine
    line.validateLine(number)
    if strutils.strip(line, trailing = false).isComment:
      if logicalLine.len == 0:
        result.entries.add Comment(text: line)
        if spans != nil: spans[].add bounds
      continue
    if logicalLine.len == 0:
      start = number
      span.first = bounds.first
    span.pastLast = bounds.pastLast
    if logicalLine.len + line.len > MaxLineLength:
      raise syntaxError(start, "continued line exceeds 1 MiB")
    logicalLine.add(line)
    if line.hasContinuation:
      logicalLine[^1] = ' '
    else:
      result.entries.add parseEntry(logicalLine, start)
      if spans != nil: spans[].add span
      logicalLine.setLen(0)
  if logicalLine.len > 0:
    result.entries.add parseEntry(logicalLine, start)
    if spans != nil: spans[].add span

proc readSystemdFile*(stream: Stream): SystemdFile
    {.raises: [IOError, OSError, SystemdSyntaxError].} =
  parseSystemdFile(stream, nil)

proc readSystemdFile*(stream: Stream, spans: var seq[EntrySpan]): SystemdFile =
  spans.setLen(0)
  parseSystemdFile(stream, addr spans)

func formatEntry*(entry: SystemdEntry, number: int): string
    {.raises: [SystemdSyntaxError].} =
  case entry
  of BlankLine:
    discard
  of Comment(text):
    if not strutils.strip(text, trailing = false).isComment:
      raise syntaxError(number, "invalid comment")
    result = text
  of Section(name):
    if not name.isSectionName:
      raise syntaxError(number, "invalid section name")
    result = "[" & name & "]"
  of Directive(key, value):
    if key.find(LineBreaks) >= 0 or value.find(LineBreaks) >= 0:
      raise syntaxError(number, "entry contains a line break")
    let name = strutils.strip(key)
    if name.len == 0 or '=' in name or name[0] in CommentMarkers + {'['}:
      raise syntaxError(number, "invalid directive name")
    result = name & "=" & strutils.strip(value)
    if result.startsWith(Bom):
      result = " " & result
    if result.hasContinuation:
      result.add(' ')
  if result.find(LineBreaks) >= 0:
    raise syntaxError(number, "entry contains a line break")
  if result.len > MaxLineLength:
    raise syntaxError(number, "line exceeds 1 MiB")
  result.validateLine(number)

proc writeSystemdFile*(stream: Stream, document: SystemdFile)
    {.raises: [IOError, OSError, SystemdSyntaxError].} =
  var text: string
  for index, entry in document.entries:
    text.add formatEntry(entry, index + 1)
    text.add '\n'
  stream.write(text)
