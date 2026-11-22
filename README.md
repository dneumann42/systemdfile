# Systemd file

A Nim library for reading and writing systemd unit documents, shared by
Alatar and Systemd UI. It retains comments, blank lines, directive order,
and repeated keys. The reader can also return byte spans for editors.

```nim
import std/streams
import systemdfile

let stream = newStringStream("[Service]\nType=oneshot\n")
let document = readSystemdFile(stream)
stream.close()
```

The library parses unit syntax. The systemd manager interprets individual
directives and controls unit installation and activation.
