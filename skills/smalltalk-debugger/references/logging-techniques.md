# Transcript Logging Techniques

## Basic Procedure

1. Open the Transcript window:
```smalltalk
Transcript open
```

2. Log values using `crShow:` with a format string:
```smalltalk
Transcript crShow: ('##DEBUG## var1:{1}' format: {var1})
```

- `crShow:` prepends a newline, keeping output readable
- The `##DEBUG##` prefix makes it easy to search for and remove logging code later
- Format strings eliminate the need for explicit `printString` calls (more concise than `'var1:', var1 printString`)

3. Read the Transcript output via `read_screen`:
```
mcp__smalltalk-interop__read_screen: target_type='transcript'
```

## Tips

### Tracing call order over time

Include `DateAndTime current` in the format string:
```smalltalk
Transcript crShow: ('##DEBUG## @{1}' format: {DateAndTime current})
```

### Finding the call site (stack trace output)

Use `thisContext shortStack` or `thisContext printStackOfSize:`:
```smalltalk
Transcript crShow: ('##DEBUG## stack:{1}' format: {thisContext shortStack})
Transcript crShow: ('##DEBUG## stack:{1}' format: {thisContext printStackOfSize: 20})  "longer stack"
```

### Headless images

Eval the following to redirect Transcript output to a file:
```smalltalk
NonInteractiveTranscript file install
```

Subsequent output is written to `PharoTranscript.log`.
