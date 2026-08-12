# Transcript Logging Techniques

## Basic Procedure

1. Open the Transcript window:
```smalltalk
Transcript open
```

2. Log values using `show:` with a format string:
```smalltalk
Transcript show: ('##DEBUG## var1:{1}' format: {var1})
```

- To prepends a newline to each log entry, use `crShow:` (Pharo), `showln:` (Squeak) or `cr; show:` (both).
- The `##DEBUG##` prefix makes it easy to search for and remove logging code later
- Format strings eliminate the need for explicit `printString` calls (more concise than `'var1:', var1 printString`)

3. Read the Transcript output via `read_screen`:

## Tips

### Tracing call order over time

Include `DateAndTime current` in the format string:
```smalltalk
Transcript cr; show: ('##DEBUG## @{1}' format: {DateAndTime current})
```

### Finding the call site (stack trace output)

Use `thisContext shortStack`:

```smalltalk
Transcript cr; show: ('##DEBUG## stack:{1}' format: {thisContext shortStack})
```

For getting longer stack, use `thisContext printStackOfSize:` (Pharo), or `thisContext stackOfSize:` (Squeak).

#### Pharo

```smalltalk
Transcript cr; show: ('##DEBUG## stack:{1}' format: {thisContext printStackOfSize: 20})  "longer stack"
```

#### Squeak

```smalltalk
Transcript cr; show: ('##DEBUG## stack:{1}' format: {thisContext stackOfSize: 20})  "longer stack"
```

### Headless images (Pharo only)

Eval the following to redirect Transcript output to a file:

```smalltalk
NonInteractiveTranscript file install
```

Subsequent output is written to `PharoTranscript.log`.
