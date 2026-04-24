---
name: st-eval
description: Execute Smalltalk code snippet
allowed-tools:
  - mcp__smalltalk-interop__eval
---

# Execute Smalltalk Code

Execute arbitrary Smalltalk code snippets for quick testing, verification, or connection checks.

## Usage

```
/st-eval 1 + 1
/st-eval Smalltalk version
/st-eval MyClass new doSomething
```

## Implementation

Uses `mcp__smalltalk-interop__eval`.

## Examples

```smalltalk
"Connection check"
Smalltalk version

"Simple expression"
#(1 2 3 4 5) select: [:n | n even]

"Object creation"
Person new firstName: 'Alice'; printString

"Error handling - capture result and error in array"
| result |
result := Array new: 2.
[ result at: 1 put: (10 / 0) ]
  on: Error do: [:ex | result at: 2 put: ex description].
^ result

"Inspect intermediate values"
| step1 step2 |
step1 := objA computeStep1.
step2 := step1 processStep2.
^ { 'step1' -> step1 printString. 'step2' -> step2 printString } asDictionary printString
```

## Tips

- Use `printString` when returning objects to get readable output
- Use `on: Error do:` pattern to capture errors without crashing
- See `smalltalk-debugger` skill for more debugging patterns

## Workaround for Blocking Operations

If `eval` times out without returning a response, the code may be opening a modal or otherwise blocking the image.
Use `fork` to run the operation in a background process and return a dummy value immediately:

```smalltalk
[ <original blocking expression> ] fork.
^ nil "dummy return value for mcp response"
```
