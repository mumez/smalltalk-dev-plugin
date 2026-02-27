---
name: st:eval
description: Execute Smalltalk code snippet
allowed-tools:
  - mcp__smalltalk-interop__eval
---

# Execute Smalltalk Code

Execute arbitrary Smalltalk code snippets for quick testing, verification, or connection checks.

## Usage

```bash
/st:eval 1 + 1
/st:eval Smalltalk version
/st:eval MyClass new doSomething
```

## Implementation

Uses `eval` from pharo-interop MCP server.

## Notes

- Test smalltalk-interop connection
- Execute partial test code
- Verify intermediate values
- Debug with error handling patterns

## Common Use Cases

### Connection Check
```bash
/st:eval Smalltalk version
/st:eval 1 + 1
```

### Quick Object Testing
```bash
/st:eval MyClass new
/st:eval Person new firstName: 'John'; lastName: 'Doe'; fullName
```

### Partial Test Execution
```bash
/st:eval | result |
result := Array new: 2.
[ | obj |
  obj := MyClass new name: 'Test'.
  result at: 1 put: obj getName.
] on: Error do: [:ex | result at: 2 put: ex description].
^ result
```

### Debugging Patterns

**Basic Error Capture:**
```smalltalk
| result |
result := Array new: 2.
[ | ret |
  ret := objA doAAA.
  result at: 1 put: ret printString.
] on: Error do: [:ex | result at: 2 put: (ex description)].
^ result
```

**Check Intermediate Values:**
```smalltalk
| intermediate result |
intermediate := objA computeStep1.
result := intermediate processStep2.
^ result printString
```

**Debug Collections:**
```smalltalk
| items filtered mapped |
items := self getItems.
filtered := items select: [:each | each isValid].
mapped := filtered collect: [:each | each name].
^ { 
    'items size' -> items size. 
    'filtered size' -> filtered size. 
    'mapped' -> mapped 
  } asDictionary printString
```

## Examples

```bash
# Simple expression
/st:eval 2 + 2

# Check Pharo version
/st:eval Smalltalk version

# Object creation and method call
/st:eval Person new firstName: 'Alice'; yourself

# With error handling
/st:eval | result |
result := Array new: 2.
[ result at: 1 put: (10 / 0) ]
on: Error do: [:ex | result at: 2 put: ex description].
^ result

# Collection inspection
/st:eval #(1 2 3 4 5) select: [:n | n even]

# Dictionary operations
/st:eval | dict |
dict := Dictionary new.
dict at: 'name' put: 'Test'.
dict at: 'value' put: 42.
^ dict printString
```

## MCP Tool Call

```
mcp__smalltalk-interop__eval: 'Smalltalk code here'
```

## Tips

- Always use `printString` when returning objects to get readable output
- Use error handling pattern (`on: Error do:`) for debugging
- Multi-line code is supported
- Useful for verifying behavior before writing full tests
- See `smalltalk-debugger` skill for more debugging patterns
