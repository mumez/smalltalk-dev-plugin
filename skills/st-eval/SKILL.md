---
name: st-eval
description: Smalltalk code evaluator for Pharo via MCP. Use when: executing Smalltalk expressions, verifying object state or intermediate values, debugging code incrementally, checking Pharo connection, or running quick experiments. Key patterns: always use printString on return values, wrap risky code in on:do: for error capture, use fork for blocking operations that would hang the MCP call.
allowed-tools:
  - mcp__smalltalk-interop__eval
---

# Execute Smalltalk Code

Evaluate arbitrary Smalltalk expressions in the running Pharo image via `mcp__smalltalk-interop__eval`.

## Essential Rules

**Always use `printString`** — MCP returns serialized output. Raw objects cause errors or unreadable results:

```smalltalk
✅ MyClass new doSomething printString
✅ collection printString
❌ MyClass new doSomething
```

**Wrap risky code in `on:do:`** — Uncaught errors in eval crash the MCP call with no useful message:

```smalltalk
| result |
result := Array new: 2.
[ result at: 1 put: (riskyOperation) printString ]
  on: Error do: [:ex | result at: 2 put: ex description].
^ result
```

**Use `fork` for blocking operations** — Code that opens a dialog or blocks the image will cause the MCP call to hang indefinitely. Detach it:

```smalltalk
[ <blocking expression> ] fork.
^ 'started'
```

## Common Patterns

### Connection check
```smalltalk
Smalltalk version
```

### Inspect intermediate values
```smalltalk
| step1 step2 |
step1 := objA computeStep1.
step2 := step1 processStep2.
^ { 'step1' -> step1 printString. 'step2' -> step2 printString } asDictionary printString
```

### Inspect object state
```smalltalk
{
  'class' -> obj class name.
  'value' -> obj printString.
  'size'  -> obj size printString
} asDictionary printString
```

### Safe collection access
```smalltalk
collection ifEmpty: ['empty'] ifNotEmpty: [:col | col first printString]
```

### Error-capturing eval
```smalltalk
| result |
result := Array new: 2.
[
  | obj |
  obj := MyClass new name: 'Test'.
  result at: 1 put: obj process printString.
] on: Error do: [:ex |
  result at: 2 put: ex description
].
^ result
```

## Troubleshooting

| Symptom | Cause | Fix |
|---------|-------|-----|
| MCP call hangs / no response | Blocking operation (dialog, modal, infinite loop) | Wrap in `fork` |
| `Error: cannot serialize` | Returning raw object | Add `printString` |
| Debugger window opened in Pharo | Uncaught error triggered Pharo debugger | Close debugger in Pharo, then re-eval with `on:do:` |
| `MessageNotUnderstood` | Method doesn't exist or typo | Check with `mcp__smalltalk-interop__search_implementors` |

## Related Skills

- `smalltalk-dev:smalltalk-debugger` — Systematic debugging workflow using eval
- `smalltalk-dev:st-init` — Verify Pharo connection if eval fails unexpectedly
