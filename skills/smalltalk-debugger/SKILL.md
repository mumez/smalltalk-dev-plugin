---
name: smalltalk-debugger
description: Systematic debugging guide for Smalltalk (Pharo/Squeak) development. Provides expertise in error diagnosis (MessageNotUnderstood, KeyNotFound, SubscriptOutOfBounds, AssertionFailure), incremental code execution with eval tool, intermediate value inspection, error handling patterns (`on:do:` blocks), stack trace analysis, UI debugger window detection (read_screen for hung operations), Transcript logging (crShow:, ##DEBUG## prefix, format strings, call-site tracing with thisContext, headless output via NonInteractiveTranscript on Pharo), and debug-fix-reimport workflow. Use when encountering Smalltalk test failures, Smalltalk exceptions, unexpected behavior, timeout or non-responsive operations, need to verify intermediate values, execute code incrementally for diagnosis, troubleshoot Tonel import errors, add debug logging to trace variable values or call sites, or redirect Transcript output in headless/Docker images.
---

# Smalltalk Debugger

Systematic debugging techniques for Smalltalk (Pharo/Squeak) development using AI editors.

## Core Debugging Workflow

When tests fail or errors occur, follow this systematic approach:

### 1. Identify Error Location

From error message, confirm:
- **Error type** (MessageNotUnderstood, KeyNotFound, etc.)
- **Stack trace** - where error occurred
- **Expected vs Actual** - what went wrong

### 2. Verify with Partial Execution

Use `/st-eval` tool to execute relevant code incrementally.

**Basic error capture pattern:**
```smalltalk
| result |
result := Array new: 2.
[ | ret |
  ret := objA doSomething.
  result at: 1 put: ret printString.
] on: Error do: [:ex | result at: 2 put: ex description].
^ result
```

**Interpreting results:**
- `result at: 1` - Normal result (success case)
- `result at: 2` - Error description (failure case)

### 3. Check Intermediate Values

Inspect state at each step:

```smalltalk
| step1 step2 |
step1 := self getData.
step2 := step1 select: [:each | each isValid].
{
    'step1 count' -> step1 size.
    'step2 count' -> step2 size.
    'step2 result' -> step2 printString
} asDictionary printString
```

### 4. Fix and Re-test

1. **Fix in Tonel file** (never in the Smalltalk image)
2. **Re-import** with `import_package`
3. **Re-test** with `run_class_test`

## When Operations Stop Responding

When an MCP call times out, follow this escalation sequence:

### Step 1: Health Check

Run a quick eval to verify the Smalltalk image is still responsive:

```
mcp__smalltalk-interop__eval: 'Smalltalk version'
```

If this succeeds, the Smalltalk image is alive — a **debugger window may have opened** (see below).

### Step 2: Try `read_screen`

If eval also times out, check the UI state:

```
mcp__smalltalk-interop__read_screen: target_type='world'
```

If `read_screen` responds, inspect the output for debugger windows (see "Detecting Hidden Debuggers" below).

### Step 3: Process Hang — Ask User to Restart

If `read_screen` itself times out, **the Smalltalk image has hung at the process level**. MCP tools cannot recover from this state.

Ask the user to:
1. Kill the Pharo/Squeak process (or Docker container) and restart it
2. After restart, re-import all packages before continuing
3. Re-run tests to confirm state before making further claims

### Detecting Hidden Debuggers

Use the `read_screen` tool to capture the Smalltalk image's UI state:

```
mcp__smalltalk-interop__read_screen: target_type='world'
```

This captures all morphs including debugger windows. Look for:
- Window titles containing "Debugger", "Error", or "Exception"
- UI hierarchy showing debugger-related components
- Error messages or stack traces in window content

### Resolution Steps

1. **Notify the user**: Inform them that a debugger window appears to be open in the Smalltalk image
2. **Request manual intervention**: Ask the user to:
   - Check their Smalltalk image for open debugger windows
   - Close any debugger windows
   - Review the error shown in the debugger to understand the root cause
3. **Address root cause**: Once the debugger is closed, investigate and fix the underlying error using standard debugging techniques
4. **Retry operation**: Re-run the failed MCP operation

**Note**: The Smalltalk debugger cannot be controlled remotely through MCP tools. User intervention in the Smalltalk image is required.

For complete UI debugging guidance, see [UI Debugging Reference](references/ui-debugging.md).

## Common Error Types Quick Reference

### MessageNotUnderstood
**Cause**: Method doesn't exist or typo in method name
**Debug**: Check spelling, search implementors
```
mcp__smalltalk-interop__search_implementors: 'methodName'
```

### KeyNotFound
**Cause**: Accessing non-existent Dictionary key
**Debug**: List keys, use at:ifAbsent:
```smalltalk
dict keys printString
dict at: #key ifAbsent: ['default']
```

### SubscriptOutOfBounds
**Cause**: Collection index out of range
**Debug**: Check size, use at:ifAbsent:
```smalltalk
collection size printString
collection at: index ifAbsent: [nil]
```

### ZeroDivide
**Cause**: Division by zero
**Debug**: Check denominator before dividing
```smalltalk
count = 0 ifTrue: [0] ifFalse: [sum / count]
```

### AssertionFailure (in tests)
**Cause**: Test expectation doesn't match actual
**Debug**: Execute test code with `/st-eval`, check if package imported

For complete error patterns and solutions, see [Error Patterns Reference](references/error-patterns.md).

## Object Inspection Quick Guide

### Basic Inspection
```smalltalk
" Object class "
obj class printString

" Instance variables "
obj instVarNames

" Check method exists "
obj respondsTo: #methodName
```

### Collection Inspection
```smalltalk
" Size and elements "
collection size
collection printString

" Safe first/last "
collection ifEmpty: [nil] ifNotEmpty: [:col | col first]
```

### Dictionary Inspection
```smalltalk
" Keys and values "
dict keys
dict values

" Safe access "
dict at: #key ifAbsent: ['default']
```

For comprehensive inspection techniques, see [Inspection Techniques Reference](references/inspection-techniques.md).

## Debugging Best Practices

### 1. Divide into Small Steps
Break problems into incremental steps and verify each with `/st-eval`:

```smalltalk
obj := MyClass new.
obj printString  " Step 1: verify creation "

result := obj doSomething.
result printString  " Step 2: verify method call "
```

### 2. Check Intermediate Values
Never assume - verify at each step:

```smalltalk
intermediate := obj step1.
" Check here before proceeding "
result := intermediate step2.
```

### 3. Always Use printString
When returning objects via JSON/MCP:

```smalltalk
✅ obj printString
✅ collection printString

❌ obj  " Don't return raw objects "
```

### 4. Use Error Handling
Always capture errors with `on:do:`:

```smalltalk
[
    risky operation
] on: Error do: [:ex |
    ex description
]
```

### 5. Fix in Tonel, Not the Smalltalk Image
- ✅ Edit `.st` file → Import → Test
- ❌ Edit in the Smalltalk image → Export → Commit

## Transcript Logging

Use `Transcript crShow:` with `##DEBUG##`-prefixed format strings to log values. Read output via `read_screen target_type='transcript'`. For call-order tracing use `DateAndTime current`; for call-site tracing use `thisContext shortStack` or `printStackOfSize:`. Headless Pharo images: `NonInteractiveTranscript file install` (writes to `PharoTranscript.log`; Pharo-specific — check Squeak's headless Transcript equivalent separately).

See [Logging Techniques Reference](references/logging-techniques.md) for full examples.

## Debugging Tools

### Primary Tool: `/st-eval`

```
mcp__smalltalk-interop__eval: 'Smalltalk version'
mcp__smalltalk-interop__eval: 'MyClass new doSomething printString'
```

### Code Inspection Tools

```
mcp__smalltalk-interop__get_class_source: 'ClassName'
mcp__smalltalk-interop__get_method_source: class: 'ClassName' method: 'methodName'
mcp__smalltalk-interop__search_implementors: 'methodName'
mcp__smalltalk-interop__search_references: 'methodName'
```

## Practical Example

### Test Failure: AssertionFailure

**Error**: `Expected 'John Doe' but got 'John nil'`

1. Execute test code with `/st-eval` to reproduce
2. Inspect intermediate values (was lastName set?)
3. Check setter implementation: was `^ self` missing?
4. Fix in Tonel file, re-import, re-test

For complete debugging scenarios, see [Debug Scenarios Examples](examples/debug-scenarios.md).

## Troubleshooting Checklist

When debugging, systematically check:

- [ ] Read complete error message
- [ ] Use `/st-eval` to test incrementally
- [ ] Inspect all intermediate values
- [ ] Check method implementation
- [ ] Verify package was imported
- [ ] Edit Tonel file (not the Smalltalk image)
- [ ] Re-import after fixing
- [ ] Re-run tests

## Complete Documentation

This skill provides focused debugging guidance. For comprehensive information:

- **[Error Patterns Reference](references/error-patterns.md)** - All error types with solutions
- **[Inspection Techniques](references/inspection-techniques.md)** - Complete object inspection guide
- **[Debug Scenarios](examples/debug-scenarios.md)** - Real-world debugging examples
- **[Logging Techniques](references/logging-techniques.md)** - Transcript logging patterns and tips

## Summary

**Core debugging cycle:**

```
Error occurs → Identify error type → /st-eval incrementally
    → Inspect intermediate values → Identify root cause
    → Fix in Tonel → Re-import → Re-test → Success or repeat
```

**Remember**: Systematic approach, incremental testing, fix in Tonel, always re-import.
