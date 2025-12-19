# UI Debugging Reference

Comprehensive guide for diagnosing hidden debugger windows in Pharo Smalltalk using the `read_screen` MCP tool.

## Purpose

When developing with Pharo Smalltalk through MCP tools, operations can suddenly stop responding or timeout. This often indicates that a **debugger window has opened in the Pharo image** due to an error or exception. Since the debugger UI is not visible from the AI editor, this creates an invisible blocking state where:

- MCP tool calls hang indefinitely
- Operations timeout without clear error messages
- The Pharo image appears unresponsive to remote commands
- No feedback reaches the development environment

The `read_screen` tool provides visibility into the Pharo UI state, allowing you to detect and diagnose these hidden debugger windows.

## The read_screen Tool

### Overview

The `read_screen` tool captures screenshots and extracts UI structure from the running Pharo image. It provides detailed information about all visible UI components, including windows, morphs, and their hierarchical relationships.

### Parameters

**target_type** (string, default: 'world')
- `'world'`: Captures all morphs in the Pharo World (display environment)
  - Best for detecting debugger windows
  - Shows all top-level windows and UI elements
  - Includes class names, bounds, colors, text content

- `'spec'`: Examines Spec presenter windows
  - Provides window titles, positions, extent
  - Shows presenter hierarchy (up to 3 levels deep)
  - Displays decoration states (menu, toolbar, statusbar)

- `'roassal'`: Inspects Roassal visualization canvases
  - Canvas bounds, zoom levels, background colors
  - Shape and edge details with positions
  - Less useful for debugger detection

**capture_screenshot** (boolean, default: true)
- `true`: Includes PNG screenshot data in response
- `false`: Returns only UI structure data (faster execution)
- For debugger detection, screenshots are optional but can be helpful

### Return Structure

The tool returns a structured representation of the UI hierarchy:

**For 'world' target:**
```
{
  "morphs": [
    {
      "class": "MorphClassName",
      "bounds": {"x": 100, "y": 200, "width": 400, "height": 300},
      "visible": true,
      "background_color": "Color white",
      "owner": "ParentMorphName",
      "submorph_count": 5,
      "text_content": "Window title or text"
    },
    ...
  ]
}
```

**For 'spec' target:**
```
{
  "windows": [
    {
      "title": "Debugger - MessageNotUnderstood",
      "extent": {"width": 800, "height": 600},
      "position": {"x": 100, "y": 100},
      "state": "normal",
      "has_menu": true,
      "has_toolbar": true,
      "presenter_hierarchy": [...]
    }
  ]
}
```

## Debugger Detection Workflow

### Step 1: Recognize the Symptoms

Operations stop responding when you observe:
- MCP tool call hangs for >10 seconds
- Timeout errors from Smalltalk interop server
- No response to `eval`, `import_package`, or `run_test` commands
- Previous operations were working normally

### Step 2: Capture UI State

Execute the read_screen tool with 'world' target:

```
mcp__smalltalk-interop__read_screen: target_type='world'
```

Or for faster execution without screenshot:

```
mcp__smalltalk-interop__read_screen: target_type='world', capture_screenshot=false
```

Alternatively, use 'spec' target to focus on windows:

```
mcp__smalltalk-interop__read_screen: target_type='spec'
```

### Step 3: Analyze the Output

Look for debugger indicators in the returned data:

**Window Title Patterns:**
- "Debugger"
- "Debug session"
- "MessageNotUnderstood"
- "Error"
- "Exception"
- "KeyNotFound"
- "SubscriptOutOfBounds"
- "ZeroDivide"
- Any error class names

**UI Hierarchy Indicators:**
- Morph classes containing "Debug" or "Inspector"
- Text content showing stack traces
- Error messages in window content
- Multiple panes typical of debugger layout

**Example Debugger Detection:**
```
{
  "windows": [
    {
      "title": "Debugger - MessageNotUnderstood: RGMethodDefinition>>gtDisplayOn:",
      "extent": {"width": 900, "height": 700},
      "state": "normal",
      "has_toolbar": true
    }
  ]
}
```

### Step 4: Notify the User

When a debugger window is detected, immediately inform the user:

**Message Template:**
```
I've detected that a debugger window has opened in your Pharo image.
This is blocking further MCP operations.

Debugger window found:
- Title: [Window title from read_screen output]
- Error: [Error type if visible]

Please:
1. Switch to your Pharo image
2. Locate the open debugger window
3. Review the error message and stack trace shown in the debugger
4. Close the debugger window
5. Let me know once closed so we can investigate the root cause

The debugger cannot be controlled remotely through MCP tools -
manual intervention in Pharo is required.
```

### Step 5: Address Root Cause

After the user closes the debugger:

1. **Gather error details**: Ask the user to share:
   - The error message shown in the debugger
   - The stack trace (top 3-5 methods)
   - What operation triggered the error

2. **Investigate using standard debugging**:
   - Use `get_method_source` to examine failing methods
   - Use `eval` to test hypotheses incrementally
   - Check for common errors (typos, missing methods, wrong arguments)

3. **Fix in Tonel files**:
   - Edit the `.st` files with corrections
   - Re-import the package
   - Re-run tests to verify the fix

4. **Retry the original operation**:
   - Re-run the MCP command that caused the debugger
   - Confirm it now completes successfully

## Common Debugger Patterns

### Pattern 1: MessageNotUnderstood During Import

**Symptom**: `import_package` hangs
**Cause**: Class initialization or method compilation triggers missing method
**Debugger shows**: "MessageNotUnderstood: SomeClass>>unknownMethod"

**Resolution**:
1. User closes debugger
2. Check method spelling in Tonel file
3. Verify method exists in superclass or trait
4. Fix typo or add missing method
5. Re-import

### Pattern 2: Test Execution Error

**Symptom**: `run_class_test` or `run_package_test` hangs
**Cause**: Test code triggers unhandled exception
**Debugger shows**: "KeyNotFound", "SubscriptOutOfBounds", etc.

**Resolution**:
1. User closes debugger and shares error details
2. Use `eval` to run test code step-by-step
3. Identify failing assertion or operation
4. Fix test or implementation
5. Re-test

### Pattern 3: Eval Code Error

**Symptom**: `eval` command hangs
**Cause**: Executed code raises unhandled exception
**Debugger shows**: Error with stack trace of eval'd code

**Resolution**:
1. User closes debugger
2. Review the code sent to eval
3. Add error handling with `on:do:` pattern
4. Test smaller code increments
5. Fix the problematic code

## When to Use read_screen

### Primary Scenarios

**Use read_screen when:**
- Any MCP tool call hangs for >10 seconds
- Operations timeout without clear error
- Pharo becomes unresponsive to commands
- Previously working operations suddenly stop
- User reports "nothing is happening"

### Secondary Scenarios

**Also consider read_screen for:**
- Verifying Pharo UI state after complex operations
- Checking if modal dialogs are blocking
- Confirming window layouts during UI development
- Debugging Spec or Roassal visualization issues

### When NOT to Use

**Don't use read_screen for:**
- Normal debugging with clear error messages (use `eval` instead)
- Inspecting object state (use `eval` with `printString`)
- Code navigation (use `get_class_source`, `get_method_source`)
- Finding implementors or references (use dedicated search tools)

## Best Practices

### 1. Quick Detection

At the first sign of hanging:
```
mcp__smalltalk-interop__read_screen: target_type='world', capture_screenshot=false
```

Fast execution without screenshot overhead provides immediate visibility.

### 2. Clear User Communication

Always explain:
- What you detected (debugger window)
- Why operations are blocked
- What the user needs to do (close debugger manually)
- That remote control is not possible

### 3. Systematic Investigation

After debugger is closed:
1. Gather error details from user
2. Use standard debugging tools (`eval`, source inspection)
3. Fix root cause, don't just retry
4. Verify fix with tests

### 4. Prevention

Help users avoid debuggers by:
- Using error handling in `eval` code (`on:do:`)
- Testing code incrementally
- Validating inputs before operations
- Running tests regularly to catch issues early

## Summary

The `read_screen` tool is essential for diagnosing invisible blocking states in Pharo development through MCP. When operations hang:

1. Use `read_screen` to capture UI state
2. Look for debugger window indicators in output
3. Notify user to manually close the debugger
4. Investigate and fix the root cause
5. Retry the operation

Remember: **The Pharo debugger requires manual intervention**. You cannot close it remotely through MCP tools. Always inform the user and request they handle it in the Pharo image.

For general debugging techniques, see the main [Smalltalk Debugger skill](../SKILL.md).
