---
name: smalltalk-debugger
description: This skill should be used when debugging Smalltalk code, including:
  - Test failures ("test failed", "test error")
  - Runtime errors and exceptions
  - Analyzing stack traces
  - Investigating unexpected behavior
  - Finding root causes of bugs
  Use this after tests fail or when the user asks "why is this failing?" or "debug this".
model_selection:
  enabled: false
triggers:
  - "test failed"
  - "test failure"
  - "error occurred"
  - "debug"
  - "verify behavior"
  - "run partially"
  - "inspect"
  - "investigate"
  - "check result"
tool_permissions:
  allowed_tools:
    - eval
    - get_class_source
    - get_method_source
    - search_implementors
    - search_references
    - run_class_test
---

# Smalltalk Debugger

This skill provides debugging techniques and best practices for Smalltalk development.

## Error Handling Patterns

### Basic Pattern: Capture Both Results and Errors

```smalltalk
| result |
result := Array new: 2.
[ | ret |
  ret := objA doAAA.
  result at: 1 put: ret printString.
] on: Error do: [:ex | result at: 2 put: (ex description)].
^ result
```

**Interpreting Return Values:**
- `result at: 1`: Normal result (as printString)
- `result at: 2`: Error message when error occurs

### Pattern 2: Check Intermediate Values

```smalltalk
| intermediate result |
intermediate := objA computeStep1.
"Check intermediate contents with printString"
result := intermediate processStep2.
^ result printString
```

### Pattern 3: Debug Collection Operations

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

## Step-by-Step Debugging Procedures

### 1. Identify Error Location

From test error messages, confirm:
- Error type (MessageNotUnderstood, KeyNotFound, etc.)
- Stack trace
- Failed assertion

### 2. Verify Code with Partial Execution

Use the `eval` tool to execute relevant parts of the test code:

```smalltalk
"Step 1: Verify object creation"
| obj |
obj := MyClass new.
^ obj printString

"Step 2: Verify method call"
| obj result |
obj := MyClass new name: 'Test'.
result := obj getName.
^ result printString

"Step 3: Execute with error handling"
| result |
result := Array new: 2.
[ | obj |
  obj := MyClass new name: 'Test'.
  result at: 1 put: obj getName.
] on: Error do: [:ex | result at: 2 put: ex description].
^ result
```

### 3. Check Intermediate Values

Confirm intermediate results at each step:

```smalltalk
| step1 step2 step3 |
step1 := self getData.
step2 := step1 select: [:each | each isValid].
step3 := step2 collect: [:each | each process].
^ {
    'step1 count' -> step1 size.
    'step2 count' -> step2 size.
    'step3 result' -> step3
  } asDictionary printString
```

### 4. Fix and Re-test

1. Fix the Tonel file
2. Re-import with `import_package`
3. Re-test with `run_class_test`

## Object Inspection Techniques

### Basic Inspection

```smalltalk
"Check object type"
obj class printString

"Check instance variables"
obj instVarNames

"Object state"
obj printString

"More detailed information"
obj inspect  "When executing in Pharo"
```

### Collection Inspection

```smalltalk
"Output collection contents"
collection do: [:each | Transcript show: each printString; cr].

"Check size and elements"
{
  'size' -> collection size.
  'isEmpty' -> collection isEmpty.
  'first' -> (collection ifNotEmpty: [collection first] ifEmpty: [nil]).
  'last' -> (collection ifNotEmpty: [collection last] ifEmpty: [nil])
} asDictionary printString
```

### Dictionary Inspection

```smalltalk
"Check keys and values"
{
  'keys' -> dict keys.
  'values' -> dict values.
  'associations' -> dict associations
} asDictionary printString
```

## Common Error Patterns and Solutions

### 1. MessageNotUnderstood

**Causes:**
- Method name typo
- Receiver class is different than expected
- Method is undefined

**Debugging Method:**

```smalltalk
"Check receiver class"
receiver class printString

"Search for implemented methods"
mcp__smalltalk-interop__search_implementors: 'methodName'

"Check method source"
mcp__smalltalk-interop__get_method_source: class: 'ClassName' method: 'methodName'
```

### 2. KeyNotFound

**Cause:**
- Accessing non-existent key in Dictionary

**Debugging Method:**

```smalltalk
"Check key existence"
| result |
result := dict at: #key ifAbsent: ['Key not found'].
^ result printString

"Check all keys"
dict keys printString
```

### 3. SubscriptOutOfBounds

**Cause:**
- Collection index out of range

**Debugging Method:**

```smalltalk
"Check collection size"
| collection index |
collection := self getCollection.
index := 10.
^ {
    'collection size' -> collection size.
    'requested index' -> index.
    'is valid' -> (index between: 1 and: collection size)
  } asDictionary printString

"Safe access"
collection at: index ifAbsent: [nil]
```

### 4. ZeroDivide

**Cause:**
- Division by zero

**Debugging Method:**

```smalltalk
"Check before division"
| numerator denominator result |
numerator := 10.
denominator := 0.
result := denominator = 0
    ifTrue: ['Division by zero']
    ifFalse: [numerator / denominator].
^ result printString
```

## Practical Debugging Examples

### Example 1: Investigating Test Failures

```
User: "PersonTest's testFullName is failing"

Claude (smalltalk-debugger):
1. Check test code
2. Execute partially with eval:

| result |
result := Array new: 2.
[ | person |
  person := Person new.
  person firstName: 'John'.
  person lastName: 'Doe'.
  result at: 1 put: person fullName.
] on: Error do: [:ex | result at: 2 put: ex description].
^ result

3. Confirm error type (e.g., MessageNotUnderstood: #firstName:)
4. Check Person.st and fix method name
5. Re-import and re-test
```

### Example 2: Debugging Complex Logic

```
User: "JSON conversion is not working correctly"

Claude (smalltalk-debugger):
1. Execute step by step:

"Step 1: Check input data"
| input |
input := '{"name":"John","age":30}'.
^ input printString

"Step 2: Parse processing"
| input parsed |
input := '{"name":"John","age":30}'.
parsed := Json readFrom: input readStream.
^ parsed printString

"Step 3: Conversion processing"
| input parsed result |
result := Array new: 2.
[ 
  input := '{"name":"John","age":30}'.
  parsed := Json readFrom: input readStream.
  result at: 1 put: (parsed at: 'name').
] on: Error do: [:ex | result at: 2 put: ex description].
^ result

2. Identify error location
3. Fix and re-test
```

## Tool Usage

### eval Tool

The most important debugging tool. Can execute arbitrary Smalltalk code.

```
mcp__smalltalk-interop__eval: 'Smalltalk version'
mcp__smalltalk-interop__eval: '1 + 1'
mcp__smalltalk-interop__eval: 'MyClass new doSomething'
```

### Source Code Verification

```
mcp__smalltalk-interop__get_class_source: 'ClassName'
mcp__smalltalk-interop__get_method_source: class: 'ClassName' method: 'methodName'
```

### Method Search

```
mcp__smalltalk-interop__search_implementors: 'methodName'
mcp__smalltalk-interop__search_references: 'methodName'
```

## Debugging Best Practices

1. **Divide into small pieces**: Break down problems into small steps and verify
2. **Check intermediate values**: Confirm results of each step with printString
3. **Error handling**: Always capture errors with on:do:
4. **Step-by-step execution**: Execute line by line with eval
5. **Source confirmation**: Check implementation before making assumptions

## Important Notes

### Using printString

When returning objects to AI via JSON, always convert to string with `printString`:

```smalltalk
obj printString
collection printString
dict printString
```

### Using Transcript

Use Transcript when debugging on the Pharo side:

```smalltalk
Transcript show: 'Debug message'; cr.
Transcript show: obj printString; cr.
```

### Reading Error Messages

- `MessageNotUnderstood`: Method not found
- `KeyNotFound`: Dictionary key does not exist
- `SubscriptOutOfBounds`: Collection index out of range
- `Error`: General error (check message for details)

## Summary

Basic debugging flow:

1. Confirm test failure/error
2. Execute partially with eval
3. Identify error location
4. Check intermediate values
5. Fix Tonel file
6. Re-import and re-test

By repeating this cycle, you can debug efficiently.
