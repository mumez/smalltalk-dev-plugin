# Common Error Patterns and Solutions

Comprehensive guide to recognizing and resolving common Smalltalk error patterns.

## Error Type 1: MessageNotUnderstood

### Description
Occurs when sending a message to an object that doesn't implement the method.

### Common Causes
1. **Method name typo** - Misspelled method name
2. **Wrong receiver** - Object is different class than expected
3. **Method undefined** - Method hasn't been implemented yet
4. **Case sensitivity** - Smalltalk is case-sensitive

### Error Message Example
```
MessageNotUnderstood: Person>>firstName:
```

### Debugging Steps

**Step 1: Verify receiver class**
```smalltalk
receiver class printString
```

**Step 2: Search for method implementations**
```
mcp__smalltalk-interop__search_implementors: 'methodName'
```

**Step 3: Check method exists in class**
```
mcp__smalltalk-interop__get_method_source: class: 'ClassName' method: 'methodName'
```

**Step 4: Inspect class protocol**
```smalltalk
Person methodDict keys
```

### Common Fixes

**Typo in method name:**
```smalltalk
" Wrong "
person fistName: 'John'.

" Correct "
person firstName: 'John'.
```

**Wrong method signature:**
```smalltalk
" Wrong - single argument method "
person name: 'John Doe'.

" Correct - two separate calls "
person firstName: 'John'.
person lastName: 'Doe'.
```

**Missing cascade:**
```smalltalk
" Wrong - returns last value, not person "
person firstName: 'John'.
person lastName: 'Doe'.
person fullName  " Fails if previous line returns String "

" Correct - use cascade "
person
    firstName: 'John';
    lastName: 'Doe';
    fullName
```

### Prevention
- Use IDE autocompletion when available
- Check method signatures in documentation
- Search for implementors before using unfamiliar methods
- Review class protocol before calling methods

---

## Error Type 2: KeyNotFound

### Description
Occurs when accessing a non-existent key in a Dictionary or similar collection.

### Common Causes
1. **Key doesn't exist** - Trying to access key that was never added
2. **Wrong key type** - Using String instead of Symbol or vice versa
3. **Case mismatch** - Dictionary keys are case-sensitive
4. **Key removed** - Key was previously deleted

### Error Message Example
```
KeyNotFound: key #age not found in Dictionary
```

### Debugging Steps

**Step 1: Check if key exists**
```smalltalk
dict includesKey: #age
```

**Step 2: List all keys**
```smalltalk
dict keys printString
```

**Step 3: Use safe access**
```smalltalk
dict at: #key ifAbsent: ['Key not found']
```

**Step 4: Check key type**
```smalltalk
" Symbols vs Strings are different keys "
dict at: #name ifAbsent: [dict at: 'name' ifAbsent: ['Not found']]
```

### Common Fixes

**Use ifAbsent: for safe access:**
```smalltalk
" Wrong - throws error if key missing "
value := dict at: #age.

" Correct - provides default "
value := dict at: #age ifAbsent: [0].
```

**Use ifAbsentPut: for lazy initialization:**
```smalltalk
" Create key if doesn't exist "
dict at: #counter ifAbsentPut: [0].
dict at: #counter put: (dict at: #counter) + 1.
```

**Check before accessing:**
```smalltalk
" Defensive access "
(dict includesKey: #age)
    ifTrue: [value := dict at: #age]
    ifFalse: [value := 0]
```

### Prevention
- Always use `at:ifAbsent:` when key existence uncertain
- Be consistent with Symbol (#key) vs String ('key') usage
- Initialize dictionaries with expected keys
- Use `at:ifAbsentPut:` for lazy initialization

---

## Error Type 3: SubscriptOutOfBounds

### Description
Occurs when accessing a collection with an invalid index.

### Common Causes
1. **Index too large** - Beyond collection size
2. **Index too small** - Less than 1 (Smalltalk uses 1-based indexing)
3. **Empty collection** - Accessing first/last of empty collection
4. **Off-by-one error** - Common fence-post mistake

### Error Message Example
```
SubscriptOutOfBounds: 10 is not between 1 and 5
```

### Debugging Steps

**Step 1: Check collection size**
```smalltalk
collection size printString
```

**Step 2: Verify index range**
```smalltalk
| index |
index := 10.
{
    'collection size' -> collection size.
    'requested index' -> index.
    'is valid' -> (index between: 1 and: collection size)
} asDictionary printString
```

**Step 3: Check for empty collection**
```smalltalk
collection isEmpty ifTrue: ['Collection is empty']
```

### Common Fixes

**Safe indexed access:**
```smalltalk
" Wrong - crashes if index out of bounds "
item := collection at: index.

" Correct - provides default "
item := collection at: index ifAbsent: [nil].
```

**Check bounds before access:**
```smalltalk
" Defensive access "
item := (index between: 1 and: collection size)
    ifTrue: [collection at: index]
    ifFalse: [nil]
```

**Use first/last safely:**
```smalltalk
" Wrong - crashes on empty collection "
firstItem := collection first.

" Correct - handles empty case "
firstItem := collection ifEmpty: [nil] ifNotEmpty: [:col | col first].
```

**Iterator instead of index:**
```smalltalk
" Better - avoids index issues "
collection do: [:each |
    " process each item "
]
```

### Prevention
- Use iterators (do:, collect:, select:) instead of indexed access
- Always check collection size before indexed access
- Remember Smalltalk uses 1-based indexing, not 0-based
- Use ifEmpty:ifNotEmpty: for safe first/last access

---

## Error Type 4: ZeroDivide

### Description
Occurs when dividing by zero.

### Common Causes
1. **Literal division by zero** - Hardcoded 0 divisor
2. **Variable is zero** - Computed value happens to be zero
3. **Uninitialized variable** - nil or 0 default
4. **Edge case** - Specific data causes zero divisor

### Error Message Example
```
ZeroDivide: Division by zero
```

### Debugging Steps

**Step 1: Check denominator value**
```smalltalk
denominator printString
```

**Step 2: Trace computation**
```smalltalk
| numerator denominator |
numerator := 100.
denominator := items select: [:each | each isValid].
{
    'numerator' -> numerator.
    'denominator' -> denominator size.
    'will crash' -> (denominator size = 0)
} asDictionary printString
```

### Common Fixes

**Check before dividing:**
```smalltalk
" Wrong "
average := sum / count.

" Correct "
average := count = 0
    ifTrue: [0]
    ifFalse: [sum / count]
```

**Use guard clause:**
```smalltalk
computeAverage: items
    items ifEmpty: [^ 0].
    ^ (items sum) / (items size)
```

**Use ifEmpty: for collections:**
```smalltalk
average := items
    ifEmpty: [0]
    ifNotEmpty: [:col | col sum / col size]
```

### Prevention
- Always check for zero before division
- Use guard clauses for edge cases
- Initialize variables properly
- Consider what happens with empty collections

---

## Error Type 5: Error (General)

### Description
Generic error class, often with custom description.

### Common Causes
1. **Custom error signaled** - `self error: 'message'`
2. **Assertion failure** - Failed precondition
3. **Invalid state** - Object in unexpected state
4. **Business logic violation** - Domain rule violated

### Error Message Example
```
Error: Invalid age: must be between 0 and 150
```

### Debugging Steps

**Step 1: Read error description carefully**
```smalltalk
" Error object contains useful message "
```

**Step 2: Check where error was signaled**
```smalltalk
" Look at stack trace in error "
```

**Step 3: Verify preconditions**
```smalltalk
" Check what conditions should be true "
```

### Common Fixes

**Handle specific error:**
```smalltalk
[
    person age: value
] on: Error do: [:ex |
    (ex messageText includesSubstring: 'Invalid age')
        ifTrue: [self showAgeError]
        ifFalse: [ex pass]
]
```

**Validate before operation:**
```smalltalk
age: anInteger
    (anInteger between: 0 and: 150)
        ifFalse: [self error: 'Invalid age: must be between 0 and 150'].
    age := anInteger
```

---

## Error Type 6: AssertionFailure (in Tests)

### Description
Test assertion failed - expected vs actual values don't match.

### Common Causes
1. **Wrong expected value** - Test expectation is incorrect
2. **Implementation bug** - Code doesn't work as intended
3. **Stale test** - Test needs updating after refactoring
4. **Missing import** - Testing old code version

### Error Message Example
```
AssertionFailure: Expected 'John Doe' but got 'John nil'
```

### Debugging Steps

**Step 1: Compare expected vs actual**
```smalltalk
" From error message "
Expected: 'John Doe'
Actual: 'John nil'
```

**Step 2: Execute test code manually**
```smalltalk
| person |
person := Person new.
person firstName: 'John'.
person lastName: 'Doe'.
person fullName printString
```

**Step 3: Check if package was imported**
```
mcp__smalltalk-interop__get_method_source: class: 'Person' method: 'fullName'
```

### Common Fixes

**Re-import package:**
```
import_package: 'MyPackage' path: '/absolute/path/src'
```

**Fix implementation:**
```smalltalk
" Bug - lastName not being set "
Person >> lastName: aString [
    lastName := aString.
    " Missing: ^ self "
]

" Fixed "
Person >> lastName: aString [
    lastName := aString.
    ^ self
]
```

**Update test expectation:**
```smalltalk
" If implementation is correct but test is wrong "
self assert: person fullName equals: 'John Doe'
```

---

## Error Type 7: NotFound

### Description
General "not found" error for various resources.

### Common Causes
1. **Class not loaded** - Class not in image
2. **Package not imported** - Missing dependency
3. **File not found** - Invalid file path
4. **Variable undefined** - Typo in variable name

### Debugging Steps

**Check class exists:**
```smalltalk
Smalltalk globals includesKey: #ClassName
```

**Search for class:**
```
mcp__smalltalk-interop__search_classes_like: 'ClassName'
```

**Check loaded packages:**
```
mcp__smalltalk-interop__list_packages
```

### Common Fixes

**Import missing package:**
```
import_package: 'MissingPackage' path: '/absolute/path/src'
```

**Check class name spelling:**
```
" Verify exact class name including case "
```

**Load external dependency:**
```smalltalk
" In Pharo "
Metacello new
    baseline: 'NeoJSON';
    repository: 'github://svenvc/NeoJSON/repository';
    load.
```

---

## Error Handling Best Practices

### Use Specific Error Classes

```smalltalk
" Prefer specific error "
balance < 0 ifTrue: [InsufficientFundsError signal].

" Instead of generic "
balance < 0 ifTrue: [self error: 'Insufficient funds'].
```

### Provide Helpful Error Messages

```smalltalk
" Good - actionable message "
age < 0 ifTrue: [
    self error: 'Age must be non-negative. Got: ', age printString
].

" Bad - vague message "
age < 0 ifTrue: [self error: 'Bad age'].
```

### Catch Specific Errors

```smalltalk
" Good - catch specific error "
[
    dict at: #key
] on: KeyNotFound do: [:ex |
    ^ defaultValue
]

" Bad - catches everything "
[
    dict at: #key
] on: Error do: [:ex |
    ^ defaultValue
]
```

### Clean Up on Error

```smalltalk
" Use ensure: for cleanup "
file := FileStream fileNamed: 'data.txt'.
[
    file contents
] ensure: [
    file close
]
```

---

## Quick Error Reference

| Error | Common Cause | Quick Fix |
|-------|-------------|-----------|
| MessageNotUnderstood | Typo, wrong class | Check method name |
| KeyNotFound | Missing key | Use at:ifAbsent: |
| SubscriptOutOfBounds | Invalid index | Check collection size |
| ZeroDivide | Division by zero | Check denominator ≠ 0 |
| AssertionFailure | Test mismatch | Re-import or fix code |
| NotFound | Missing class/package | Import package |

---

## Debugging Checklist

When encountering an error:

1. ✅ Read error message completely
2. ✅ Check error type
3. ✅ Look at stack trace
4. ✅ Use `/st:eval` to test hypotheses
5. ✅ Verify package is imported
6. ✅ Check for typos
7. ✅ Fix in Tonel file, not Pharo
8. ✅ Re-import and re-test
