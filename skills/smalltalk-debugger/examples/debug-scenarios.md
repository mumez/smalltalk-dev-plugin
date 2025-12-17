# Practical Debugging Scenarios

Real-world debugging examples demonstrating systematic problem-solving in Smalltalk development.

## Scenario 1: Investigating Test Failures

### Problem
```
User: "PersonTest's testFullName is failing"
```

**Test output:**
```
testFullName FAILED
Expected: 'John Doe'
Actual: 'John nil'
```

### Debugging Process

**Step 1: Review Test Code**

First, examine what the test is actually doing:

```smalltalk
PersonTest >> testFullName [
    | person |
    person := Person new.
    person firstName: 'John'.
    person lastName: 'Doe'.
    self assert: person fullName equals: 'John Doe'
]
```

**Step 2: Execute Partial Code with `/st:eval`**

Test object creation and method calls incrementally:

```smalltalk
| result |
result := Array new: 2.
[ | person |
  person := Person new.
  person firstName: 'John'.
  person lastName: 'Doe'.
  result at: 1 put: person fullName printString.
] on: Error do: [:ex | result at: 2 put: ex description].
^ result
```

**Step 3: Analyze Result**

Output shows: `#('John nil' nil)`

This tells us:
- No error occurred (result at: 2 is nil)
- fullName returns 'John nil' instead of 'John Doe'
- The lastName value is nil, not 'Doe'

**Step 4: Inspect Intermediate State**

Check if lastName was actually set:

```smalltalk
| person |
person := Person new.
person firstName: 'John'.
person lastName: 'Doe'.
{
    'firstName' -> person firstName.
    'lastName' -> person lastName.
    'fullName' -> person fullName
} asDictionary printString
```

Output: `{'firstName'->'John'. 'lastName'->nil. 'fullName'->'John nil'}`

**Step 5: Identify Root Cause**

The `lastName:` setter is not actually setting the value. Check implementation:

```
mcp__smalltalk-interop__get_method_source: class: 'Person' method: 'lastName:'
```

Source shows:
```smalltalk
Person >> lastName: aString [
    lastName := aString
    " Missing ^ self for method chaining "
]
```

**Problem identified**: The setter doesn't return `self`, breaking the cascade pattern.

**Step 6: Fix the Code**

Edit `src/MyPackage/Person.st`:

```smalltalk
{ #category : #accessing }
Person >> lastName: aString [
    lastName := aString.
    ^ self
]
```

**Step 7: Re-import and Re-test**

```
import_package: 'MyPackage' path: '/home/user/project/src'
run_class_test: 'PersonTest'
```

**Result**: ✅ Test passes

### Key Takeaways
- Use `/st:eval` to execute code incrementally
- Test intermediate values, don't assume
- Check implementation when behavior is unexpected
- Always re-import after fixing Tonel files
- Missing `^ self` breaks method chaining

---

## Scenario 2: Debugging Complex Logic

### Problem
```
User: "JSON conversion is not working correctly"
```

**Symptoms**: JSON parsing returns incorrect or nil values.

### Debugging Process

**Step 1: Test Input Data**

Verify the input is valid:

```smalltalk
| input |
input := '{"name":"John","age":30}'.
input printString
" => '{"name":"John","age":30}' "
```

Input looks correct.

**Step 2: Test Parsing**

Try to parse the JSON:

```smalltalk
| input parsed |
result := Array new: 2.
[
    input := '{"name":"John","age":30}'.
    parsed := Json readFrom: input readStream.
    result at: 1 put: parsed printString.
] on: Error do: [:ex | result at: 2 put: ex description].
^ result
```

Output: `#(nil 'MessageNotUnderstood: ByteString>>readStream')`

**Step 3: Identify Issue**

Error shows `readStream` message not understood by ByteString. The input is already a String, doesn't need `readStream`.

**Step 4: Test with Correct Code**

```smalltalk
| input parsed result |
result := Array new: 2.
[
    input := '{"name":"John","age":30}'.
    parsed := NeoJSONReader fromString: input.
    result at: 1 put: parsed printString.
] on: Error do: [:ex | result at: 2 put: ex description].
^ result
```

Output: `#('a Dictionary(''name''->''John'' ''age''->30)' nil)`

Success! JSON parsed correctly.

**Step 5: Test Value Extraction**

```smalltalk
| input parsed name age result |
result := Dictionary new.
[
    input := '{"name":"John","age":30}'.
    parsed := NeoJSONReader fromString: input.
    name := parsed at: 'name'.
    age := parsed at: 'age'.
    result at: 'name' put: name.
    result at: 'age' put: age.
] on: Error do: [:ex |
    result at: 'error' put: ex description
].
result printString
```

Output: `{'name'->'John'. 'age'->30}`

**Step 6: Fix Implementation**

Edit `src/RediStick-Json/RsJsonSerializer.st`:

```smalltalk
{ #category : #serialization }
RsJsonSerializer >> fromJson: aString [
    " Fixed: removed unnecessary readStream "
    ^ NeoJSONReader fromString: aString
]
```

**Step 7: Re-import and Test**

```
import_package: 'RediStick-Json' path: '/home/user/git/RediStick/src'
run_class_test: 'RsJsonTest'
```

**Result**: ✅ All tests pass

### Key Takeaways
- Break complex operations into steps
- Test each step individually
- Error messages reveal the exact problem
- String already is a stream, doesn't need `readStream`
- Use NeoJSON's convenience methods

---

## Scenario 3: KeyNotFound Error

### Problem
```
User: "Getting KeyNotFound error when accessing person's age"
```

**Error message:**
```
KeyNotFound: key #age not found in Dictionary
```

### Debugging Process

**Step 1: Reproduce Error**

```smalltalk
| dict |
result := Array new: 2.
[
    dict := Dictionary new.
    dict at: #name put: 'John'.
    result at: 1 put: (dict at: #age).
] on: Error do: [:ex | result at: 2 put: ex description].
^ result
```

Output: `#(nil 'KeyNotFound: key #age not found in Dictionary')`

**Step 2: Inspect Dictionary Keys**

```smalltalk
| dict |
dict := Dictionary new.
dict at: #name put: 'John'.
{
    'keys' -> dict keys.
    'size' -> dict size.
    'hasAge' -> (dict includesKey: #age).
    'hasName' -> (dict includesKey: #name)
} asDictionary printString
```

Output: `{'keys'->#(#name). 'size'->1. 'hasAge'->false. 'hasName'->true}`

Key `#age` was never added to the dictionary.

**Step 3: Check Where Age Should Be Set**

Look for where age is supposed to be initialized:

```smalltalk
Person >> asDictionary [
    ^ Dictionary new
        at: #name put: self fullName;
        at: #email put: self email;
        yourself
]
```

Age is missing from the dictionary creation.

**Step 4: Fix Implementation**

Add age to dictionary:

```smalltalk
{ #category : #converting }
Person >> asDictionary [
    ^ Dictionary new
        at: #name put: self fullName;
        at: #age put: self age;
        at: #email put: self email;
        yourself
]
```

**Step 5: Alternative Fix - Safe Access**

Or use safe access pattern:

```smalltalk
{ #category : #accessing }
Person class >> fromDictionary: aDictionary [
    ^ self new
        fullName: (aDictionary at: #name ifAbsent: ['Unknown']);
        age: (aDictionary at: #age ifAbsent: [0]);
        email: (aDictionary at: #email ifAbsent: ['']);
        yourself
]
```

**Step 6: Test Fix**

```
import_package: 'MyPackage' path: '/home/user/project/src'
run_class_test: 'PersonTest'
```

### Key Takeaways
- KeyNotFound means the key literally doesn't exist
- Use `at:ifAbsent:` for safe access
- Check where dictionary is populated
- Initialize all expected keys

---

## Scenario 4: SubscriptOutOfBounds Error

### Problem
```
User: "Crash when trying to get third person from collection"
```

**Error:**
```
SubscriptOutOfBounds: 3 is not between 1 and 2
```

### Debugging Process

**Step 1: Check Collection Size**

```smalltalk
| people |
people := OrderedCollection new.
people add: (Person new firstName: 'John').
people add: (Person new firstName: 'Jane').
{
    'size' -> people size.
    'isEmpty' -> people isEmpty.
    'requestedIndex' -> 3.
    'isValid' -> (3 between: 1 and: people size)
} asDictionary printString
```

Output: `{'size'->2. 'isEmpty'->false. 'requestedIndex'->3. 'isValid'->false}`

Only 2 people in collection, but trying to access index 3.

**Step 2: Find Where Index 3 Is Used**

Search code for hardcoded index:

```smalltalk
processThreePeople
    | first second third |
    first := people at: 1.
    second := people at: 2.
    third := people at: 3.  " Hardcoded assumption of 3 people "
    ...
```

**Step 3: Fix with Safe Access**

Option 1 - Check size first:
```smalltalk
{ #category : #processing }
processThreePeople [
    people size < 3 ifTrue: [
        ^ self error: 'Need at least 3 people'
    ].

    | first second third |
    first := people at: 1.
    second := people at: 2.
    third := people at: 3.
    ...
]
```

Option 2 - Use safe access:
```smalltalk
{ #category : #processing }
processThreePeople [
    | first second third |
    first := people at: 1 ifAbsent: [^ self].
    second := people at: 2 ifAbsent: [^ self].
    third := people at: 3 ifAbsent: [^ self].
    ...
]
```

Option 3 - Use iterators (best):
```smalltalk
{ #category : #processing }
processPeople [
    people do: [:person |
        self processOnePerson: person
    ]
]
```

**Step 4: Test Fix**

```
import_package: 'MyPackage' path: '/home/user/project/src'
run_class_test: 'PersonProcessorTest'
```

### Key Takeaways
- Don't hardcode collection indices
- Use iterators (do:, collect:) instead of indexed access
- Always check collection size before access
- Smalltalk uses 1-based indexing

---

## Scenario 5: Nil Reference Error

### Problem
```
User: "Getting MessageNotUnderstood: nil when calling methods"
```

**Error:**
```
MessageNotUnderstood: UndefinedObject>>firstName
```

### Debugging Process

**Step 1: Identify Where Nil Comes From**

```smalltalk
| person name |
result := Dictionary new.
[
    person := self findPerson: 'John'.
    name := person firstName.
    result at: 'name' put: name.
] on: Error do: [:ex |
    result at: 'error' put: ex description.
    result at: 'person' put: person printString.
].
result printString
```

Output: `{'error'->'MessageNotUnderstood: UndefinedObject>>firstName'. 'person'->'nil'}`

`findPerson:` is returning nil.

**Step 2: Check findPerson Implementation**

```smalltalk
findPerson: aName [
    ^ people detect: [:each | each fullName = aName]
]
```

If no person matches, `detect:` raises an error (not returns nil).

**Step 3: Test detect: Behavior**

```smalltalk
| people result |
result := Array new: 2.
people := OrderedCollection new.
people add: (Person new firstName: 'Jane').
[
    result at: 1 put: (people detect: [:each | each firstName = 'John']).
] on: Error do: [:ex |
    result at: 2 put: ex description
].
result printString
```

Output: `#(nil 'NotFound: Object is not in the collection.')`

**Step 4: Fix with detect:ifNone:**

```smalltalk
{ #category : #accessing }
findPerson: aName [
    ^ people
        detect: [:each | each fullName = aName]
        ifNone: [nil]
]
```

**Step 5: Handle Nil Result Safely**

```smalltalk
{ #category : #operations }
processPersonNamed: aName [
    | person |
    person := self findPerson: aName.
    person ifNil: [^ self handlePersonNotFound: aName].

    ^ person firstName
]
```

**Step 6: Test Fix**

```
import_package: 'MyPackage' path: '/home/user/project/src'
run_class_test: 'PersonManagerTest'
```

### Key Takeaways
- `detect:` raises error when not found, doesn't return nil
- Use `detect:ifNone:` for safe searching
- Always check for nil before using result
- Use `ifNil:` or `ifNotNil:` for nil handling

---

## Scenario 6: Import Fails with Dependency Error

### Problem
```
User: "Import fails with 'undefined class Json' error"
```

**Error message:**
```
Import failed: RsJsonSerializer references undefined class 'Json'
```

### Debugging Process

**Step 1: Identify Missing Class**

The error clearly states `Json` class is undefined. This is likely an external dependency.

**Step 2: Check Baseline for Dependencies**

Look at `BaselineOfRediStick`:

```
mcp__smalltalk-interop__get_class_source: 'BaselineOfRediStick'
```

Source shows:
```smalltalk
baseline: spec
    <baseline>
    spec for: #common do: [
        spec
            package: 'RediStick-Core';
            package: 'RediStick-Json' with: [
                spec requires: #('RediStick-Core')
            ]
    ]
```

No external dependency on JSON library is declared.

**Step 3: Check What JSON Library Should Be Used**

Search for JSON usage in code:

```
mcp__smalltalk-interop__search_references: 'Json'
```

Results show usage of `NeoJSONReader` and `NeoJSONWriter`, not `Json`.

**Step 4: Identify Mismatch**

Code uses `Json` but should use `NeoJSONReader`/`NeoJSONWriter`.

**Step 5: Fix Code**

Edit `src/RediStick-Json/RsJsonSerializer.st`:

```smalltalk
{ #category : #serialization }
RsJsonSerializer >> fromJson: aString [
    " Fixed: use NeoJSON instead of Json "
    ^ NeoJSONReader fromString: aString
]

{ #category : #serialization }
RsJsonSerializer >> toJson: anObject [
    " Fixed: use NeoJSON instead of Json "
    ^ NeoJSONWriter toString: anObject
]
```

**Step 6: Add NeoJSON Dependency to Baseline**

If NeoJSON isn't in base Pharo, add to baseline:

```smalltalk
baseline: spec
    <baseline>
    spec for: #common do: [
        spec
            baseline: 'NeoJSON' with: [
                spec repository: 'github://svenvc/NeoJSON/repository'
            ];
            package: 'RediStick-Core';
            package: 'RediStick-Json' with: [
                spec requires: #('RediStick-Core' 'NeoJSON')
            ]
    ]
```

**Step 7: Load NeoJSON in Pharo** (if not loaded)

User needs to run in Pharo:
```smalltalk
Metacello new
    baseline: 'NeoJSON';
    repository: 'github://svenvc/NeoJSON/repository';
    load.
```

**Step 8: Retry Import**

```
import_package: 'RediStick-Json' path: '/home/user/git/RediStick/src'
```

**Result**: ✅ Import succeeds

### Key Takeaways
- Check Baseline for external dependencies
- Install external packages before importing
- Use correct JSON library (NeoJSON in Pharo)
- Declare dependencies in Baseline

---

## General Debugging Workflow

For any debugging scenario, follow this systematic approach:

### 1. Understand the Error
- Read error message completely
- Identify error type
- Note stack trace

### 2. Reproduce Minimally
- Use `/st:eval` to reproduce issue
- Isolate to smallest failing code
- Verify it's reproducible

### 3. Inspect State
- Check object values with `printString`
- Verify intermediate values
- Use dictionaries for structured inspection

### 4. Form Hypothesis
- Based on error and inspection, guess cause
- Consider: nil values, wrong types, missing data
- Think about what should happen

### 5. Test Hypothesis
- Use `/st:eval` to test assumption
- Check implementation with `get_method_source`
- Search for similar patterns with `search_implementors`

### 6. Fix in Tonel File
- Never fix in Pharo directly
- Edit the `.st` file
- Ensure fix addresses root cause

### 7. Re-import and Re-test
- Import package with absolute path
- Run tests to verify fix
- Check for regression

### 8. Verify Fix
- Test edge cases
- Run full test suite
- Confirm no side effects

---

## Debugging Checklist

When stuck debugging:

- [ ] Have you read the complete error message?
- [ ] Have you used `/st:eval` to test incrementally?
- [ ] Have you inspected all intermediate values?
- [ ] Have you checked the method implementation?
- [ ] Have you verified the package was imported?
- [ ] Are you editing the Tonel file (not Pharo)?
- [ ] Have you re-imported after your fix?
- [ ] Have you re-run the tests?
- [ ] Have you checked for typos in names?
- [ ] Have you considered nil values?
- [ ] Have you checked collection sizes?
- [ ] Have you verified object types?

---

## Summary

Key debugging principles demonstrated:

1. **Systematic approach** - Follow steps, don't jump to conclusions
2. **Incremental testing** - Test one thing at a time
3. **Inspect everything** - Don't assume, verify
4. **Use `/st:eval`** - Most powerful debugging tool
5. **Fix in Tonel** - Never edit Pharo directly
6. **Re-import always** - Changes don't apply automatically
7. **Test thoroughly** - Verify fix and check for regression
