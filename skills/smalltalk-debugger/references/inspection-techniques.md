# Object Inspection Techniques

Comprehensive techniques for inspecting and understanding Smalltalk objects during debugging.

## Basic Object Inspection

### Check Object Type

```smalltalk
" Get object's class "
obj class printString
" => Person "

" Get class hierarchy "
obj class allSuperclasses printString
" => an OrderedCollection(Object ProtoObject) "

" Check if instance of class "
obj isKindOf: Person
" => true "

" Check exact class match "
obj class = Person
" => true "
```

### Inspect Instance Variables

```smalltalk
" Get instance variable names "
obj instVarNames
" => #('firstName' 'lastName' 'age') "

" Get instance variable value "
obj instVarNamed: 'firstName'
" => 'John' "

" Get all instance variables and values "
obj instVarNames collect: [:varName |
    varName -> (obj instVarNamed: varName)
]
" => an OrderedCollection('firstName'->'John' 'lastName'->'Doe' 'age'->30) "
```

### Object State Inspection

```smalltalk
" Simple string representation "
obj printString
" => 'a Person(John Doe, age: 30)' "

" Detailed inspection (in Pharo) "
obj inspect

" Check object identity "
obj identityHash
" => 123456789 "

" Check if object is nil "
obj isNil
" => false "

" Check if object responds to selector "
obj respondsTo: #fullName
" => true "
```

---

## Collection Inspection

### Basic Collection Info

```smalltalk
" Collection size "
collection size
" => 5 "

" Check if empty "
collection isEmpty
" => false "

" Check if includes element "
collection includes: 'John'
" => true "

" Count matching elements "
collection count: [:each | each age > 30]
" => 2 "
```

### Iterate and Display

```smalltalk
" Print all elements "
collection do: [:each |
    Transcript show: each printString; cr
].

" Collect descriptions "
collection collect: [:each | each printString]
" => an OrderedCollection('John' 'Jane' 'Bob') "

" Find specific element "
collection detect: [:each | each age > 25]
" => Person(John Doe, age: 30) "

" Find with default "
collection
    detect: [:each | each age > 100]
    ifNone: [nil]
" => nil "
```

### Collection Analysis

```smalltalk
" Get statistical information "
| collection stats |
collection := #(1 2 3 4 5 6 7 8 9 10).
stats := {
    'size' -> collection size.
    'sum' -> collection sum.
    'average' -> collection average.
    'min' -> collection min.
    'max' -> collection max.
    'isEmpty' -> collection isEmpty.
    'first' -> collection first.
    'last' -> collection last
} asDictionary.
stats printString
```

### Advanced Collection Inspection

```smalltalk
" Group by criteria "
people groupedBy: [:each | each age // 10 * 10]
" => Dictionary(20->#(Person1 Person2) 30->#(Person3)) "

" Check all/any match "
collection allSatisfy: [:each | each age > 0]
" => true "

collection anySatisfy: [:each | each age > 100]
" => false "

" Get indices "
collection findFirst: [:each | each age > 30]
" => 2 "

collection findAll: [:each | each age > 30]
" => #(2 4 5) "
```

---

## Dictionary Inspection

### Basic Dictionary Operations

```smalltalk
" Get all keys "
dict keys
" => a Set(#name #age #email) "

" Get all values "
dict values
" => an OrderedCollection('John' 30 'john@example.com') "

" Get key-value pairs "
dict associations
" => {#name->'John'. #age->30. #email->'john@example.com'} "

" Check key existence "
dict includesKey: #name
" => true "

" Count entries "
dict size
" => 3 "
```

### Safe Dictionary Access

```smalltalk
" Safe access with default "
dict at: #age ifAbsent: [0]
" => 30 (or 0 if not found) "

" Access with default block "
dict at: #missing ifAbsentPut: ['default']
" => 'default' (and adds to dict) "

" Multiple key check "
#(#name #age #email) collect: [:key |
    key -> (dict includesKey: key)
]
" => {#name->true. #age->true. #email->true} "
```

### Dictionary Analysis

```smalltalk
" Get complete dictionary state "
{
    'size' -> dict size.
    'isEmpty' -> dict isEmpty.
    'keys' -> dict keys asArray sorted.
    'hasName' -> (dict includesKey: #name).
    'hasAge' -> (dict includesKey: #age)
} asDictionary printString

" Find entries by value "
dict associations select: [:assoc | assoc value > 25]
" => {#age->30} "

" Transform dictionary "
dict associations collect: [:assoc |
    assoc key -> assoc value printString
]
" => New dictionary with string values "
```

---

## String Inspection

### Basic String Info

```smalltalk
" String length "
string size
" => 10 "

" Check if empty "
string isEmpty
" => false "

" Check contents "
string includesSubstring: 'test'
" => true "

" Case checks "
string asLowercase
string asUppercase
string capitalized
```

### String Analysis

```smalltalk
" Character breakdown "
{
    'length' -> string size.
    'isEmpty' -> string isEmpty.
    'first' -> (string ifEmpty: [''] ifNotEmpty: [:s | s first]).
    'last' -> (string ifEmpty: [''] ifNotEmpty: [:s | s last]).
    'contains space' -> (string includes: Character space).
    'word count' -> (string substrings size)
} asDictionary printString

" Character frequency "
string asSet collect: [:char |
    char -> (string occurrencesOf: char)
]
```

---

## Block/Closure Inspection

### Block Analysis

```smalltalk
" Number of arguments "
block numArgs
" => 2 "

" Block source (if available) "
block sourceString
" => '[:a :b | a + b]' "

" Test block "
| testBlock result |
testBlock := [:x | x * 2].
result := testBlock value: 5.
result printString
" => '10' "
```

---

## Class Inspection

### Class Structure

```smalltalk
" Get class name "
Person name
" => #Person "

" Get superclass "
Person superclass
" => Object "

" Get all superclasses "
Person allSuperclasses
" => an OrderedCollection(Object ProtoObject) "

" Get subclasses "
Person subclasses
" => an OrderedCollection(Employee Student) "

" Get all subclasses (recursive) "
Person allSubclasses
```

### Class Methods and Variables

```smalltalk
" Instance variable names "
Person instVarNames
" => #('firstName' 'lastName' 'age') "

" Class variable names "
Person classVarNames
" => #('DefaultAge') "

" All instance methods "
Person methodDict keys
" => a Set(#firstName #firstName: #lastName #lastName: ...) "

" Methods in category "
Person methodsInCategory: 'accessing'
" => OrderedCollection(#firstName #firstName: #lastName ...) "

" Check method exists "
Person canUnderstand: #fullName
" => true "
```

### Class Inspection

```smalltalk
" Complete class information "
{
    'name' -> Person name.
    'superclass' -> Person superclass name.
    'instance variables' -> Person instVarNames.
    'class variables' -> Person classVarNames.
    'method count' -> Person methodDict size.
    'subclass count' -> Person subclasses size.
    'package' -> Person package name
} asDictionary printString
```

---

## Method Inspection

### Method Information

```smalltalk
" Get method source "
(Person >> #fullName) sourceCode
" => 'fullName\n\t^ firstName, '' '', lastName' "

" Get method selector "
(Person >> #fullName) selector
" => #fullName "

" Get method category "
(Person >> #fullName) protocol
" => #operations "

" Number of arguments "
(Person >> #setName:lastName:) numArgs
" => 2 "
```

---

## Stream Inspection

### Stream State

```smalltalk
" Check stream position "
stream position
" => 10 "

" Check if at end "
stream atEnd
" => false "

" Get remaining content "
stream upToEnd
" => 'remaining text' "

" Peek next character "
stream peek
" => $a "

" Get stream contents "
stream contents
" => 'full stream contents' "
```

---

## Advanced Inspection Patterns

### Conditional Inspection

```smalltalk
" Inspect based on condition "
obj isNil
    ifTrue: ['Object is nil']
    ifFalse: [
        {
            'class' -> obj class name.
            'value' -> obj printString
        } asDictionary
    ]
```

### Nested Object Inspection

```smalltalk
" Inspect object hierarchy "
person := Person new firstName: 'John'; lastName: 'Doe'; yourself.
address := Address new street: 'Main St'; city: 'Springfield'; yourself.
person address: address.

" Inspect nested structure "
{
    'person' -> person printString.
    'firstName' -> person firstName.
    'address' -> person address printString.
    'street' -> person address street.
    'city' -> person address city
} asDictionary printString
```

### Error-Safe Inspection

```smalltalk
" Inspect with error handling "
| result |
result := Dictionary new.
[
    result at: 'class' put: obj class name.
    result at: 'value' put: obj printString.
    result at: 'size' put: obj size.
] on: Error do: [:ex |
    result at: 'error' put: ex description
].
result printString
```

---

## Debugging with Transcript

### Basic Transcript Usage

```smalltalk
" Write to Transcript "
Transcript show: 'Debug message'; cr.

" Write object state "
Transcript show: 'Person: ', person printString; cr.

" Write separator "
Transcript show: '-------------------'; cr.
```

### Structured Transcript Output

```smalltalk
" Formatted debug output "
Transcript
    show: '=== Debugging Person ==='; cr;
    show: 'Name: ', person fullName; cr;
    show: 'Age: ', person age printString; cr;
    show: 'Address: ', person address printString; cr;
    show: '======================='; cr.
```

### Conditional Transcript

```smalltalk
" Debug output with condition "
debugMode ifTrue: [
    Transcript
        show: 'Processing: ', item printString; cr;
        show: 'State: ', item state; cr.
]
```

---

## Inspection Utilities

### Create Inspection Dictionary

```smalltalk
" Utility method for consistent inspection "
inspectObject: obj
    ^ {
        'class' -> obj class name.
        'value' -> obj printString.
        'isNil' -> obj isNil.
        'respondsToFullName' -> (obj respondsTo: #fullName)
    } asDictionary
```

### Deep Inspection

```smalltalk
" Recursively inspect object tree "
deepInspect: obj depth: maxDepth
    | result |
    result := Dictionary new.
    result at: 'class' put: obj class name.
    result at: 'value' put: obj printString.

    (maxDepth > 0 and: [obj class instVarNames notEmpty]) ifTrue: [
        obj class instVarNames do: [:varName |
            | varValue |
            varValue := obj instVarNamed: varName.
            result at: varName put: (self deepInspect: varValue depth: maxDepth - 1)
        ]
    ].

    ^ result
```

---

## Inspection Best Practices

### 1. Always Use printString for JSON

When returning objects via MCP (JSON), convert to string:

```smalltalk
✅ obj printString
✅ collection printString
✅ dict printString

❌ obj  " Don't return raw objects "
```

### 2. Safe Collection Access

```smalltalk
✅ collection ifEmpty: [nil] ifNotEmpty: [:col | col first]
❌ collection first  " Crashes on empty "
```

### 3. Structured Inspection

Create dictionaries for complex inspections:

```smalltalk
{
    'property1' -> value1.
    'property2' -> value2.
    'nested' -> nestedObject printString
} asDictionary printString
```

### 4. Error Handling

Wrap inspection in error handlers:

```smalltalk
[
    inspectionResult := obj inspect
] on: Error do: [:ex |
    inspectionResult := 'Error inspecting: ', ex description
]
```

---

## Quick Inspection Reference

| Goal | Code |
|------|------|
| Object class | `obj class printString` |
| Instance variables | `obj instVarNames` |
| Collection size | `collection size` |
| Dictionary keys | `dict keys` |
| Check method exists | `obj respondsTo: #methodName` |
| Safe collection first | `col ifEmpty: [nil] ifNotEmpty: [:c \| c first]` |
| Print all items | `col do: [:each \| Transcript show: each printString; cr]` |

---

## Summary

Key principles for effective object inspection:

1. **Use printString** for all object output
2. **Check before accessing** (ifEmpty:, at:ifAbsent:)
3. **Create structured output** using dictionaries
4. **Handle errors gracefully** with on:do:
5. **Use Transcript** for runtime debugging
6. **Inspect incrementally** - one property at a time
7. **Build inspection dictionaries** for complex objects
