# Tonel File Format Reference

Comprehensive guide to Tonel file format for Pharo Smalltalk development.

## File Structure Overview

Tonel is a text-based format for Smalltalk code that uses a directory structure to represent packages and classes.

### Directory Structure

```
project/
├── src/
│   ├── MyPackage/
│   │   ├── MyClass.st
│   │   ├── AnotherClass.st
│   │   └── package.st
│   └── MyPackage-Tests/
│       ├── MyClassTest.st
│       └── package.st
```

## Package Definition

Every package directory must contain a `package.st` file:

```smalltalk
Package { #name : #MyPackage }
```

This minimal definition identifies the package name.

## Class Definition Format

### Standard Class

```smalltalk
Class {
    #name : #MyClass,
    #superclass : #Object,
    #instVars : [
        'instanceVariable1',
        'instanceVariable2'
    ],
    #classVars : [
        'ClassVariable'
    ],
    #category : #'MyPackage'
}
```

### Class with Multiple Instance Variables

```smalltalk
Class {
    #name : #Person,
    #superclass : #Object,
    #instVars : [
        'firstName',
        'lastName',
        'age',
        'email'
    ],
    #category : #'MyPackage-Model'
}
```

### Subclass Definition

```smalltalk
Class {
    #name : #Employee,
    #superclass : #Person,
    #instVars : [
        'employeeId',
        'department',
        'salary'
    ],
    #category : #'MyPackage-Model'
}
```

## Class Comments

Class comments are crucial documentation in Smalltalk. They explain the purpose, responsibilities, and usage of a class.

### Correct Format: Double Quotes at Top of File

**✅ CORRECT**: Class comments MUST be placed at the very beginning of the `.st` file, enclosed in double quotes `""`, BEFORE the `Class { }` definition:

```smalltalk
"
I represent a person with basic demographic information.

Responsibility:
- Store and manage personal data (name, age)
- Provide formatted output of person information
- Validate age constraints

Collaborators:
- String: For name formatting
- Integer: For age storage

Public API and Key Messages:
- #fullName - Returns formatted full name
- #age: - Sets age with validation
- #initialize - Sets default values

Example:
  person := Person new
      firstName: 'John';
      lastName: 'Doe';
      age: 30;
      yourself.
  person fullName. ""Returns 'John Doe'""

Internal Representation:
- firstName - String containing first name
- lastName - String containing last name
- age - Integer for person's age (>= 0)
"

Class {
    #name : #Person,
    #superclass : #Object,
    #instVars : [
        'firstName',
        'lastName',
        'age'
    ],
    #category : #'MyPackage-Model'
}

{ #category : #accessing }
Person >> firstName [
    ^ firstName
]

...
```

### CRITICAL: Common Mistake to Avoid

**❌ WRONG**: Do NOT place comments inside the `Class { }` definition using `#comment :`:

```smalltalk
Class {
    #name : #Person,
    #comment : 'This represents a person',  ← This will be IGNORED!
    #superclass : #Object,
    #instVars : [
        'firstName',
        'lastName'
    ],
    #category : #'MyPackage-Model'
}
```

**Why this is wrong:**
- The `#comment : 'text'` syntax inside `Class { }` CAN be imported to Pharo
- However, it will be **completely ignored** and won't appear as a class comment in the image
- Pharo only recognizes comments in the `""` format at the top of the file

### Class Comment Best Practices

1. **First-person perspective**: Write as if the class is speaking ("I represent...", "I maintain...")
2. **CRC format**: Cover Class, Responsibility, Collaborators
3. **Include examples**: Show practical usage with real code
4. **Escape quotes**: Use `""` to include quotes within comments
5. **Be concise**: Aim for clarity over length (avoid >200 lines)
6. **Document public API**: List key methods and their purposes
7. **Explain internal state**: Describe important instance variables

### Minimal Class Comment Example

For simpler classes, a shorter comment is acceptable:

```smalltalk
"
I represent a point in 2D space with x and y coordinates.

Example:
  point := Point x: 10 y: 20.
  point distanceTo: (Point x: 0 y: 0). ""Returns 22.36...""
"

Class {
    #name : #Point,
    #superclass : #Object,
    #instVars : [
        'x',
        'y'
    ],
    #category : #'Geometry-Core'
}
```

### Quote Escaping in Comments

Since comments are delimited by `"`, you must escape internal quotes by doubling them:

```smalltalk
"
Example with quotes:
  ""This text appears in quotes""
  person name: ""John"". ""Sets the name""
"
```

**Important:** Single quotes don't need escaping:

```smalltalk
"
Example:
  person name: 'John'.  ← Single quotes work as-is
"
```

## Method Definition Format

### Basic Method Syntax

Methods follow the class definition in the same file:

```smalltalk
{ #category : #accessing }
MyClass >> instanceVariable1 [
    ^ instanceVariable1
]

{ #category : #accessing }
MyClass >> instanceVariable1: anObject [
    instanceVariable1 := anObject
]
```

### Method with Arguments

```smalltalk
{ #category : #operations }
Person >> fullName [
    ^ firstName, ' ', lastName
]

{ #category : #operations }
Person >> setName: first lastName: last [
    firstName := first.
    lastName := last
]
```

### Method with Temporary Variables

```smalltalk
{ #category : #operations }
Calculator >> computeTotal: items [
    | sum count average |
    sum := items inject: 0 into: [:total :each | total + each].
    count := items size.
    average := count > 0
        ifTrue: [sum / count]
        ifFalse: [0].
    ^ average
]
```

### Method with Block Arguments

```smalltalk
{ #category : #enumerating }
Collection >> select: aBlock [
    | result |
    result := OrderedCollection new.
    self do: [:each |
        (aBlock value: each) ifTrue: [
            result add: each
        ]
    ].
    ^ result
]
```

## Test Class Format

Test classes inherit from `TestCase`:

```smalltalk
TestCase subclass: #MyClassTest
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'MyPackage-Tests'
```

### Test Methods

Test methods must start with `test`:

```smalltalk
{ #category : #tests }
MyClassTest >> testCreation [
    | instance |
    instance := MyClass new.
    self assert: instance notNil.
    self assert: instance class equals: MyClass
]

{ #category : #tests }
MyClassTest >> testAccessors [
    | instance |
    instance := MyClass new.
    instance instanceVariable1: 'test value'.
    self assert: instance instanceVariable1 equals: 'test value'
]
```

## Class-Side Methods

Class-side methods (class methods) are defined similarly but marked differently:

```smalltalk
{ #category : #'instance creation' }
MyClass class >> newWithName: aName [
    ^ self new
        name: aName;
        yourself
]
```

## Method Categories

Common category names:

- `#accessing` - Getters and setters
- `#operations` - Business logic methods
- `#'instance creation'` - Factory methods (class-side)
- `#initialization` - Setup methods
- `#tests` - Test methods (in test classes)
- `#private` - Private helper methods
- `#printing` - printOn:, displayString, etc.
- `#comparing` - =, hash, etc.
- `#enumerating` - Collection iteration methods

## Complete Class Example

```smalltalk
Class {
    #name : #Person,
    #superclass : #Object,
    #instVars : [
        'firstName',
        'lastName',
        'age'
    ],
    #category : #'MyPackage-Model'
}

{ #category : #accessing }
Person >> firstName [
    ^ firstName
]

{ #category : #accessing }
Person >> firstName: aString [
    firstName := aString
]

{ #category : #accessing }
Person >> lastName [
    ^ lastName
]

{ #category : #accessing }
Person >> lastName: aString [
    lastName := aString
]

{ #category : #accessing }
Person >> age [
    ^ age
]

{ #category : #accessing }
Person >> age: anInteger [
    age := anInteger
]

{ #category : #operations }
Person >> fullName [
    ^ firstName, ' ', lastName
]

{ #category : #initialization }
Person >> initialize [
    super initialize.
    age := 0
]

{ #category : #printing }
Person >> printOn: aStream [
    super printOn: aStream.
    aStream
        nextPutAll: ' (';
        nextPutAll: self fullName;
        nextPutAll: ', age: ';
        print: age;
        nextPutAll: ')'
]
```

## Common Patterns

### Lazy Initialization

```smalltalk
{ #category : #accessing }
MyClass >> collection [
    ^ collection ifNil: [ collection := OrderedCollection new ]
]
```

### Builder Pattern

```smalltalk
{ #category : #building }
Person >> firstName: first lastName: last age: anInteger [
    firstName := first.
    lastName := last.
    age := anInteger.
    ^ self
]
```

### Cascade Pattern (Method Chaining)

```smalltalk
person := Person new
    firstName: 'John';
    lastName: 'Doe';
    age: 30;
    yourself.
```

## Special Characters and Syntax

- `:=` - Assignment
- `^` - Return statement
- `#` - Symbol literal
- `'...'` - String literal
- `$a` - Character literal
- `[...]` - Block (anonymous function)
- `|...|` - Temporary variable declaration
- `.` - Statement separator
- `;` - Cascade (send to same receiver)

## File Naming Conventions

- Class files: `ClassName.st`
- One class per file
- File name must match class name
- Package file: always `package.st`
