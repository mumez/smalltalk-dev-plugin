# Typical Development Session Examples

Real-world examples of Smalltalk development workflows using AI editors and Pharo.

## Example 1: Creating a New Class

### Scenario
User wants to create a simple Person class with name and age attributes.

### Session Flow

**User Request:**
```
"Create a Person class with name and age accessors"
```

**AI Response:**

1. **Creates** `src/MyPackage/Person.st`:
```smalltalk
Class {
    #name : #Person,
    #superclass : #Object,
    #instVars : [
        'name',
        'age'
    ],
    #category : #'MyPackage'
}

{ #category : #accessing }
Person >> name [
    ^ name
]

{ #category : #accessing }
Person >> name: aString [
    name := aString
]

{ #category : #accessing }
Person >> age [
    ^ age
]

{ #category : #accessing }
Person >> age: anInteger [
    age := anInteger
]
```

2. **Suggests import:**
```
mcp__smalltalk-interop__import_package: 'MyPackage' path: '/home/user/project/src'
```

3. **Suggests test:**
```
mcp__smalltalk-interop__run_class_test: 'PersonTest'
```

### Key Points
- Single `.st` file contains class definition and all methods
- AI automatically suggests import after file creation
- Test suggestion helps maintain quality

---

## Example 2: Adding Methods to Existing Class

### Scenario
User wants to add a `fullName` method to the Person class.

### Session Flow

**User Request:**
```
"Add a fullName method to Person that combines first and last name"
```

**AI Response:**

1. **Reads** existing `src/MyPackage/Person.st`

2. **Adds instance variables and methods:**
```smalltalk
Class {
    #name : #Person,
    #superclass : #Object,
    #instVars : [
        'firstName',
        'lastName',
        'age'
    ],
    #category : #'MyPackage'
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

{ #category : #operations }
Person >> fullName [
    ^ firstName, ' ', lastName
]
```

3. **Suggests:**
```
mcp__smalltalk-interop__import_package: 'MyPackage' path: '/home/user/project/src'
mcp__smalltalk-interop__run_class_test: 'PersonTest'
```

### Key Points
- Read existing file before editing
- Update instance variables when needed
- Re-import after changes
- Always suggest testing

---

## Example 3: Working with Multiple Packages

### Scenario
Adding JSON functionality to the RediStick project with multiple packages.

### Session Flow

**User Request:**
```
"Add JSON serialization support to RediStick package"
```

**AI Response:**

1. **Creates** `src/RediStick-Json/RsJsonSerializer.st`:
```smalltalk
Class {
    #name : #RsJsonSerializer,
    #superclass : #Object,
    #category : #'RediStick-Json'
}

{ #category : #serialization }
RsJsonSerializer >> toJson: anObject [
    ^ Json render: anObject
]

{ #category : #serialization }
RsJsonSerializer >> fromJson: aString [
    ^ Json readFrom: aString readStream
]
```

2. **Creates** `src/RediStick-Json-Tests/RsJsonTest.st`:
```smalltalk
TestCase subclass: #RsJsonTest
    instanceVariableNames: 'serializer'
    classVariableNames: ''
    package: 'RediStick-Json-Tests'

{ #category : #running }
RsJsonTest >> setUp [
    super setUp.
    serializer := RsJsonSerializer new
]

{ #category : #tests }
RsJsonTest >> testBasicSerialization [
    | dict json |
    dict := Dictionary new
        at: 'name' put: 'John';
        at: 'age' put: 30;
        yourself.
    json := serializer toJson: dict.
    self assert: json notNil
]
```

3. **Checks baseline** for correct import order in `BaselineOfRediStick`:
```smalltalk
baseline: spec
    <baseline>
    spec for: #common do: [
        spec
            package: 'RediStick-Core';
            package: 'RediStick-Json' with: [
                spec requires: #('RediStick-Core')
            ];
            package: 'RediStick-Json-Tests' with: [
                spec requires: #('RediStick-Core' 'RediStick-Json')
            ]
    ]
```

4. **Imports in dependency order:**
```
mcp__smalltalk-interop__import_package: 'RediStick-Core' path: '/home/user/git/RediStick/src'
mcp__smalltalk-interop__import_package: 'RediStick-Json' path: '/home/user/git/RediStick/src'
mcp__smalltalk-interop__import_package: 'RediStick-Json-Tests' path: '/home/user/git/RediStick/src'
```

5. **Runs tests:**
```
mcp__smalltalk-interop__run_class_test: 'RsJsonTest'
```

### Key Points
- Check Baseline for dependency order
- Import packages in correct sequence
- Create both implementation and test packages
- Import test package after main package

---

## Example 4: Test-Driven Development Session

### Scenario
Implementing a Calculator class using TDD approach.

### Session Flow

**Step 1: Write Test First**

User: "Let's create a Calculator with TDD. Start with addition test."

AI creates `src/MyPackage-Tests/CalculatorTest.st`:
```smalltalk
TestCase subclass: #CalculatorTest
    instanceVariableNames: 'calculator'
    classVariableNames: ''
    package: 'MyPackage-Tests'

{ #category : #running }
CalculatorTest >> setUp [
    super setUp.
    calculator := Calculator new
]

{ #category : #tests }
CalculatorTest >> testAddition [
    | result |
    result := calculator add: 5 to: 3.
    self assert: result equals: 8
]
```

**Step 2: Import Test (Should Fail)**
```
import_package: 'MyPackage-Tests' path: '/home/user/project/src'
run_class_test: 'CalculatorTest'
```

Result: Test fails because Calculator class doesn't exist yet.

**Step 3: Implement Minimal Code**

AI creates `src/MyPackage/Calculator.st`:
```smalltalk
Class {
    #name : #Calculator,
    #superclass : #Object,
    #category : #'MyPackage'
}

{ #category : #operations }
Calculator >> add: a to: b [
    ^ a + b
]
```

**Step 4: Import and Test (Should Pass)**
```
import_package: 'MyPackage' path: '/home/user/project/src'
run_class_test: 'CalculatorTest'
```

Result: Test passes.

**Step 5: Add More Tests**

User: "Add subtraction functionality."

AI adds to `CalculatorTest.st`:
```smalltalk
{ #category : #tests }
CalculatorTest >> testSubtraction [
    | result |
    result := calculator subtract: 10 from: 3.
    self assert: result equals: 7
]
```

**Step 6: Implement Feature**

AI adds to `Calculator.st`:
```smalltalk
{ #category : #operations }
Calculator >> subtract: b from: a [
    ^ a - b
]
```

**Step 7: Import and Test All**
```
import_package: 'MyPackage' path: '/home/user/project/src'
import_package: 'MyPackage-Tests' path: '/home/user/project/src'
run_class_test: 'CalculatorTest'
```

### Key Points
- Write test first (red phase)
- Implement minimal code to pass (green phase)
- Refactor if needed
- Re-import and re-test at each step

---

## Example 5: Debugging a Failed Test

### Scenario
A test fails with unexpected results, need to debug.

### Session Flow

**Initial Situation:**
```
User: "PersonTest's testFullName is failing"
```

Test output:
```
testFullName FAILED
Expected: 'John Doe'
Actual: 'John nil'
```

**AI Debugging Steps:**

**Step 1: Review Test Code**
```smalltalk
PersonTest >> testFullName [
    | person |
    person := Person new.
    person firstName: 'John'.
    person lastName: 'Doe'.
    self assert: person fullName equals: 'John Doe'
]
```

**Step 2: Use `/st:eval` to Debug**
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

**Step 3: Identify Issue**
AI discovers that `lastName:` is not actually setting the value (missing return value handling).

**Step 4: Check Implementation**
```smalltalk
Person >> lastName: aString [
    lastName := aString.
    "Missing ^ self for method chaining"
]
```

**Step 5: Fix the Code**
```smalltalk
Person >> lastName: aString [
    lastName := aString.
    ^ self
]
```

**Step 6: Re-import and Re-test**
```
import_package: 'MyPackage' path: '/home/user/project/src'
run_class_test: 'PersonTest'
```

Result: Test passes now.

### Key Points
- Use `/st:eval` to execute code snippets
- Test incrementally to isolate issues
- Fix in Tonel file, not Pharo
- Always re-import after fixes

---

## Example 6: Refactoring Existing Code

### Scenario
Refactoring a class to improve design.

### Session Flow

**User Request:**
```
"Refactor Person class to use a separate Address object for address fields"
```

**Current Code** (Person.st):
```smalltalk
Class {
    #name : #Person,
    #superclass : #Object,
    #instVars : [
        'name',
        'street',
        'city',
        'zipCode'
    ],
    #category : #'MyPackage'
}
```

**AI Refactoring Steps:**

**Step 1: Create Address Class**

Create `src/MyPackage/Address.st`:
```smalltalk
Class {
    #name : #Address,
    #superclass : #Object,
    #instVars : [
        'street',
        'city',
        'zipCode'
    ],
    #category : #'MyPackage'
}

{ #category : #accessing }
Address >> street [ ^ street ]
Address >> street: aString [ street := aString ]

{ #category : #accessing }
Address >> city [ ^ city ]
Address >> city: aString [ city := aString ]

{ #category : #accessing }
Address >> zipCode [ ^ zipCode ]
Address >> zipCode: aString [ zipCode := aString ]

{ #category : #printing }
Address >> printOn: aStream [
    aStream
        nextPutAll: street; nextPutAll: ', ';
        nextPutAll: city; nextPutAll: ' ';
        nextPutAll: zipCode
]
```

**Step 2: Update Person Class**

Update `src/MyPackage/Person.st`:
```smalltalk
Class {
    #name : #Person,
    #superclass : #Object,
    #instVars : [
        'name',
        'address'
    ],
    #category : #'MyPackage'
}

{ #category : #accessing }
Person >> address [
    ^ address ifNil: [ address := Address new ]
]

{ #category : #accessing }
Person >> address: anAddress [
    address := anAddress
]
```

**Step 3: Update Tests**

Update `src/MyPackage-Tests/PersonTest.st`:
```smalltalk
PersonTest >> testAddress [
    | person address |
    person := Person new.
    address := Address new
        street: '123 Main St';
        city: 'Springfield';
        zipCode: '12345';
        yourself.
    person address: address.

    self assert: person address street equals: '123 Main St'.
    self assert: person address city equals: 'Springfield'
]
```

**Step 4: Import in Correct Order**
```
import_package: 'MyPackage' path: '/home/user/project/src'
import_package: 'MyPackage-Tests' path: '/home/user/project/src'
run_package_test: 'MyPackage-Tests'
```

### Key Points
- Refactor incrementally
- Create new classes before modifying existing ones
- Update tests to match new design
- Test thoroughly after refactoring

---

## Example 7: Handling Import Errors

### Scenario
Import fails with dependency error.

### Session Flow

**Error Message:**
```
Import failed: RsJsonSerializer references undefined class 'Json'
```

**AI Debugging Steps:**

**Step 1: Identify Missing Dependency**
AI recognizes that `Json` class is from external package (NeoJSON).

**Step 2: Check Baseline**
Look at `BaselineOfRediStick`:
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

**Step 3: Install External Dependency**
```smalltalk
User must run in Pharo:
Metacello new
    baseline: 'NeoJSON';
    repository: 'github://svenvc/NeoJSON/repository';
    load.
```

**Step 4: Retry Import**
After NeoJSON is loaded:
```
import_package: 'RediStick-Json' path: '/home/user/git/RediStick/src'
```

### Key Points
- Check Baseline for external dependencies
- Install external packages in Pharo first
- Import order matters for dependencies
- Read error messages carefully

---

## Common Patterns Summary

### Pattern 1: Standard Edit-Import-Test Cycle
```
1. Edit .st file
2. import_package
3. run_class_test or run_package_test
4. Repeat if needed
```

### Pattern 2: Multi-Package Development
```
1. Check Baseline for dependencies
2. Import packages in dependency order
3. Import test packages last
4. Run comprehensive package tests
```

### Pattern 3: TDD Workflow
```
1. Write test (red)
2. Import test package
3. Implement feature (green)
4. Import main package
5. Run tests (should pass)
6. Refactor if needed
```

### Pattern 4: Debugging Workflow
```
1. Test fails
2. Use /st:eval to investigate
3. Identify root cause
4. Fix in Tonel file
5. Re-import
6. Re-test
```

### Pattern 5: Refactoring Workflow
```
1. Plan refactoring
2. Create new structures first
3. Update existing code
4. Update tests
5. Import all affected packages
6. Run comprehensive tests
```
