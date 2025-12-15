---
name: smalltalk-developer
description: This skill should be used for Pharo Smalltalk development tasks including:  
  - Editing Tonel files  
  - Importing packages to Pharo image  
  - Running SUnit tests  
  - Debugging Smalltalk code  
  Implements the standard workflow: Edit → Import → Test cycle.
model_selection:
  enabled: false
triggers:
  - "Smalltalk development"
  - "tonel file"
  - "import package"
  - "create class"
  - "add method"
  - "apply changes"
  - "edit tonel"
  - "sync to pharo"
tool_permissions:
  allowed_tools:
    - validate_tonel_smalltalk_from_file
    - validate_tonel_smalltalk
    - import_package
    - run_package_test
    - run_class_test
    - eval
    - export_package
    - get_class_source
    - get_method_source
---

# Smalltalk Developer Workflow

This skill implements the standard workflow for Smalltalk development using AI editors.

## Development Cycle

### 1. Edit Tonel Files

Edit Tonel files directly in the AI editor. Since we cannot directly manipulate the Pharo environment, all changes are made through Tonel files.

**Tonel Format Example:**

```smalltalk
Class {
    #name : #MyClass,
    #superclass : #Object,
    #instVars : [
        'name',
        'age'
    ],
    #category : #'MyPackage'
}

{ #category : #accessing }
MyClass >> name [
    ^ name
]

{ #category : #accessing }
MyClass >> name: aString [
    name := aString
]

{ #category : #accessing }
MyClass >> age [
    ^ age
]

{ #category : #accessing }
MyClass >> age: anInteger [
    age := anInteger
]
```

### 2. Validation (Optional)

Modern AI can generate nearly correct Tonel, so this is usually unnecessary. Use only for complex syntax:

```
mcp__smalltalk-validator__validate_tonel_smalltalk_from_file: '/path/to/file.st'
```

### 3. Import to Pharo (Required)

**Important**: Use absolute paths. Re-import is required for every change.

```
mcp__smalltalk-interop__import_package: 'MyPackage' path: '/home/user/project/src'
mcp__smalltalk-interop__import_package: 'MyPackage-Tests' path: '/home/user/project/src'
```

### 4. Run Tests

After import, always run tests:

```
mcp__smalltalk-interop__run_class_test: 'MyTestClass'
mcp__smalltalk-interop__run_package_test: 'MyPackage-Tests'
```

### 5. If Errors Occur

Return to step 1 to fix, then re-import and re-test.

## Best Practices

### Path Management

- **Always use absolute paths** (`/home/user/...` etc.)
- Avoid relative paths
- Import multiple packages individually

**Standard Smalltalk Project Structure:**

Most Smalltalk projects follow these conventions:

1. **Source directories**: Packages are typically located in:
   - `src/` (most common)
   - `repositories/` (older projects)

2. **Project metadata**: Check `.project` file in project root:
   ```json
   {
     "srcDirectory": "src"
   }
   ```

3. **Typical project layout**:
   ```
   MyProject/
   ├── .project           # Contains srcDirectory configuration
   ├── src/              # Source directory (or 'repositories')
   │   ├── MyPackage/
   │   │   ├── MyClass.st
   │   │   └── package.st
   │   └── MyPackage-Tests/
   │       └── ...
   └── README.md
   ```

**Path Discovery Strategy:**

1. Check for `.project` file and read `srcDirectory` field
2. If no `.project`, look for `src/` directory
3. Fall back to `repositories/` for older projects
4. Construct absolute path: `<project_root>/<srcDirectory>`

**Example:**
```bash
# If .project says "srcDirectory": "src"
import_package: 'MyPackage' path: '/home/user/MyProject/src'

# If .project says "srcDirectory": "repositories"  
import_package: 'MyPackage' path: '/home/user/MyProject/repositories'
```

### File Editing

- Tonel files in the AI editor are the single source of truth
- Avoid direct editing in Pharo
- Use `export_package` only when necessary (rare)

### Import Timing

- Re-import after every change
- Don't forget to import test packages
- Import order: Main package → Test package

**Package Dependencies and Import Order:**

When importing multiple packages, the correct order is critical. Dependency information is defined in the Baseline:

1. **Baseline location**: `BaselineOf<ProjectName>` package in source directory
   ```
   src/
   ├── BaselineOfMyProject/
   │   ├── BaselineOfMyProject.st
   │   └── package.st
   ├── MyPackage-Core/
   ├── MyPackage-Json/
   └── MyPackage-Tests/
   ```

2. **Dependency definition**: Check `baseline:` method in `BaselineOfMyProject`
   ```smalltalk
   baseline: spec
       <baseline>
       spec for: #common do: [
           spec
               package: 'MyPackage-Core';
               package: 'MyPackage-Json' with: [ spec requires: #('MyPackage-Core') ];
               package: 'MyPackage-Tests' with: [ spec requires: #('MyPackage-Core' 'MyPackage-Json') ]
       ]
   ```

3. **Import order strategy**:
   - Read `BaselineOf<ProjectName>` class's `baseline:` method
   - Parse `requires:` declarations to build dependency graph
   - Import packages in dependency order (dependencies first)
   - Example order: `MyPackage-Core` → `MyPackage-Json` → `MyPackage-Tests`

4. **Automatic dependency resolution**:
   ```bash
   # Instead of manually ordering:
   /st:import MyPackage-Core /path/to/src
   /st:import MyPackage-Json /path/to/src
   /st:import MyPackage-Tests /path/to/src
   
   # AI reads BaselineOfMyProject>>baseline: and imports in correct order automatically
   ```

**Note**: Even without explicit user instruction, AI should check the Baseline to determine the correct import sequence.

### Test Execution

- Run tests after every import
- For specific classes: `run_class_test`
- For entire packages: `run_package_test`

## Automation

When this skill is active, it automatically suggests:

1. Import commands after editing Tonel files
2. Running related tests after import
3. Debugging procedures when tests fail

## Typical Development Session Examples

### Example 1: Creating a New Class

```
User: "Create a Person class with name and age accessors"

Claude:
1. Creates src/MyPackage/Person.st
2. Writes class definition and accessor methods
3. Suggests: mcp__smalltalk-interop__import_package: 'MyPackage' path: '/home/user/project/src'
4. Suggests: mcp__smalltalk-interop__run_class_test: 'PersonTest'
```

### Example 2: Adding Methods

```
User: "Add a fullName method to Person"

Claude:
1. Edits src/MyPackage/Person.st
2. Adds fullName method
3. Suggests: mcp__smalltalk-interop__import_package: 'MyPackage' path: '/home/user/project/src'
4. Suggests: mcp__smalltalk-interop__run_class_test: 'PersonTest'
```

### Example 3: Working with Multiple Packages

```
User: "Add JSON functionality to RediStick package"

Claude:
1. Edits src/RediStick-Json/RsJsonSerializer.st
2. Edits src/RediStick-Json-Tests/RsJsonTest.st
3. Suggests:
   - mcp__smalltalk-interop__import_package: 'RediStick-Json' path: '/home/user/git/RediStick/src'
   - mcp__smalltalk-interop__import_package: 'RediStick-Json-Tests' path: '/home/user/git/RediStick/src'
4. Suggests: mcp__smalltalk-interop__run_class_test: 'RsJsonTest'
```

## Tonel File Structure

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

### package.st Example

```smalltalk
Package { #name : #MyPackage }
```

### Class Definition Example

```smalltalk
Class {
    #name : #MyClass,
    #superclass : #Object,
    #instVars : [
        'instanceVariable'
    ],
    #classVars : [
        'ClassVariable'
    ],
    #category : #'MyPackage'
}
```

### Test Class Example

```smalltalk
TestCase subclass: #MyClassTest
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'MyPackage-Tests'

{ #category : #tests }
MyClassTest >> testBasic [
    | instance |
    instance := MyClass new.
    self assert: instance notNil
]
```

## Common Pitfalls

1. **Path handling**: Always use absolute paths
2. **Import order**: Main package → Test package
3. **Re-import**: Must re-import after every change
4. **Test execution**: Always test after import
5. **On error**: Fix in Tonel file and re-import

## MCP Integration

This skill leverages the following MCP tools:

- **pharo-interop**: Integration with Pharo image
- **smalltalk-validator**: Tonel syntax validation

Always interact with Pharo through these tools.
