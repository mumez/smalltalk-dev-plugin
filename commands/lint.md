---
name: st:lint
description: Lint Tonel files for Smalltalk best practices before import
allowed-tools:
  - mcp__smalltalk-validator__lint_tonel_smalltalk_from_file
  - mcp__smalltalk-validator__lint_tonel_smalltalk
  - Glob
  - Bash
---

# Lint Tonel Files

Analyze Tonel files for Smalltalk best practices and code quality issues before importing to Pharo. Uses the smalltalk-validator MCP server to check code quality.

## Usage

```bash
# Lint specific file
/st:lint src/MyPackage/MyClass.st

# Lint entire package
/st:lint src/MyPackage

# Lint all packages in src/
/st:lint src
```

## Lint Rules

The MCP server checks for the following Smalltalk best practices:

### 1. Class Prefix Check (Warning)

**Rule**: Classes should have a project-specific prefix to avoid name collisions.

**Examples**:
- ✅ `STUser`, `JSONParser`, `RedisClient` (prefixed)
- ⚠️ `User`, `Parser`, `Client` (no prefix - potential collision)

**Exceptions**:
- Test classes (ending with `Test`)
- BaselineOf classes

### 2. Method Length Check (Warning/Error)

**Rule**: Methods should be concise and focused.

**Limits**:
- **Standard methods**: 15 lines (Warning at 16+, Error at 25+)
- **UI building methods**: 40 lines (Warning at 41+)
- **Test methods**: 40 lines (Warning at 41+)

**Examples**:
```smalltalk
"✅ Good: Focused 5-line method"
Person >> fullName [
    ^ firstName, ' ', lastName
]

"⚠️ Warning: 20 lines (consider extracting helpers)"
Calculator >> complexCalculation [
    "... 20 lines of logic ..."
]

"❌ Error: 30 lines in standard method"
DataProcessor >> process [
    "... 30 lines - refactor needed ..."
]
```

### 3. Instance Variable Count Check (Warning)

**Rule**: Classes should have focused responsibilities with limited instance variables.

**Limits**:
- **Standard classes**: 10 instance variables max
- **Warning**: 11+ variables suggests responsibility splitting needed

**Examples**:
```smalltalk
"✅ Good: 4 focused instance variables"
Class {
    #name : #Person,
    #instVars : [
        'firstName',
        'lastName',
        'age',
        'email'
    ]
}

"⚠️ Warning: 12 variables - consider splitting"
Class {
    #name : #User,
    #instVars : [
        'firstName', 'lastName', 'email', 'phone',
        'address', 'city', 'state', 'zip',
        'role', 'permissions', 'preferences', 'settings'
    ]
}
```

### 4. Direct Instance Variable Access Check (Warning)

**Rule**: Access instance variables through methods, not directly (except in initialization and accessors).

**Rationale**:
- Enables lazy initialization
- Allows subclass overrides
- Provides hook points for validation

**Examples**:
```smalltalk
"✅ Good: Access via method"
Person >> greet [
    ^ 'Hello, ', self firstName
]

"✅ OK: Direct access in initialize"
{ #category : #initialization }
Person >> initialize [
    super initialize.
    firstName := ''.
    age := 0
]

"⚠️ Warning: Direct access in business logic"
{ #category : #operations }
Person >> processName [
    ^ firstName asUppercase  "Should be: self firstName asUppercase"
]
```

## Implementation

This command uses the `mcp__smalltalk-validator__lint_tonel_smalltalk_from_file` MCP tool to perform linting.

### Step 1: Determine Scope

```bash
TARGET="$1"

if [ -z "$TARGET" ]; then
  echo "Error: No target specified"
  echo "Usage: /st:lint <file-or-directory>"
  exit 1
fi

# Collect files to lint
if [ -f "$TARGET" ]; then
  FILES=("$TARGET")
elif [ -d "$TARGET" ]; then
  FILES=($(find "$TARGET" -name "*.st" ! -name "package.st"))
else
  echo "Error: $TARGET not found"
  exit 1
fi
```

### Step 2: Lint Each File

For each `.st` file (excluding `package.st`), call the MCP lint tool:

```bash
for file in "${FILES[@]}"; do
  echo "Linting: $file"

  # Call MCP lint tool
  mcp__smalltalk-validator__lint_tonel_smalltalk_from_file "$file"
done
```

### Step 3: Report Results

The MCP server returns lint results in this format:

```json
{
  "success": true,
  "result": {
    "issues": [
      {
        "type": "warning",
        "rule": "class_prefix",
        "message": "No class prefix: Person (consider adding project prefix)",
        "line": 2
      },
      {
        "type": "warning",
        "rule": "method_length",
        "message": "Method 'complexProcess' too long: 22 lines",
        "line": 45
      }
    ],
    "summary": {
      "files_analyzed": 1,
      "warnings": 2,
      "errors": 0
    }
  }
}
```

### Step 4: Display and Exit

Display the lint results and exit with appropriate code:

```bash
# Exit codes
# 0: No issues
# 1: Warnings only
# 2: Errors found
```

## Usage Examples

### Example 1: Lint Single File

```bash
/st:lint src/MyPackage/Person.st
```

The MCP tool will analyze the file and return issues found.

### Example 2: Lint Entire Package

```bash
/st:lint src/MyPackage
```

Lints all `.st` files in the package directory.

### Example 3: Lint Before Import Workflow

```bash
# Recommended workflow
/st:lint src/MyPackage       # Check code quality (MCP)
/st:import MyPackage /absolute/path/src  # Import to Pharo
/st:test MyPackage-Tests     # Run tests
```

## Integration with Other Commands

- **Before `/st:import`**: Run lint to ensure code quality
- **After code generation**: Lint AI-generated code
- **CI/CD**: Add to pre-commit hooks

## Related Commands

- **`/st:validate`** - Syntax validation (Tonel structure)
- **`/st:import`** - Import to Pharo (after linting)
- **`/st:test`** - Run tests (after import)

## Notes for Claude

When executing this command:

1. **Read target files** - Use Read tool to analyze Tonel files
2. **Apply rules systematically** - Check each rule for each file
3. **Provide clear output** - Show file name, issue type, line number
4. **Prioritize errors** - Distinguish between warnings and errors
5. **Suggest fixes** - Provide actionable recommendations

This command helps maintain high code quality and idiomatic Smalltalk style before importing to Pharo.
