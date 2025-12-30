---
name: st:lint
description: Lint Tonel files for Smalltalk best practices before import
allowed-tools:
  - Read
  - Glob
  - Bash
---

# Lint Tonel Files

Analyze Tonel files for Smalltalk best practices and code quality issues before importing to Pharo. Helps maintain clean, idiomatic Smalltalk code.

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

### 1. Class Prefix Check (Warning)

**Rule**: Classes should have a project-specific prefix to avoid name collisions.

**Severity**: Warning (not enforced for common patterns)

**Examples**:
- ✅ `STUser`, `JSONParser`, `RedisClient` (prefixed)
- ⚠️ `User`, `Parser`, `Client` (no prefix - potential collision)

**Exceptions**:
- Test classes (ending with `Test`)
- BaselineOf classes
- Classes in well-known namespaces

**Check logic**:
```bash
# Extract class name from Tonel file
class_name=$(grep -E "^\s*#name\s*:\s*#" "$file" | sed "s/.*#name\s*:\s*#\([^,]*\).*/\1/")

# Check if has uppercase prefix (2+ chars)
if ! echo "$class_name" | grep -qE "^[A-Z]{2,}"; then
  echo "⚠️  No class prefix: $class_name (consider adding project prefix)"
fi
```

### 2. Method Length Check (Warning/Error)

**Rule**: Methods should be concise and focused.

**Limits**:
- **Standard methods**: 15 lines (Warning at 16+, Error at 25+)
- **UI building methods**: 40 lines (Warning at 41+)
- **Test methods**: 40 lines (Warning at 41+)
- **Test data generation**: 40 lines (Warning at 41+)

**Severity**:
- 16-24 lines (standard): ⚠️ Warning
- 25+ lines (standard): ❌ Error
- 41+ lines (UI/test): ⚠️ Warning

**Detection**:
```bash
# Count lines in method body (from [ to ])
method_lines=$(awk '/^[A-Za-z].*>>/,/^$/' "$file" | wc -l)

# Check category for exceptions
category=$(grep "#category" method_block)

if [[ "$category" =~ (building|initialization|testing|data) ]]; then
  limit=40
else
  limit=15
fi
```

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

"✅ OK: 35 lines in UI building"
{ #category : #building }
MyWidget >> buildUI [
    "... 35 lines of UI composition ..."
]
```

### 3. Instance Variable Count Check (Warning)

**Rule**: Classes should have focused responsibilities with limited instance variables.

**Limits**:
- **Standard classes**: 10 instance variables max
- **Warning**: 11+ variables suggests responsibility splitting needed

**Severity**: ⚠️ Warning (suggests refactoring)

**Check logic**:
```bash
# Count instance variables in #instVars array
instvar_count=$(grep -A 20 "#instVars\s*:" "$file" | \
  sed -n '/\[/,/\]/p' | \
  grep -o "'" | wc -l)
instvar_count=$((instvar_count / 2))  # Divide by 2 (opening and closing quotes)

if [ "$instvar_count" -gt 10 ]; then
  echo "⚠️  Too many instance variables: $instvar_count (consider splitting responsibilities)"
fi
```

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

"✅ Better: Split into User + UserProfile + UserSettings"
```

### 4. Direct Instance Variable Access Check (Warning)

**Rule**: Access instance variables through methods, not directly (except in initialization and accessors).

**Rationale**:
- Enables lazy initialization
- Allows subclass overrides
- Provides hook points for validation

**Severity**: ⚠️ Warning (suggests adding accessor)

**Detection**:
```bash
# Find methods accessing instance variables directly (not via self)
# Look for: instVarName (not self instVarName)
# Exclude: #initialize, #accessing category

grep -E "^\s+[a-z][a-zA-Z]*\s+(:|:=)" "$file" | \
  grep -v "self" | \
  grep -v "#category.*accessing" | \
  grep -v "#category.*initialization"
```

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

"✅ OK: Direct access in accessor"
{ #category : #accessing }
Person >> firstName: aString [
    firstName := aString
]

"⚠️ Warning: Direct access in business logic"
{ #category : #operations }
Person >> processName [
    ^ firstName asUppercase  "Should be: self firstName asUppercase"
]

"✅ Better: Use accessor for flexibility"
{ #category : #operations }
Person >> processName [
    ^ self firstName asUppercase
]
```

## Implementation

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

### Step 2: Analyze Each File

For each `.st` file (excluding `package.st`):

```bash
for file in "${FILES[@]}"; do
  echo "Analyzing: $file"

  # Rule 1: Check class prefix
  check_class_prefix "$file"

  # Rule 2: Check method lengths
  check_method_lengths "$file"

  # Rule 3: Check instance variable count
  check_instvar_count "$file"

  # Rule 4: Check direct variable access
  check_direct_access "$file"
done
```

### Step 3: Report Results

**Output format**:
```
Linting: src/MyPackage/

✓ MyClass.st
  ⚠️  No class prefix (line 2)
  ⚠️  Method 'complexProcess' too long: 22 lines (line 45)

✓ MyHelper.st
  ❌ Method 'hugeMethod' exceeds limit: 30 lines (line 12)

✓ MyWidget.st
  ⚠️  Too many instance variables: 12 (consider splitting)

Summary:
  Files analyzed: 3
  Warnings: 3
  Errors: 1

⚠️  Consider fixing issues before import
```

### Step 4: Exit Codes

```bash
# Exit codes
# 0: No issues
# 1: Warnings only
# 2: Errors found
```

## Complete Implementation Script

```bash
#!/bin/bash
set -e

TARGET="${1:-.}"
WARNINGS=0
ERRORS=0
FILES_ANALYZED=0

# Color codes
RED='\033[0;31m'
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

# Helper: Extract class name from Tonel file
get_class_name() {
  local file="$1"
  grep -E "^\s*#name\s*:\s*#" "$file" | sed "s/.*#name\s*:\s*#\([^,]*\).*/\1/" | tr -d "'" | head -1
}

# Helper: Check if class name has prefix
check_class_prefix() {
  local file="$1"
  local class_name=$(get_class_name "$file")

  # Skip BaselineOf and Test classes
  if [[ "$class_name" =~ ^BaselineOf || "$class_name" =~ Test$ ]]; then
    return
  fi

  # Check for 2+ uppercase prefix
  if ! echo "$class_name" | grep -qE "^[A-Z]{2,}"; then
    echo -e "  ${YELLOW}⚠️  No class prefix: $class_name (consider adding project prefix)${NC}"
    ((WARNINGS++))
  fi
}

# Helper: Check method lengths
check_method_lengths() {
  local file="$1"
  local in_method=0
  local method_name=""
  local method_lines=0
  local line_num=0
  local category=""

  while IFS= read -r line; do
    ((line_num++))

    # Detect category
    if [[ "$line" =~ #category.*: ]]; then
      category=$(echo "$line" | sed "s/.*#category.*:\s*#\([^}]*\).*/\1/")
    fi

    # Detect method start (ClassName >> methodName)
    if [[ "$line" =~ \>\>.*\[ ]]; then
      in_method=1
      method_name=$(echo "$line" | sed 's/.*>>\s*\([^[]*\).*/\1/' | xargs)
      method_lines=0
    fi

    # Count lines in method
    if [ "$in_method" -eq 1 ]; then
      ((method_lines++))
    fi

    # Detect method end (standalone ] on line)
    if [[ "$line" =~ ^\s*\]\s*$ ]] && [ "$in_method" -eq 1 ]; then
      in_method=0

      # Determine limit based on category
      local limit=15
      if [[ "$category" =~ (building|initialization|testing|data|examples) ]]; then
        limit=40
      fi

      # Check against limit
      if [ "$method_lines" -gt "$limit" ]; then
        if [ "$method_lines" -gt 24 ] && [ "$limit" -eq 15 ]; then
          echo -e "  ${RED}❌ Method '$method_name' too long: $method_lines lines (limit: $limit) at line $line_num${NC}"
          ((ERRORS++))
        else
          echo -e "  ${YELLOW}⚠️  Method '$method_name' long: $method_lines lines (recommended: $limit) at line $line_num${NC}"
          ((WARNINGS++))
        fi
      fi
    fi
  done < "$file"
}

# Helper: Check instance variable count
check_instvar_count() {
  local file="$1"

  # Extract instVars section and count variables
  local instvar_count=$(awk '/#instVars/,/\]/' "$file" | grep -o "'" | wc -l)
  instvar_count=$((instvar_count / 2))

  if [ "$instvar_count" -gt 10 ]; then
    echo -e "  ${YELLOW}⚠️  Too many instance variables: $instvar_count (consider splitting responsibilities)${NC}"
    ((WARNINGS++))
  fi
}

# Helper: Check direct instance variable access
check_direct_access() {
  local file="$1"
  local in_method=0
  local method_name=""
  local category=""
  local line_num=0

  # Get list of instance variables
  local instvars=$(awk '/#instVars/,/\]/' "$file" | grep -o "'[^']*'" | tr -d "'")

  while IFS= read -r line; do
    ((line_num++))

    # Detect category
    if [[ "$line" =~ #category.*: ]]; then
      category=$(echo "$line" | sed "s/.*#category.*:\s*#\([^}]*\).*/\1/")
    fi

    # Skip accessing and initialization categories
    if [[ "$category" =~ (accessing|initialization) ]]; then
      continue
    fi

    # Detect method start
    if [[ "$line" =~ \>\>.*\[ ]]; then
      in_method=1
      method_name=$(echo "$line" | sed 's/.*>>\s*\([^[]*\).*/\1/' | xargs)
    fi

    # Check for direct variable access in method body
    if [ "$in_method" -eq 1 ]; then
      for var in $instvars; do
        # Look for: varName := or varName at start of statement
        if echo "$line" | grep -qE "(^\s+$var\s+:=|^\s+\^\s+$var\s)" && ! echo "$line" | grep -q "self $var"; then
          echo -e "  ${YELLOW}⚠️  Direct access to '$var' in '$method_name' (use self $var) at line $line_num${NC}"
          ((WARNINGS++))
          break
        fi
      done
    fi

    # Detect method end
    if [[ "$line" =~ ^\s*\]\s*$ ]]; then
      in_method=0
    fi
  done < "$file"
}

# Main: Collect files
if [ -f "$TARGET" ]; then
  FILES=("$TARGET")
elif [ -d "$TARGET" ]; then
  FILES=($(find "$TARGET" -name "*.st" ! -name "package.st"))
else
  echo "Error: $TARGET not found"
  exit 1
fi

if [ ${#FILES[@]} -eq 0 ]; then
  echo "No .st files found in $TARGET"
  exit 0
fi

# Analyze each file
echo "Linting Tonel files in $TARGET"
echo ""

for file in "${FILES[@]}"; do
  FILES_ANALYZED=$((FILES_ANALYZED + 1))
  echo -e "${GREEN}✓${NC} $(basename "$file")"

  check_class_prefix "$file"
  check_method_lengths "$file"
  check_instvar_count "$file"
  check_direct_access "$file"

  echo ""
done

# Summary
echo "─────────────────────────────────"
echo "Summary:"
echo "  Files analyzed: $FILES_ANALYZED"
echo "  Warnings: $WARNINGS"
echo "  Errors: $ERRORS"

if [ "$ERRORS" -gt 0 ]; then
  echo ""
  echo -e "${RED}❌ Errors found - consider fixing before import${NC}"
  exit 2
elif [ "$WARNINGS" -gt 0 ]; then
  echo ""
  echo -e "${YELLOW}⚠️  Warnings found - review recommended${NC}"
  exit 1
else
  echo ""
  echo -e "${GREEN}✓ No issues found${NC}"
  exit 0
fi
```

## Usage Examples

### Example 1: Lint Single File

```bash
/st:lint src/MyPackage/Person.st
```

**Output**:
```
✓ Person.st
  ⚠️  No class prefix: Person (consider adding project prefix)

Summary:
  Files analyzed: 1
  Warnings: 1
  Errors: 0
```

### Example 2: Lint Entire Package

```bash
/st:lint src/MyPackage
```

**Output**:
```
✓ Person.st
  ⚠️  No class prefix: Person

✓ Address.st
  ⚠️  Method 'complexValidation' long: 18 lines

✓ UserManager.st
  ❌ Method 'processAllUsers' too long: 35 lines
  ⚠️  Too many instance variables: 12

Summary:
  Files analyzed: 3
  Warnings: 3
  Errors: 1
```

### Example 3: Lint Before Import Workflow

```bash
# Recommended workflow
/st:lint src/MyPackage       # Check code quality
/st:validate src/MyPackage   # Check syntax
/st:import MyPackage /absolute/path/src  # Import to Pharo
```

## Configuration (Future Enhancement)

Could add `.stlintrc` configuration file:

```json
{
  "rules": {
    "class-prefix": "warning",
    "method-length": {
      "standard": 15,
      "ui": 40,
      "test": 40
    },
    "max-instance-vars": 10,
    "direct-access": "warning"
  },
  "ignore": [
    "src/Generated/**",
    "src/ThirdParty/**"
  ]
}
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
