---
name: st:setup-project
description: Set up Pharo project boilerplate structure from scratch
allowed-tools:
  - Bash
  - AskUserQuestion
  - Read
---

# Set Up Pharo Project Boilerplate

Create a complete Pharo project structure with BaselineOf, core package, and tests package. Designed for beginners starting a new Smalltalk project from scratch.

## Usage

```bash
# With project name argument
/st:setup-project MyProject

# Interactive mode (prompts for project name)
/st:setup-project
```

## Implementation Steps

### 1. Get Project Name

**If project name provided as argument:**
- Use the provided name directly

**If no argument:**
- Use AskUserQuestion to ask: "What is your project name? (Use PascalCase like MyProject)"
- Get the name from user response

### 2. Validate Project Name

**PascalCase validation:**
- Project name MUST start with uppercase letter
- Must contain only alphanumeric characters (no spaces, hyphens, underscores)
- Should follow PascalCase convention (e.g., MyProject, RedisClient, JSONParser)

**If invalid:**
- Stop execution and show error: "Project name must be in PascalCase (e.g., MyProject, RedisClient)"

**Validation pattern:**
```bash
if [[ ! "$PROJECT_NAME" =~ ^[A-Z][a-zA-Z0-9]*$ ]]; then
  echo "Error: Project name must be in PascalCase (e.g., MyProject)"
  exit 1
fi
```

### 3. Check for Existing Project

**Check if `src/` directory exists and contains packages:**

```bash
# Check if src/ exists and has subdirectories (packages)
if [ -d "src" ] && [ "$(find src -mindepth 1 -maxdepth 1 -type d 2>/dev/null | wc -l)" -gt 0 ]; then
  echo "Error: Project already exists (src/ directory contains packages)"
  echo "This command is for starting new projects from scratch"
  exit 1
fi
```

**If packages exist:**
- Stop execution immediately
- Show message: "Project already exists. This command is for starting new projects from scratch."

### 4. Create Directory Structure

Create the following directories:

```bash
mkdir -p src/BaselineOf${PROJECT_NAME}
mkdir -p src/${PROJECT_NAME}-Core
mkdir -p src/${PROJECT_NAME}-Tests
```

### 5. Create .project File

**If `.project` does NOT exist**, create it with Pharo's JSON-like format (using single quotes):

```bash
cat > .project << 'EOF'
{
	'srcDirectory' : 'src'
}
EOF
```

**Important:**
- Use single quotes (Pharo's format, not standard JSON)
- Only create if file doesn't exist
- Use tab for indentation

### 6. Create package.st Files

Create `package.st` in each package directory:

**src/BaselineOf${PROJECT_NAME}/package.st:**
```bash
cat > "src/BaselineOf${PROJECT_NAME}/package.st" << EOF
Package { #name : 'BaselineOf${PROJECT_NAME}' }
EOF
```

**src/${PROJECT_NAME}-Core/package.st:**
```bash
cat > "src/${PROJECT_NAME}-Core/package.st" << EOF
Package { #name : '${PROJECT_NAME}-Core' }
EOF
```

**src/${PROJECT_NAME}-Tests/package.st:**
```bash
cat > "src/${PROJECT_NAME}-Tests/package.st" << EOF
Package { #name : '${PROJECT_NAME}-Tests' }
EOF
```

### 7. Create BaselineOf Class File

Create `src/BaselineOf${PROJECT_NAME}/BaselineOf${PROJECT_NAME}.class.st` with the baseline definition:

```bash
cat > "src/BaselineOf${PROJECT_NAME}/BaselineOf${PROJECT_NAME}.class.st" << 'BASELINE_EOF'
Class {
	#name : 'BaselineOf${PROJECT_NAME}',
	#superclass : 'BaselineOf',
	#category : 'BaselineOf${PROJECT_NAME}'
}

{ #category : 'baselines' }
BaselineOf${PROJECT_NAME} >> baseline: spec [
	<baseline>

	spec for: #common do: [
		"Packages"
		spec
			package: '${PROJECT_NAME}-Core';
			package: '${PROJECT_NAME}-Tests' with: [ spec requires: #('${PROJECT_NAME}-Core') ].

		"Groups"
		spec
			group: 'Core' with: #('${PROJECT_NAME}-Core');
			group: 'Tests' with: #('${PROJECT_NAME}-Tests');
			group: 'all' with: #('Core' 'Tests');
			group: 'default' with: #('Core') ]
]
BASELINE_EOF

# Replace ${PROJECT_NAME} placeholders in the file
sed -i "s/\${PROJECT_NAME}/${PROJECT_NAME}/g" "src/BaselineOf${PROJECT_NAME}/BaselineOf${PROJECT_NAME}.class.st"
```

**Important:**
- Use heredoc with proper escaping
- Replace `${PROJECT_NAME}` placeholders after file creation
- Include proper Tonel class definition format
- Baseline method includes all package dependencies and groups

### 8. Show Success Message

Display created structure:

```bash
echo "✓ Pharo project '${PROJECT_NAME}' created successfully!"
echo ""
echo "Project structure:"
echo "  .project"
echo "  src/"
echo "    BaselineOf${PROJECT_NAME}/"
echo "      package.st"
echo "      BaselineOf${PROJECT_NAME}.class.st"
echo "    ${PROJECT_NAME}-Core/"
echo "      package.st"
echo "    ${PROJECT_NAME}-Tests/"
echo "      package.st"
echo ""
echo "Next steps:"
echo "  1. Use /st:import to load the baseline into Pharo"
echo "  2. Start adding classes to ${PROJECT_NAME}-Core"
echo "  3. Write tests in ${PROJECT_NAME}-Tests"
```

## Complete Implementation Script

Execute this bash script with the project name:

```bash
#!/bin/bash
set -e

PROJECT_NAME="$1"

# Step 1: Get project name if not provided
if [ -z "$PROJECT_NAME" ]; then
  # Use AskUserQuestion to prompt for project name
  # (Claude will handle this automatically)
  exit 1
fi

# Step 2: Validate PascalCase
if [[ ! "$PROJECT_NAME" =~ ^[A-Z][a-zA-Z0-9]*$ ]]; then
  echo "Error: Project name must be in PascalCase (e.g., MyProject, RedisClient)"
  exit 1
fi

# Step 3: Check for existing project
if [ -d "src" ] && [ "$(find src -mindepth 1 -maxdepth 1 -type d 2>/dev/null | wc -l)" -gt 0 ]; then
  echo "Error: Project already exists (src/ directory contains packages)"
  echo "This command is for starting new projects from scratch"
  exit 1
fi

# Step 4: Create directories
mkdir -p "src/BaselineOf${PROJECT_NAME}"
mkdir -p "src/${PROJECT_NAME}-Core"
mkdir -p "src/${PROJECT_NAME}-Tests"

# Step 5: Create .project if it doesn't exist
if [ ! -f ".project" ]; then
  cat > .project << 'EOF'
{
	'srcDirectory' : 'src'
}
EOF
fi

# Step 6: Create package.st files
cat > "src/BaselineOf${PROJECT_NAME}/package.st" << EOF
Package { #name : 'BaselineOf${PROJECT_NAME}' }
EOF

cat > "src/${PROJECT_NAME}-Core/package.st" << EOF
Package { #name : '${PROJECT_NAME}-Core' }
EOF

cat > "src/${PROJECT_NAME}-Tests/package.st" << EOF
Package { #name : '${PROJECT_NAME}-Tests' }
EOF

# Step 7: Create BaselineOf class file
cat > "src/BaselineOf${PROJECT_NAME}/BaselineOf${PROJECT_NAME}.class.st" << 'EOF'
Class {
	#name : 'BaselineOfPROJECT_NAME',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfPROJECT_NAME'
}

{ #category : 'baselines' }
BaselineOfPROJECT_NAME >> baseline: spec [
	<baseline>

	spec for: #common do: [
		"Packages"
		spec
			package: 'PROJECT_NAME-Core';
			package: 'PROJECT_NAME-Tests' with: [ spec requires: #('PROJECT_NAME-Core') ].

		"Groups"
		spec
			group: 'Core' with: #('PROJECT_NAME-Core');
			group: 'Tests' with: #('PROJECT_NAME-Tests');
			group: 'all' with: #('Core' 'Tests');
			group: 'default' with: #('Core') ]
]
EOF

# Replace PROJECT_NAME placeholder (portable for Linux and macOS)
if sed --version 2>&1 | grep -q GNU; then
  sed -i "s/PROJECT_NAME/${PROJECT_NAME}/g" "src/BaselineOf${PROJECT_NAME}/BaselineOf${PROJECT_NAME}.class.st"
else
  sed -i '' "s/PROJECT_NAME/${PROJECT_NAME}/g" "src/BaselineOf${PROJECT_NAME}/BaselineOf${PROJECT_NAME}.class.st"
fi

# Step 8: Show success message
echo "✓ Pharo project '${PROJECT_NAME}' created successfully!"
echo ""
echo "Project structure:"
tree -L 2 src/ 2>/dev/null || find src -type f | sed 's|[^/]*/| |g'
echo ""
echo "Next steps:"
echo "  1. Use /st:import ${PROJECT_NAME} to load into Pharo"
echo "  2. Start adding classes to ${PROJECT_NAME}-Core"
echo "  3. Write tests in ${PROJECT_NAME}-Tests"
```

## Error Handling

**Invalid project name:**
- Show clear error message with example
- Do not create any files

**Project already exists:**
- Detect src/ with packages
- Stop immediately without modifications
- Suggest this is for new projects only

**File system errors:**
- Let bash errors propagate naturally
- `set -e` ensures script stops on any error

## Examples

```bash
# Create project named MyRedisClient
/st:setup-project MyRedisClient

# Interactive mode
/st:setup-project
# (Claude will ask: "What is your project name?")
# User enters: JSONParser

# Invalid name (will fail)
/st:setup-project my-project
# Error: Project name must be in PascalCase

# Already exists (will fail)
/st:setup-project ExistingProject
# Error: Project already exists
```

## Notes for Claude

When executing this command:

1. **Check for argument first** - if user provided project name as argument, use it
2. **If no argument** - use AskUserQuestion to prompt user interactively
3. **Validate immediately** - check PascalCase before any file operations
4. **Check existence** - verify src/ is empty or doesn't exist
5. **Execute script** - run the complete bash script with validated project name
6. **Show clear output** - display the created structure and next steps

This command is designed for **beginners starting from zero**, so clear error messages and guidance are important.

## Related Commands

- `/st:init` - Start Smalltalk development session with skill loading
- `/st:import` - Import created packages into Pharo image
- `/st:export` - Export packages from Pharo to filesystem
