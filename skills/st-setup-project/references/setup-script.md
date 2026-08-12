# Complete Setup Script

Full bash implementation for creating the Smalltalk project boilerplate.

```bash
#!/bin/bash
set -e

PROJECT_NAME="$1"

# Validate PascalCase
if [[ ! "$PROJECT_NAME" =~ ^[A-Z][a-zA-Z0-9]*$ ]]; then
  echo "Error: Project name must be in PascalCase (e.g., MyProject, RedisClient)"
  exit 1
fi

# Check for existing project
if [ -d "src" ] && [ "$(find src -mindepth 1 -maxdepth 1 -type d 2>/dev/null | wc -l)" -gt 0 ]; then
  echo "Error: Project already exists (src/ directory contains packages)"
  echo "This command is for starting new projects from scratch"
  exit 1
fi

# Create directories
mkdir -p "src/BaselineOf${PROJECT_NAME}"
mkdir -p "src/${PROJECT_NAME}-Core"
mkdir -p "src/${PROJECT_NAME}-Tests"

# Create .project if it doesn't exist
if [ ! -f ".project" ]; then
  cat > .project << 'EOF'
{
	'srcDirectory' : 'src'
}
EOF
fi

# Create src/.properties if it doesn't exist
if [ ! -f "src/.properties" ]; then
  cat > "src/.properties" << EOF
{
	#format : #tonel
}
EOF
fi

# Create package.st files
cat > "src/BaselineOf${PROJECT_NAME}/package.st" << EOF
Package { #name : 'BaselineOf${PROJECT_NAME}' }
EOF

cat > "src/${PROJECT_NAME}-Core/package.st" << EOF
Package { #name : '${PROJECT_NAME}-Core' }
EOF

cat > "src/${PROJECT_NAME}-Tests/package.st" << EOF
Package { #name : '${PROJECT_NAME}-Tests' }
EOF

# Create BaselineOf class file (use placeholder to avoid heredoc variable expansion)
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

# Show success message
echo "✓ Smalltalk project '${PROJECT_NAME}' created successfully!"
echo ""
echo "Project structure:"
tree -L 2 src/ 2>/dev/null || find src -type f | sed 's|[^/]*/| |g'
echo ""
echo "Next steps:"
echo "  1. Use /st-init to verify the Smalltalk image connection"
echo "  2. Start adding classes to ${PROJECT_NAME}-Core"
echo "  3. Write tests in ${PROJECT_NAME}-Tests"
```

## BaselineOf Template

The generated `BaselineOf${PROJECT_NAME}.class.st` defines three groups:

| Group | Contents |
|-------|----------|
| `Core` | `ProjectName-Core` |
| `Tests` | `ProjectName-Tests` (requires Core) |
| `all` | Core + Tests |
| `default` | Core only |

This follows the Metacello Baseline convention for organizing package dependencies.
