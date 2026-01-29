#!/bin/bash
# Setup script to copy Smalltalk development plugin files to .agent/ directory
# This makes the plugin available in Antigravity
#
# Usage:
#   ./scripts/setup-antigravity.sh [target-directory]
#   ./scripts/setup-antigravity.sh -y [target-directory]  # Non-interactive mode
#
# If target-directory is not specified, uses the repository root (project scope).
# MCP config is always copied to ~/.gemini/antigravity/mcp_config.json
# (On WSL2, uses Windows side %USERPROFILE%\.gemini\antigravity instead)

set -e

# Parse arguments
FORCE_YES=false
TARGET_DIR=""

while [[ $# -gt 0 ]]; do
    case $1 in
        -y|--yes)
            FORCE_YES=true
            shift
            ;;
        *)
            TARGET_DIR="$1"
            shift
            ;;
    esac
done

# Get the script directory and project root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Determine target directory for project scope
if [ -z "$TARGET_DIR" ]; then
    TARGET_DIR="$PROJECT_ROOT"
fi
# Convert to absolute path
TARGET_DIR="$(cd "$TARGET_DIR" && pwd)"
ANTIGRAVITY_DIR="$TARGET_DIR/.agent"

ANTIGRAVITY_DIR_NAME=$(basename "$ANTIGRAVITY_DIR")

# Skills and workflows directory names (project scope)
SKILLS_DIR_NAME="skills"
WORKFLOWS_DIR_NAME="workflows"

echo "Setting up Smalltalk development plugin for Antigravity..."
echo "Plugin repository: $PROJECT_ROOT"
echo "Target directory: $TARGET_DIR"
echo "Antigravity directory: $ANTIGRAVITY_DIR ($ANTIGRAVITY_DIR_NAME)"
echo ""

# Create directories
mkdir -p "$ANTIGRAVITY_DIR/$SKILLS_DIR_NAME"
mkdir -p "$ANTIGRAVITY_DIR/$WORKFLOWS_DIR_NAME"
mkdir -p "$ANTIGRAVITY_DIR/prompts"
mkdir -p "$ANTIGRAVITY_DIR/agents"

# Function to copy directory with confirmation
copy_directory() {
    local src="$1"
    local dst="$2"
    local name="$3"
    
    if [ ! -d "$src" ]; then
        echo "⚠️  Warning: Source directory $src does not exist, skipping $name..."
        return
    fi
    
    if [ -d "$dst" ]; then
        if [ "$FORCE_YES" = true ]; then
            echo "Overwriting existing $name (non-interactive mode)..."
            rm -rf "$dst"
        else
            echo "⚠️  Warning: $name already exists in $ANTIGRAVITY_DIR_NAME/"
            read -p "Overwrite? (y/N): " -n 1 -r
            echo
            if [[ ! $REPLY =~ ^[Yy]$ ]]; then
                echo "Skipping $name..."
                return
            fi
            rm -rf "$dst"
        fi
    fi
    
    echo "Copying $name..."
    cp -r "$src" "$dst"
}

# Copy skills
copy_directory "$PROJECT_ROOT/skills" "$ANTIGRAVITY_DIR/$SKILLS_DIR_NAME" "$SKILLS_DIR_NAME"

# Copy agents (raw files)
copy_directory "$PROJECT_ROOT/agents" "$ANTIGRAVITY_DIR/agents" "agents"

# Copy commands to prompts (raw files)
copy_directory "$PROJECT_ROOT/commands" "$ANTIGRAVITY_DIR/prompts" "prompts"

# Generate Workflows for Commands
echo "Generating workflows for commands..."
for cmd_file in "$PROJECT_ROOT/commands"/*.md; do
    filename=$(basename -- "$cmd_file")
    name="${filename%.*}"
    workflow_name="st-${name}"
    target_file="$ANTIGRAVITY_DIR/$WORKFLOWS_DIR_NAME/$workflow_name.md"
    
    # Check if workflow already exists
    if [ -f "$target_file" ] && [ "$FORCE_YES" != true ]; then
        echo "⚠️  Warning: Workflow $workflow_name.md already exists."
        read -p "Overwrite? (y/N): " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            echo "Skipping $workflow_name..."
            continue
        fi
    fi
    
    echo "Generating $workflow_name.md..."
    cat > "$target_file" <<EOL
---
description: Run /st:$name command
---

1. Read the command instructions at \`$ANTIGRAVITY_DIR_NAME/prompts/$filename\`
2. Execute the user's request following those instructions.
EOL
done

# Generate Workflows for Agents
echo "Generating workflows for agents..."
for agent_file in "$PROJECT_ROOT/agents"/*.md; do
    filename=$(basename -- "$agent_file")
    name="${filename%.*}"
    workflow_name="agent-${name}"
    target_file="$ANTIGRAVITY_DIR/$WORKFLOWS_DIR_NAME/$workflow_name.md"
    
    # Check if workflow already exists
    if [ -f "$target_file" ] && [ "$FORCE_YES" != true ]; then
        echo "⚠️  Warning: Workflow $workflow_name.md already exists."
        read -p "Overwrite? (y/N): " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            echo "Skipping $workflow_name..."
            continue
        fi
    fi
    
    echo "Generating $workflow_name.md..."
    cat > "$target_file" <<EOL
---
description: Run $name agent
---

1. Read the agent definition at \`$ANTIGRAVITY_DIR_NAME/agents/$filename\`
2. Adopt the persona and follow the instructions in that file.
EOL
done

# Global refactor of command references (/st:name -> /st-name)
echo "Refactoring command references globally (/st:name -> /st-name)..."
# Target skills, prompts, workflows, and agents
find "$ANTIGRAVITY_DIR" -type f -name "*.md" -exec sed -i 's/\/st:\([a-zA-Z0-9_-]\+\)/\/st-\1/g' {} +

# Copy .mcp.json to user scope
# On WSL2, Antigravity looks at Windows side %USERPROFILE%\.gemini
if [ -f "$PROJECT_ROOT/.mcp.json" ]; then
    # Detect WSL2 and use Windows user profile path
    if grep -qi microsoft /proc/version 2>/dev/null; then
        WIN_USERPROFILE=$(cmd.exe /c "echo %USERPROFILE%" 2>/dev/null | tr -d '\r')
        MCP_TARGET_DIR="$(wslpath "$WIN_USERPROFILE")/.gemini/antigravity"
        echo "WSL2 detected: using Windows path for MCP config"
    else
        MCP_TARGET_DIR="$HOME/.gemini/antigravity"
    fi
    mkdir -p "$MCP_TARGET_DIR"
    target_mcp="$MCP_TARGET_DIR/mcp_config.json"
    if [ -f "$target_mcp" ]; then
        if [ "$FORCE_YES" = true ]; then
            echo "Overwriting $target_mcp..."
            cp "$PROJECT_ROOT/.mcp.json" "$target_mcp"
        else
            echo "⚠️  Warning: $target_mcp already exists"
            read -p "Overwrite? (y/N): " -n 1 -r
            echo
            if [[ $REPLY =~ ^[Yy]$ ]]; then
                echo "Copying .mcp.json to $target_mcp..."
                cp "$PROJECT_ROOT/.mcp.json" "$target_mcp"
            fi
        fi
    else
        echo "Copying .mcp.json to $target_mcp..."
        cp "$PROJECT_ROOT/.mcp.json" "$target_mcp"
    fi
fi

echo ""
echo "✅ Antigravity setup complete!"
echo ""
