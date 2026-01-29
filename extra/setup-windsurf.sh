#!/bin/bash
# Setup script to copy Smalltalk development plugin files to .windsurf/ directory
# This makes the plugin available in Windsurf IDE
#
# Usage:
#   ./extra/setup-windsurf.sh [target-directory]
#   ./extra/setup-windsurf.sh -y [target-directory]  # Non-interactive mode
#
# If target-directory is not specified, uses the repository root (project scope).
# MCP config is copied to ~/.codeium/windsurf/mcp_config.json
# (On WSL2, uses Windows side %USERPROFILE%\.codeium\windsurf instead)

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
WINDSURF_DIR="$TARGET_DIR/.windsurf"

WINDSURF_DIR_NAME=$(basename "$WINDSURF_DIR")

# Windsurf directory names
SKILLS_DIR_NAME="skills"
WORKFLOWS_DIR_NAME="workflows"

echo "Setting up Smalltalk development plugin for Windsurf..."
echo "Plugin repository: $PROJECT_ROOT"
echo "Target directory: $TARGET_DIR"
echo "Windsurf directory: $WINDSURF_DIR ($WINDSURF_DIR_NAME)"
echo ""

# Create directories
mkdir -p "$WINDSURF_DIR/$SKILLS_DIR_NAME"
mkdir -p "$WINDSURF_DIR/$WORKFLOWS_DIR_NAME"
mkdir -p "$WINDSURF_DIR/prompts"
mkdir -p "$WINDSURF_DIR/agents"

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
            echo "⚠️  Warning: $name already exists in $WINDSURF_DIR_NAME/"
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

# Copy skills (Windsurf uses SKILL.md with frontmatter - same as plugin format)
copy_directory "$PROJECT_ROOT/skills" "$WINDSURF_DIR/$SKILLS_DIR_NAME" "$SKILLS_DIR_NAME"

# Copy agents (raw files)
copy_directory "$PROJECT_ROOT/agents" "$WINDSURF_DIR/agents" "agents"

# Copy commands to prompts (raw files)
copy_directory "$PROJECT_ROOT/commands" "$WINDSURF_DIR/prompts" "prompts"

# Generate Workflows for Commands
echo "Generating workflows for commands..."
for cmd_file in "$PROJECT_ROOT/commands"/*.md; do
    filename=$(basename -- "$cmd_file")
    name="${filename%.*}"
    workflow_name="st-${name}"
    target_file="$WINDSURF_DIR/$WORKFLOWS_DIR_NAME/$workflow_name.md"

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

1. Read the command instructions at \`$WINDSURF_DIR_NAME/prompts/$filename\`
2. Execute the user's request following those instructions.
EOL
done

# Generate Workflows for Agents
echo "Generating workflows for agents..."
for agent_file in "$PROJECT_ROOT/agents"/*.md; do
    filename=$(basename -- "$agent_file")
    name="${filename%.*}"
    workflow_name="agent-${name}"
    target_file="$WINDSURF_DIR/$WORKFLOWS_DIR_NAME/$workflow_name.md"

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

1. Read the agent definition at \`$WINDSURF_DIR_NAME/agents/$filename\`
2. Adopt the persona and follow the instructions in that file.
EOL
done

# Global refactor of command references (/st:name -> /st-name)
echo "Refactoring command references globally (/st:name -> /st-name)..."
# Target skills, prompts, workflows, and agents
find "$WINDSURF_DIR" -type f -name "*.md" -exec sed -i 's/\/st:\([a-zA-Z0-9_-]\+\)/\/st-\1/g' {} +

# Copy MCP config to user scope
# Windsurf uses ~/.codeium/windsurf/mcp_config.json
# On WSL2, use Windows side %USERPROFILE%\.codeium\windsurf
if [ -f "$PROJECT_ROOT/.mcp.json" ]; then
    # Detect WSL2 and use Windows user profile path
    if grep -qi microsoft /proc/version 2>/dev/null; then
        WIN_USERPROFILE=$(cmd.exe /c "echo %USERPROFILE%" 2>/dev/null | tr -d '\r')
        MCP_TARGET_DIR="$(wslpath "$WIN_USERPROFILE")/.codeium/windsurf"
        echo "WSL2 detected: using Windows path for MCP config"
    else
        MCP_TARGET_DIR="$HOME/.codeium/windsurf"
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
echo "✅ Windsurf setup complete!"
echo ""
echo "The following have been set up:"
echo "  - $WINDSURF_DIR_NAME/skills/ (AI skills with SKILL.md)"
echo "  - $WINDSURF_DIR_NAME/workflows/ (workflows for commands and agents)"
echo "  - $WINDSURF_DIR_NAME/prompts/ (command prompt files)"
echo "  - $WINDSURF_DIR_NAME/agents/ (AI agents)"
echo "  - MCP config: ${target_mcp:-~/.codeium/windsurf/mcp_config.json}"
echo ""
echo "Note: Command references have been converted from /st:name to /st-name format."
echo "Windsurf may require restart to recognize the new configuration."
