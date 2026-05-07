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

SKILLS_DIR_NAME="skills"
WORKFLOWS_DIR_NAME="workflows"

echo "Setting up Smalltalk development plugin for Windsurf..."
echo "Plugin repository: $PROJECT_ROOT"
echo "Target directory: $TARGET_DIR"
echo "Windsurf directory: $WINDSURF_DIR ($WINDSURF_DIR_NAME)"
echo ""

mkdir -p "$WINDSURF_DIR/$SKILLS_DIR_NAME" "$WINDSURF_DIR/$WORKFLOWS_DIR_NAME" "$WINDSURF_DIR/prompts"

# Returns 0 (proceed) or 1 (skip) based on FORCE_YES or user input
confirm_overwrite() {
    local name="$1"
    if [ "$FORCE_YES" = true ]; then
        echo "Overwriting existing $name (non-interactive mode)..."
        return 0
    fi
    echo "⚠️  Warning: $name already exists"
    read -p "Overwrite? (y/N): " -n 1 -r
    echo
    [[ $REPLY =~ ^[Yy]$ ]] || { echo "Skipping $name..."; return 1; }
}

copy_file() {
    local src="$1" dst="$2" name="$3"
    if [ ! -f "$src" ]; then
        echo "⚠️  Warning: $src not found, skipping $name..."
        return
    fi
    if [ -f "$dst" ]; then
        confirm_overwrite "$name" || return
    fi
    echo "  Copying $name..."
    cp "$src" "$dst"
}

copy_directory() {
    local src="$1" dst="$2" name="$3"
    if [ ! -d "$src" ]; then
        echo "⚠️  Warning: Source directory $src does not exist, skipping $name..."
        return
    fi
    if [ -d "$dst" ]; then
        confirm_overwrite "$name" || return
        rm -rf "$dst"
    fi
    echo "  Copying $name..."
    cp -r "$src" "$dst"
}

# Copy non-st-* skills to skills/ (st-* go to workflows/ and prompts/ below)
echo "Copying skills..."
for skill_dir in "$PROJECT_ROOT/skills"/*/; do
    [ -d "$skill_dir" ] || continue
    skill_name=$(basename "$skill_dir")
    [[ "$skill_name" == st-* ]] && continue
    copy_directory "$skill_dir" "$WINDSURF_DIR/$SKILLS_DIR_NAME/$skill_name" "$skill_name"
done

# Generate workflows and prompts for st-* skills
echo ""
echo "Generating workflows for st-* skills..."
for skill_dir in "$PROJECT_ROOT/skills"/st-*/; do
    [ -d "$skill_dir" ] || continue
    skill_name=$(basename "$skill_dir")
    skill_file="$skill_dir/SKILL.md"
    [ -f "$skill_file" ] || continue

    prompt_target="$WINDSURF_DIR/prompts/$skill_name.md"
    workflow_target="$WINDSURF_DIR/$WORKFLOWS_DIR_NAME/$skill_name.md"

    if { [ -f "$prompt_target" ] || [ -f "$workflow_target" ]; }; then
        confirm_overwrite "$skill_name" || continue
    fi

    echo "  Generating $skill_name prompt and workflow..."
    cp "$skill_file" "$prompt_target"
    cat > "$workflow_target" <<EOL
---
description: Run /$skill_name skill
---

1. Read the skill instructions at \`$WINDSURF_DIR_NAME/prompts/$skill_name.md\`
2. Execute the user's request following those instructions.
EOL
done

# Copy MCP config to user scope
# Windsurf uses ~/.codeium/windsurf/mcp_config.json
# On WSL2, use Windows side %USERPROFILE%\.codeium\windsurf
echo ""
if [ -f "$PROJECT_ROOT/.mcp.json" ]; then
    if grep -qi microsoft /proc/version 2>/dev/null; then
        WIN_USERPROFILE=$(cmd.exe /c "echo %USERPROFILE%" 2>/dev/null | tr -d '\r')
        MCP_TARGET_DIR="$(wslpath "$WIN_USERPROFILE")/.codeium/windsurf"
        echo "WSL2 detected: using Windows path for MCP config"
    else
        MCP_TARGET_DIR="$HOME/.codeium/windsurf"
    fi
    mkdir -p "$MCP_TARGET_DIR"
    target_mcp="$MCP_TARGET_DIR/mcp_config.json"
    if [ ! -f "$target_mcp" ] || confirm_overwrite "mcp_config.json"; then
        echo "  Copying .mcp.json to $target_mcp..."
        cp "$PROJECT_ROOT/.mcp.json" "$target_mcp"
    fi
fi

echo ""
echo "✅ Windsurf setup complete!"
echo ""
echo "The following have been set up:"
echo "  - $WINDSURF_DIR_NAME/skills/ (AI skills with SKILL.md)"
echo "  - $WINDSURF_DIR_NAME/workflows/ (workflows for commands)"
echo "  - $WINDSURF_DIR_NAME/prompts/ (command prompt files)"
echo "  - MCP config: ${target_mcp:-~/.codeium/windsurf/mcp_config.json}"
echo ""
echo "Windsurf may require restart to recognize the new configuration."
