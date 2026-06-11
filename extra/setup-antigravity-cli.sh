#!/bin/bash
# Setup script to copy Smalltalk development plugin files for Antigravity CLI
# (replaces Gemini CLI — see https://antigravity.google/docs/gcli-migration)
#
# Skills are copied to .agents/skills/ (project scope) or
# ~/.gemini/antigravity-cli/skills/ (user scope).
# Skills double as slash commands; no separate commands handling is needed.
# MCP config is written as a standalone .agents/mcp_config.json (project scope)
# or ~/.gemini/config/mcp_config.json (user scope).
#
# Usage:
#   ./extra/setup-antigravity-cli.sh [target-directory]
#   ./extra/setup-antigravity-cli.sh -y [target-directory]  # Non-interactive mode
#   ./extra/setup-antigravity-cli.sh --user                 # Install to user scope ($HOME)
#
# If target-directory is not specified, uses the repository root.

set -e

# Parse arguments
FORCE_YES=false
TARGET_DIR=""
USER_SCOPE=false

while [[ $# -gt 0 ]]; do
    case $1 in
        -y|--yes)
            FORCE_YES=true
            shift
            ;;
        --user)
            USER_SCOPE=true
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

# Determine target directory and paths
if [ "$USER_SCOPE" = true ]; then
    SKILLS_DIR="$HOME/.gemini/antigravity-cli/skills"
    MCP_TARGET="$HOME/.gemini/config/mcp_config.json"
    echo "Setting up Smalltalk development plugin for Antigravity CLI (user scope)..."
else
    if [ -z "$TARGET_DIR" ]; then
        TARGET_DIR="$PROJECT_ROOT"
    fi
    TARGET_DIR="$(cd "$TARGET_DIR" && pwd)"
    SKILLS_DIR="$TARGET_DIR/.agents/skills"
    MCP_TARGET="$TARGET_DIR/.agents/mcp_config.json"
    echo "Setting up Smalltalk development plugin for Antigravity CLI..."
    echo "Target directory: $TARGET_DIR"
fi

echo "Plugin repository: $PROJECT_ROOT"
echo "Skills directory: $SKILLS_DIR"
echo "MCP config: $MCP_TARGET"
echo ""

mkdir -p "$SKILLS_DIR"

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

copy_skill() {
    local src="$1"
    local dst="$2"
    local name="$3"

    if [ -d "$dst" ]; then
        confirm_overwrite "$name" || return
        rm -rf "$dst"
    fi

    echo "  Copying skill $name..."
    cp -r "$src" "$dst"
}

# Copy each skill directory to skills/
echo "Copying skills..."
if [ ! -d "$PROJECT_ROOT/skills" ]; then
    echo "⚠️  Warning: skills/ directory not found in plugin repository, skipping..."
else
    for skill_dir in "$PROJECT_ROOT/skills"/*/; do
        [ -d "$skill_dir" ] || continue
        skill_name=$(basename "$skill_dir")
        copy_skill "$skill_dir" "$SKILLS_DIR/$skill_name" "$skill_name"
    done
fi

# Copy MCP config as a standalone mcp_config.json
echo ""
MCP_JSON="$PROJECT_ROOT/.mcp.json"

if [ ! -f "$MCP_JSON" ]; then
    echo "⚠️  Warning: .mcp.json not found in plugin repository, skipping MCP config..."
else
    mkdir -p "$(dirname "$MCP_TARGET")"
    if [ -f "$MCP_TARGET" ] && ! confirm_overwrite "$(basename "$MCP_TARGET")"; then
        echo "Skipping MCP config..."
    else
        echo "Writing MCP config to $MCP_TARGET..."
        python3 -c "
import json
with open('$MCP_JSON') as f:
    mcp = json.load(f)
result = {'mcpServers': mcp.get('mcpServers', {})}
with open('$MCP_TARGET', 'w') as f:
    json.dump(result, f, indent=4)
print('  Done.')
"
    fi
fi

echo ""
echo "✅ Antigravity CLI setup complete!"
echo ""
echo "The following have been set up:"
echo "  - $SKILLS_DIR (AI skills for Antigravity CLI)"
echo "  - MCP config: $MCP_TARGET"
