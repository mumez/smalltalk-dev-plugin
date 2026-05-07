#!/bin/bash
# Setup script to copy Smalltalk development plugin files for OpenCode
# Skills are copied to .agents/skills/ in the target directory (project scope).
# Commands are copied to .opencode/commands/ (project scope).
# MCP config is copied to opencode.json in the target directory.
#
# Usage:
#   ./extra/setup-opencode.sh [target-directory]
#   ./extra/setup-opencode.sh -y [target-directory]  # Non-interactive mode
#   ./extra/setup-opencode.sh --user                 # Install to $HOME (user scope)
#
# If target-directory is not specified, uses the repository root.
# If opencode.json already exists, the mcp section is merged automatically
# if jq is available, otherwise manual merge instructions are shown.

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

# Determine target directory
if [ "$USER_SCOPE" = true ]; then
    TARGET_DIR="$HOME"
elif [ -z "$TARGET_DIR" ]; then
    TARGET_DIR="$PROJECT_ROOT"
fi

# Convert to absolute path
TARGET_DIR="$(cd "$TARGET_DIR" && pwd)"

AGENTS_SKILLS_DIR="$TARGET_DIR/.agents/skills"
OPENCODE_COMMANDS_DIR="$TARGET_DIR/.opencode/commands"
OPENCODE_CONFIG="$TARGET_DIR/opencode.json"
SOURCE_MCP="$PROJECT_ROOT/extra/opencode.json"

echo "Setting up Smalltalk development plugin for OpenCode..."
echo "Plugin repository: $PROJECT_ROOT"
echo "Target directory: $TARGET_DIR"
echo ""

mkdir -p "$AGENTS_SKILLS_DIR" "$OPENCODE_COMMANDS_DIR"

# Returns 0 (proceed) or 1 (skip) based on FORCE_YES or user input
confirm_overwrite() {
    local name="$1"
    if [ "$FORCE_YES" = true ]; then
        echo "  Overwriting existing $name (non-interactive mode)..."
        return 0
    fi
    echo "⚠️  Warning: $name already exists"
    read -p "  Overwrite? (y/N): " -n 1 -r
    echo
    [[ $REPLY =~ ^[Yy]$ ]] || { echo "  Skipping $name..."; return 1; }
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

# Copy skills: st-* go to commands/ as SKILL.md, others go to .agents/skills/ as directories
echo "Copying skills..."
if [ ! -d "$PROJECT_ROOT/skills" ]; then
    echo "⚠️  Warning: skills/ directory not found in plugin repository, skipping..."
else
    for skill_dir in "$PROJECT_ROOT/skills"/*/; do
        [ -d "$skill_dir" ] || continue
        skill_name=$(basename "$skill_dir")
        if [[ "$skill_name" == st-* ]]; then
            copy_file "$skill_dir/SKILL.md" "$OPENCODE_COMMANDS_DIR/$skill_name.md" "$skill_name.md"
        else
            copy_directory "$skill_dir" "$AGENTS_SKILLS_DIR/$skill_name" "$skill_name"
        fi
    done
fi

# Copy MCP config to opencode.json
echo ""
echo "Configuring MCP servers in opencode.json..."

if [ ! -f "$SOURCE_MCP" ]; then
    echo "⚠️  Warning: $SOURCE_MCP not found, skipping MCP config..."
elif [ ! -f "$OPENCODE_CONFIG" ]; then
    echo "  Copying opencode.json..."
    cp "$SOURCE_MCP" "$OPENCODE_CONFIG"
else
    echo "opencode.json already exists in target directory."

    if command -v jq &> /dev/null; then
        echo "jq detected: merging mcp section automatically..."
        _do_merge=false
        if [ "$FORCE_YES" = true ]; then
            _do_merge=true
        else
            read -p "Merge mcp section into existing opencode.json? (y/N): " -n 1 -r
            echo
            [[ $REPLY =~ ^[Yy]$ ]] && _do_merge=true || echo "Skipping MCP config. See $SOURCE_MCP for the mcp section to add manually."
        fi

        if [ "$_do_merge" = true ]; then
            BACKUP="$OPENCODE_CONFIG.bak"
            cp "$OPENCODE_CONFIG" "$BACKUP"
            echo "Backup created: $BACKUP"
            jq -s '.[0] * {"mcp": ((.[0].mcp // {}) + .[1].mcp)}' \
                "$OPENCODE_CONFIG" "$SOURCE_MCP" > "$OPENCODE_CONFIG.tmp" && \
                mv "$OPENCODE_CONFIG.tmp" "$OPENCODE_CONFIG"
            echo "mcp section merged into opencode.json."
        fi
    else
        echo "⚠️  jq is not installed. Please merge the mcp section manually."
        echo ""
        echo "Add the following to your opencode.json:"
        echo "---"
        cat "$SOURCE_MCP"
        echo "---"
        echo ""
        echo "Source file: $SOURCE_MCP"
    fi
fi

echo ""
echo "✅ OpenCode setup complete!"
echo ""
echo "The following have been set up:"
echo "  - .agents/skills/ (AI skills)"
echo "  - .opencode/commands/ (custom slash commands)"
echo "  - opencode.json (MCP server configuration)"
echo ""
echo "OpenCode may require restart to recognize the new configuration."
