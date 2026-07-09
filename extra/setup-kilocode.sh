#!/bin/bash
# Setup script to copy Smalltalk development plugin files for Kilo Code
# Skills are copied to .agents/skills/ in the target directory (project scope).
# There are no separate commands; all skills (including st-*) are copied as-is.
# MCP config is derived from extra/opencode.json (Kilo Code is OpenCode-based)
# by swapping the $schema value, then copied to ~/.config/kilo/kilo.json
# (user scope, always).
#
# Usage:
#   ./extra/setup-kilocode.sh [target-directory]
#   ./extra/setup-kilocode.sh -y [target-directory]  # Non-interactive mode
#   ./extra/setup-kilocode.sh --user                 # Install to $HOME (user scope)
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

# Determine target directory
if [ "$USER_SCOPE" = true ]; then
    TARGET_DIR="$HOME"
elif [ -z "$TARGET_DIR" ]; then
    TARGET_DIR="$PROJECT_ROOT"
fi

# Convert to absolute path
TARGET_DIR="$(cd "$TARGET_DIR" && pwd)"

AGENTS_SKILLS_DIR="$TARGET_DIR/.agents/skills"
KILO_CONFIG="$HOME/.config/kilo/kilo.json"
SOURCE_OPENCODE_MCP="$PROJECT_ROOT/extra/opencode.json"

# Derive the Kilo Code MCP config from opencode.json (same content, different
# $schema) so the two configs don't need to be maintained separately.
SOURCE_MCP="$(mktemp)"
trap 'rm -f "$SOURCE_MCP"' EXIT

echo "Setting up Smalltalk development plugin for Kilo Code..."
echo "Plugin repository: $PROJECT_ROOT"
echo "Target directory: $TARGET_DIR"
echo ""

mkdir -p "$AGENTS_SKILLS_DIR"

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

# Copy all skills as-is to .agents/skills/ (no separate commands for Kilo Code)
echo "Copying skills..."
if [ ! -d "$PROJECT_ROOT/skills" ]; then
    echo "⚠️  Warning: skills/ directory not found in plugin repository, skipping..."
else
    for skill_dir in "$PROJECT_ROOT/skills"/*/; do
        [ -d "$skill_dir" ] || continue
        skill_name=$(basename "$skill_dir")
        copy_directory "$skill_dir" "$AGENTS_SKILLS_DIR/$skill_name" "$skill_name"
    done
fi

# Copy MCP config to ~/.config/kilo/kilo.json
echo ""
echo "Configuring MCP servers in kilo.json..."

mkdir -p "$(dirname "$KILO_CONFIG")"

if [ ! -f "$SOURCE_OPENCODE_MCP" ]; then
    echo "⚠️  Warning: $SOURCE_OPENCODE_MCP not found, skipping MCP config..."
    rm -f "$SOURCE_MCP"
elif ! sed 's#"https://opencode.ai/config.json"#"https://app.kilo.ai/config.json"#' \
    "$SOURCE_OPENCODE_MCP" > "$SOURCE_MCP"; then
    echo "⚠️  Warning: Failed to derive kilo.json from $SOURCE_OPENCODE_MCP, skipping MCP config..."
elif [ ! -f "$KILO_CONFIG" ]; then
    echo "  Copying kilo.json to $KILO_CONFIG..."
    cp "$SOURCE_MCP" "$KILO_CONFIG"
else
    echo "kilo.json already exists at $KILO_CONFIG."

    if command -v jq &> /dev/null; then
        echo "jq detected: merging mcp section automatically..."
        _do_merge=false
        if [ "$FORCE_YES" = true ]; then
            _do_merge=true
        else
            read -p "Merge mcp section into existing kilo.json? (y/N): " -n 1 -r
            echo
            [[ $REPLY =~ ^[Yy]$ ]] && _do_merge=true || echo "Skipping MCP config. See $SOURCE_OPENCODE_MCP for the mcp section to add manually."
        fi

        if [ "$_do_merge" = true ]; then
            BACKUP="$KILO_CONFIG.bak"
            cp "$KILO_CONFIG" "$BACKUP"
            echo "Backup created: $BACKUP"
            jq -s '.[0] * {"mcp": ((.[0].mcp // {}) + .[1].mcp)}' \
                "$KILO_CONFIG" "$SOURCE_MCP" > "$KILO_CONFIG.tmp" && \
                mv "$KILO_CONFIG.tmp" "$KILO_CONFIG"
            echo "mcp section merged into kilo.json."
        fi
    else
        echo "⚠️  jq is not installed. Please merge the mcp section manually."
        echo ""
        echo "Add the following to your kilo.json:"
        echo "---"
        cat "$SOURCE_MCP"
        echo "---"
        echo ""
        echo "Derived from: $SOURCE_OPENCODE_MCP"
    fi
fi

echo ""
echo "✅ Kilo Code setup complete!"
echo ""
echo "The following have been set up:"
echo "  - .agents/skills/ (AI skills, including st-* skills)"
echo "  - $KILO_CONFIG (MCP server configuration)"
echo ""
echo "Kilo Code may require restart to recognize the new configuration."
