#!/bin/bash
# Setup script to copy Smalltalk development plugin files to .cursor/ directory
# This makes the plugin available in Cursor IDE
#
# Usage:
#   ./extra/setup-cursor.sh [target-directory]
#   ./extra/setup-cursor.sh -y [target-directory]  # Non-interactive mode
#   ./extra/setup-cursor.sh --user                 # Install to $HOME (user scope)
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
CURSOR_DIR="$TARGET_DIR/.cursor"

echo "Setting up Smalltalk development plugin for Cursor..."
echo "Plugin repository: $PROJECT_ROOT"
echo "Target directory: $TARGET_DIR"
echo "Cursor directory: $CURSOR_DIR"
echo ""

mkdir -p "$CURSOR_DIR/commands" "$CURSOR_DIR/skills"

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

# Copy skills: st-* go to commands/ as SKILL.md, others go to skills/ as directories
echo "Copying skills..."
for skill_dir in "$PROJECT_ROOT/skills"/*/; do
    [ -d "$skill_dir" ] || continue
    skill_name=$(basename "$skill_dir")
    if [[ "$skill_name" == st-* ]]; then
        copy_file "$skill_dir/SKILL.md" "$CURSOR_DIR/commands/$skill_name.md" "$skill_name.md"
    else
        copy_directory "$skill_dir" "$CURSOR_DIR/skills/$skill_name" "$skill_name"
    fi
done

# Copy .mcp.json
echo ""
if [ ! -f "$PROJECT_ROOT/.mcp.json" ]; then
    echo "⚠️  Warning: .mcp.json not found in plugin repository, skipping..."
elif [ -f "$CURSOR_DIR/mcp.json" ] && ! confirm_overwrite "mcp.json"; then
    echo "Note: You may need to manually merge MCP server configurations."
else
    echo "  Copying mcp.json..."
    cp "$PROJECT_ROOT/.mcp.json" "$CURSOR_DIR/mcp.json"
fi

# Create hooks.json for Cursor
echo ""
HOOKS_FILE="$CURSOR_DIR/hooks.json"
if [ ! -f "$HOOKS_FILE" ] || confirm_overwrite "hooks.json"; then
    echo "  Creating hooks.json..."
    cat > "$HOOKS_FILE" <<'EOF'
{
  "version": 1,
  "hooks": {
    "afterFileEdit": [
      {
        "command": "./scripts/suggest-class-comment.sh"
      }
    ]
  }
}
EOF
fi

# Copy Cursor-specific hook script
echo ""
CURSOR_HOOK_SCRIPT="$PROJECT_ROOT/extra/suggest-class-comment_cursor.sh"
if [ -f "$CURSOR_HOOK_SCRIPT" ]; then
    mkdir -p "$CURSOR_DIR/scripts"
    copy_file "$CURSOR_HOOK_SCRIPT" "$CURSOR_DIR/scripts/suggest-class-comment.sh" "suggest-class-comment.sh"
    chmod +x "$CURSOR_DIR/scripts/suggest-class-comment.sh"
else
    echo "⚠️  Warning: Cursor hook script not found at $CURSOR_HOOK_SCRIPT"
fi

echo ""
echo "✅ Setup complete!"
echo ""
echo "The following have been copied to .cursor/:"
echo "  - commands/ (custom slash commands)"
echo "  - skills/ (AI skills)"
echo "  - mcp.json (MCP server configuration)"
[ -f "$CURSOR_DIR/hooks.json" ] && echo "  - hooks.json (afterFileEdit hooks)"
[ -d "$CURSOR_DIR/scripts" ] && echo "  - scripts/suggest-class-comment.sh (hook script)"
echo ""
echo "Cursor may require restart to recognize the new configuration."
