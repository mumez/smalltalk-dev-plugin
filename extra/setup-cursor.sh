#!/bin/bash
# Setup script to copy Smalltalk development plugin files to .cursor/ directory
# This makes the plugin available in Cursor IDE
#
# Usage:
#   ./extra/setup-cursor.sh [target-directory]
#   ./extra/setup-cursor.sh -y [target-directory]  # Non-interactive mode
#
# If target-directory is not specified, uses the repository root.
#
# Note: Cursor uses filename as command name, so commands are prefixed with 'st-'
#       (e.g., init.md -> st-init.md)

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

# Determine target directory
if [ -z "$TARGET_DIR" ]; then
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

# Create .cursor directory if it doesn't exist
mkdir -p "$CURSOR_DIR"
mkdir -p "$CURSOR_DIR/commands"

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
            echo "⚠️  Warning: $name already exists in .cursor/"
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

# Copy commands with st- prefix (Cursor uses filename as command name)
echo "Copying commands with st- prefix..."
for cmd_file in "$PROJECT_ROOT/commands"/*.md; do
    if [ -f "$cmd_file" ]; then
        filename=$(basename -- "$cmd_file")
        target_file="$CURSOR_DIR/commands/st-$filename"

        if [ -f "$target_file" ] && [ "$FORCE_YES" != true ]; then
            echo "⚠️  Warning: st-$filename already exists."
            read -p "Overwrite? (y/N): " -n 1 -r
            echo
            if [[ ! $REPLY =~ ^[Yy]$ ]]; then
                echo "Skipping st-$filename..."
                continue
            fi
        fi

        cp "$cmd_file" "$target_file"
        echo "  Copied st-$filename"
    fi
done

# Copy skills
copy_directory "$PROJECT_ROOT/skills" "$CURSOR_DIR/skills" "skills"

# Copy agents
copy_directory "$PROJECT_ROOT/agents" "$CURSOR_DIR/agents" "agents"

# Global refactor of command references (/st:name -> /st-name)
echo "Refactoring command references globally (/st:name -> /st-name)..."
find "$CURSOR_DIR" -type f -name "*.md" -exec sed -i 's/\/st:\([a-zA-Z0-9_-]\+\)/\/st-\1/g' {} +

# Copy .mcp.json
if [ ! -f "$PROJECT_ROOT/.mcp.json" ]; then
    echo "⚠️  Warning: .mcp.json not found in plugin repository, skipping..."
elif [ -f "$CURSOR_DIR/mcp.json" ]; then
    if [ "$FORCE_YES" = true ]; then
        echo "Overwriting existing mcp.json (non-interactive mode)..."
        cp "$PROJECT_ROOT/.mcp.json" "$CURSOR_DIR/mcp.json"
    else
        echo "⚠️  Warning: mcp.json already exists in .cursor/"
        read -p "Overwrite? (y/N): " -n 1 -r
        echo
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            echo "Copying .mcp.json..."
            cp "$PROJECT_ROOT/.mcp.json" "$CURSOR_DIR/mcp.json"
        else
            echo "Skipping mcp.json..."
            echo "Note: You may need to manually merge MCP server configurations."
        fi
    fi
else
    echo "Copying .mcp.json..."
    cp "$PROJECT_ROOT/.mcp.json" "$CURSOR_DIR/mcp.json"
fi

# Create hooks.json for Cursor (different format and location than Claude Code)
HOOKS_FILE="$CURSOR_DIR/hooks.json"
if [ -f "$HOOKS_FILE" ]; then
    if [ "$FORCE_YES" = true ]; then
        echo "Overwriting existing hooks.json (non-interactive mode)..."
    else
        echo "⚠️  Warning: hooks.json already exists in .cursor/"
        read -p "Overwrite? (y/N): " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            echo "Skipping hooks.json..."
            HOOKS_FILE=""
        fi
    fi
fi

if [ -n "$HOOKS_FILE" ]; then
    echo "Creating hooks.json for Cursor..."
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
CURSOR_HOOK_SCRIPT="$PROJECT_ROOT/extra/suggest-class-comment_cursor.sh"
if [ -f "$CURSOR_HOOK_SCRIPT" ]; then
    echo "Copying Cursor hook script..."
    mkdir -p "$TARGET_DIR/scripts"
    cp "$CURSOR_HOOK_SCRIPT" "$TARGET_DIR/scripts/suggest-class-comment.sh"
    chmod +x "$TARGET_DIR/scripts/suggest-class-comment.sh"
else
    echo "⚠️  Warning: Cursor hook script not found at $CURSOR_HOOK_SCRIPT"
fi

echo ""
echo "✅ Setup complete!"
echo ""
echo "The following have been copied to .cursor/:"
echo "  - commands/ (custom slash commands with st- prefix)"
echo "  - skills/ (AI skills)"
echo "  - agents/ (AI agents)"
echo "  - mcp.json (MCP server configuration)"
if [ -f "$CURSOR_DIR/hooks.json" ]; then
    echo "  - hooks.json (afterFileEdit hooks)"
fi
if [ -d "$TARGET_DIR/scripts" ]; then
    echo "  - scripts/suggest-class-comment.sh (hook script)"
fi
echo ""
echo "Note: Command references have been converted from /st:name to /st-name format."
echo "Cursor may require restart to recognize the new configuration."
