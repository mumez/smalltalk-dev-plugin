#!/bin/bash
# Setup script to copy Smalltalk development plugin files to .windsurf/ directory
# This makes the plugin available in Windsurf IDE
#
# Usage:
#   ./extra/setup-windsurf.sh [target-directory]
#   ./extra/setup-windsurf.sh -y [target-directory]  # Non-interactive mode
#
# If target-directory is not specified, uses the repository root.
#
# Note: Windsurf uses filename as command name, so commands are prefixed with 'st-'
#       (e.g., init.md -> st-init.md)
#
# References:
#   - Skills: https://docs.windsurf.com/windsurf/cascade/skills
#   - MCP: https://docs.windsurf.com/windsurf/cascade/mcp
#   - Hooks: https://docs.windsurf.com/windsurf/cascade/hooks

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
WINDSURF_DIR="$TARGET_DIR/.windsurf"

echo "Setting up Smalltalk development plugin for Windsurf..."
echo "Plugin repository: $PROJECT_ROOT"
echo "Target directory: $TARGET_DIR"
echo "Windsurf directory: $WINDSURF_DIR"
echo ""

# Create .windsurf directory if it doesn't exist
mkdir -p "$WINDSURF_DIR"
mkdir -p "$WINDSURF_DIR/rules"

# Function to copy directory with confirmation
copy_directory() {
    local src="$1"
    local dst="$2"
    local name="$3"

    if [ ! -d "$src" ]; then
        echo "Warning: Source directory $src does not exist, skipping $name..."
        return
    fi

    if [ -d "$dst" ]; then
        if [ "$FORCE_YES" = true ]; then
            echo "Overwriting existing $name (non-interactive mode)..."
            rm -rf "$dst"
        else
            echo "Warning: $name already exists in .windsurf/"
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

# Copy skills directory
copy_directory "$PROJECT_ROOT/skills" "$WINDSURF_DIR/skills" "skills"

# Copy commands with st- prefix to rules (Windsurf uses filename as command name)
echo "Copying commands with st- prefix..."
mkdir -p "$WINDSURF_DIR/rules"
for cmd_file in "$PROJECT_ROOT/commands"/*.md; do
    if [ -f "$cmd_file" ]; then
        filename=$(basename -- "$cmd_file")
        target_file="$WINDSURF_DIR/rules/st-$filename"

        if [ -f "$target_file" ] && [ "$FORCE_YES" != true ]; then
            echo "Warning: st-$filename already exists."
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

# Copy agents
copy_directory "$PROJECT_ROOT/agents" "$WINDSURF_DIR/agents" "agents"

# Global refactor of command references (/st:name -> /st-name)
echo "Refactoring command references globally (/st:name -> /st-name)..."
find "$WINDSURF_DIR" -type f -name "*.md" -exec sed -i 's/\/st:\([a-zA-Z0-9_-]\+\)/\/st-\1/g' {} +

# Copy .mcp.json
if [ ! -f "$PROJECT_ROOT/.mcp.json" ]; then
    echo "Warning: .mcp.json not found in plugin repository, skipping..."
elif [ -f "$WINDSURF_DIR/mcp.json" ]; then
    if [ "$FORCE_YES" = true ]; then
        echo "Overwriting existing mcp.json (non-interactive mode)..."
        cp "$PROJECT_ROOT/.mcp.json" "$WINDSURF_DIR/mcp.json"
    else
        echo "Warning: mcp.json already exists in .windsurf/"
        read -p "Overwrite? (y/N): " -n 1 -r
        echo
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            echo "Copying .mcp.json..."
            cp "$PROJECT_ROOT/.mcp.json" "$WINDSURF_DIR/mcp.json"
        else
            echo "Skipping mcp.json..."
            echo "Note: You may need to manually merge MCP server configurations."
        fi
    fi
else
    echo "Copying .mcp.json..."
    cp "$PROJECT_ROOT/.mcp.json" "$WINDSURF_DIR/mcp.json"
fi

# Create cascade.json for Windsurf hooks (Windsurf hook configuration)
CASCADE_FILE="$WINDSURF_DIR/cascade.json"
if [ -f "$CASCADE_FILE" ]; then
    if [ "$FORCE_YES" = true ]; then
        echo "Overwriting existing cascade.json (non-interactive mode)..."
    else
        echo "Warning: cascade.json already exists in .windsurf/"
        read -p "Overwrite? (y/N): " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            echo "Skipping cascade.json..."
            CASCADE_FILE=""
        fi
    fi
fi

if [ -n "$CASCADE_FILE" ]; then
    echo "Creating cascade.json for Windsurf hooks..."
    cat > "$CASCADE_FILE" <<'EOF'
{
  "hooks": {
    "afterFileEdit": [
      {
        "command": "./.windsurf/scripts/suggest-class-comment.sh"
      }
    ]
  }
}
EOF
fi

# Copy Windsurf-specific hook script
WINDSURF_HOOK_SCRIPT="$PROJECT_ROOT/extra/suggest-class-comment_windsurf.sh"
if [ -f "$WINDSURF_HOOK_SCRIPT" ]; then
    echo "Copying Windsurf hook script..."
    mkdir -p "$WINDSURF_DIR/scripts"
    cp "$WINDSURF_HOOK_SCRIPT" "$WINDSURF_DIR/scripts/suggest-class-comment.sh"
    chmod +x "$WINDSURF_DIR/scripts/suggest-class-comment.sh"
else
    echo "Warning: Windsurf hook script not found at $WINDSURF_HOOK_SCRIPT"
fi

echo ""
echo "Setup complete!"
echo ""
echo "The following have been copied to .windsurf/:"
echo "  - skills/ (AI skills)"
echo "  - rules/ (commands with st- prefix)"
echo "  - agents/ (AI agents)"
echo "  - mcp.json (MCP server configuration)"
if [ -f "$WINDSURF_DIR/cascade.json" ]; then
    echo "  - cascade.json (afterFileEdit hooks)"
fi
if [ -d "$WINDSURF_DIR/scripts" ]; then
    echo "  - scripts/suggest-class-comment.sh (hook script)"
fi
echo ""
echo "Note: Command references have been converted from /st:name to /st-name format."
echo "Windsurf may require restart to recognize the new configuration."
