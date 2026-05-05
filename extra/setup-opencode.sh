#!/bin/bash
# Setup script to copy Smalltalk development plugin files for OpenCode
# Skills are copied to .agents/skills/ in the target directory (project scope).
# Commands are copied to .opencode/commands/ (project scope).
# MCP config is copied to opencode.json in the target directory.
#
# Usage:
#   ./extra/setup-opencode.sh [target-directory]
#   ./extra/setup-opencode.sh -y [target-directory]  # Non-interactive mode
#
# If target-directory is not specified, uses the repository root.
# If opencode.json already exists, the mcp section is merged automatically
# if jq is available, otherwise manual merge instructions are shown.

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

AGENTS_SKILLS_DIR="$TARGET_DIR/.agents/skills"
OPENCODE_COMMANDS_DIR="$TARGET_DIR/.opencode/commands"
OPENCODE_CONFIG="$TARGET_DIR/opencode.json"
SOURCE_MCP="$PROJECT_ROOT/extra/opencode.json"

echo "Setting up Smalltalk development plugin for OpenCode..."
echo "Plugin repository: $PROJECT_ROOT"
echo "Target directory: $TARGET_DIR"
echo ""

# Create directories
mkdir -p "$AGENTS_SKILLS_DIR"
mkdir -p "$OPENCODE_COMMANDS_DIR"

# Function to copy a skill directory with confirmation
copy_skill() {
    local src="$1"
    local dst="$2"
    local name="$3"

    if [ -d "$dst" ]; then
        if [ "$FORCE_YES" = true ]; then
            echo "  Overwriting existing skill $name (non-interactive mode)..."
            rm -rf "$dst"
        else
            echo "⚠️  Warning: Skill $name already exists in .agents/skills/"
            read -p "  Overwrite? (y/N): " -n 1 -r
            echo
            if [[ ! $REPLY =~ ^[Yy]$ ]]; then
                echo "  Skipping $name..."
                return
            fi
            rm -rf "$dst"
        fi
    fi

    echo "  Copying skill $name..."
    cp -r "$src" "$dst"
}

# Copy skills to .agents/skills/
echo "Copying skills to .agents/skills/..."
if [ ! -d "$PROJECT_ROOT/skills" ]; then
    echo "⚠️  Warning: skills/ directory not found in plugin repository, skipping..."
else
    for skill_dir in "$PROJECT_ROOT/skills"/*/; do
        if [ -d "$skill_dir" ]; then
            skill_name=$(basename "$skill_dir")
            copy_skill "$skill_dir" "$AGENTS_SKILLS_DIR/$skill_name" "$skill_name"
        fi
    done
fi

# Copy commands to .opencode/commands/
echo ""
echo "Copying commands to .opencode/commands/..."
if [ ! -d "$PROJECT_ROOT/commands" ]; then
    echo "⚠️  Warning: commands/ directory not found in plugin repository, skipping..."
else
    for cmd_file in "$PROJECT_ROOT/commands"/*.md; do
        if [ -f "$cmd_file" ]; then
            filename=$(basename -- "$cmd_file")
            target_file="$OPENCODE_COMMANDS_DIR/$filename"

            if [ -f "$target_file" ]; then
                if [ "$FORCE_YES" = true ]; then
                    echo "  Overwriting $filename (non-interactive mode)..."
                    cp "$cmd_file" "$target_file"
                else
                    echo "⚠️  Warning: $filename already exists in .opencode/commands/"
                    read -p "  Overwrite? (y/N): " -n 1 -r
                    echo
                    if [[ $REPLY =~ ^[Yy]$ ]]; then
                        cp "$cmd_file" "$target_file"
                        echo "  Copied $filename"
                    else
                        echo "  Skipping $filename..."
                    fi
                fi
            else
                cp "$cmd_file" "$target_file"
                echo "  Copied $filename"
            fi
        fi
    done
fi

# Copy st-* skills to .opencode/commands/
echo ""
echo "Copying st-* skills to .opencode/commands/..."
for skill_dir in "$PROJECT_ROOT/skills"/st-*/; do
    if [ -d "$skill_dir" ]; then
        skill_name=$(basename "$skill_dir")
        skill_file="$skill_dir/SKILL.md"
        if [ ! -f "$skill_file" ]; then
            continue
        fi
        target_file="$OPENCODE_COMMANDS_DIR/$skill_name.md"

        if [ -f "$target_file" ]; then
            if [ "$FORCE_YES" = true ]; then
                echo "  Overwriting $skill_name.md (non-interactive mode)..."
                cp "$skill_file" "$target_file"
            else
                echo "⚠️  Warning: $skill_name.md already exists in .opencode/commands/"
                read -p "  Overwrite? (y/N): " -n 1 -r
                echo
                if [[ $REPLY =~ ^[Yy]$ ]]; then
                    cp "$skill_file" "$target_file"
                    echo "  Copied $skill_name.md"
                else
                    echo "  Skipping $skill_name..."
                fi
            fi
        else
            cp "$skill_file" "$target_file"
            echo "  Copied $skill_name.md"
        fi
    fi
done

# Copy MCP config to opencode.json
echo ""
echo "Configuring MCP servers in opencode.json..."

if [ ! -f "$SOURCE_MCP" ]; then
    echo "⚠️  Warning: $SOURCE_MCP not found, skipping MCP config..."
elif [ ! -f "$OPENCODE_CONFIG" ]; then
    # No existing config — copy directly
    echo "Copying opencode.json..."
    cp "$SOURCE_MCP" "$OPENCODE_CONFIG"
else
    # opencode.json already exists — try to merge
    echo "opencode.json already exists in target directory."

    if command -v jq &> /dev/null; then
        # jq is available — merge mcp section automatically
        echo "jq detected: merging mcp section automatically..."

        if [ "$FORCE_YES" != true ]; then
            read -p "Merge mcp section into existing opencode.json? (y/N): " -n 1 -r
            echo
            if [[ ! $REPLY =~ ^[Yy]$ ]]; then
                echo "Skipping MCP config. See $SOURCE_MCP for the mcp section to add manually."
            else
                _do_merge=true
            fi
        else
            _do_merge=true
        fi

        if [ "${_do_merge:-false}" = true ]; then
            BACKUP="$OPENCODE_CONFIG.bak"
            cp "$OPENCODE_CONFIG" "$BACKUP"
            echo "Backup created: $BACKUP"
            jq -s '.[0] * {"mcp": ((.[0].mcp // {}) + .[1].mcp)}' \
                "$OPENCODE_CONFIG" "$SOURCE_MCP" > "$OPENCODE_CONFIG.tmp" && \
                mv "$OPENCODE_CONFIG.tmp" "$OPENCODE_CONFIG"
            echo "mcp section merged into opencode.json."
        fi
    else
        # jq not available — show manual instructions
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
