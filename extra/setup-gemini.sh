#!/bin/bash
# Setup script to copy Smalltalk development plugin files for Gemini CLI
# Skills are copied to .agents/skills/ in the target directory (project scope).
# Commands are copied to .agents/skills/<command-name>/SKILL.md (one subdir per command).
# MCP config is merged into ~/.gemini/settings.json (user scope).
#
# Usage:
#   ./extra/setup-gemini.sh [target-directory]
#   ./extra/setup-gemini.sh -y [target-directory]  # Non-interactive mode
#
# If target-directory is not specified, uses the repository root.

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
AGENTS_DIR="$TARGET_DIR/.agents"
SKILLS_DIR="$AGENTS_DIR/skills"

echo "Setting up Smalltalk development plugin for Gemini CLI..."
echo "Plugin repository: $PROJECT_ROOT"
echo "Target directory: $TARGET_DIR"
echo "Skills directory: $SKILLS_DIR"
echo "Commands → .agents/skills/<name>/SKILL.md"
echo ""

# Create .agents/skills directory if it doesn't exist
mkdir -p "$SKILLS_DIR"

# Function to copy a skill directory with confirmation
copy_skill() {
    local src="$1"
    local dst="$2"
    local name="$3"

    if [ -d "$dst" ]; then
        if [ "$FORCE_YES" = true ]; then
            echo "Overwriting existing skill $name (non-interactive mode)..."
            rm -rf "$dst"
        else
            echo "⚠️  Warning: Skill $name already exists in .agents/skills/"
            read -p "Overwrite? (y/N): " -n 1 -r
            echo
            if [[ ! $REPLY =~ ^[Yy]$ ]]; then
                echo "Skipping $name..."
                return
            fi
            rm -rf "$dst"
        fi
    fi

    echo "  Copying skill $name..."
    cp -r "$src" "$dst"
}

# Copy each skill directory to .agents/skills/
echo "Copying skills..."
if [ ! -d "$PROJECT_ROOT/skills" ]; then
    echo "⚠️  Warning: skills/ directory not found in plugin repository, skipping..."
else
    for skill_dir in "$PROJECT_ROOT/skills"/*/; do
        if [ -d "$skill_dir" ]; then
            skill_name=$(basename "$skill_dir")
            copy_skill "$skill_dir" "$SKILLS_DIR/$skill_name" "$skill_name"
        fi
    done
fi

# Copy each command to .agents/skills/<command-name>/SKILL.md
echo ""
echo "Copying commands to .agents/skills/<name>/SKILL.md..."
if [ ! -d "$PROJECT_ROOT/commands" ]; then
    echo "⚠️  Warning: commands/ directory not found in plugin repository, skipping..."
else
    for cmd_file in "$PROJECT_ROOT/commands"/*.md; do
        if [ -f "$cmd_file" ]; then
            filename=$(basename -- "$cmd_file")
            name="${filename%.*}"
            cmd_dir="$SKILLS_DIR/$name"
            target_file="$cmd_dir/SKILL.md"

            mkdir -p "$cmd_dir"
            if [ -f "$target_file" ]; then
                if [ "$FORCE_YES" = true ]; then
                    echo "  Overwriting $name/SKILL.md..."
                    cp "$cmd_file" "$target_file"
                else
                    echo "⚠️  Warning: $name/SKILL.md already exists in .agents/skills/"
                    read -p "Overwrite? (y/N): " -n 1 -r
                    echo
                    if [[ $REPLY =~ ^[Yy]$ ]]; then
                        cp "$cmd_file" "$target_file"
                        echo "  Copied $name/SKILL.md"
                    else
                        echo "  Skipping $name..."
                    fi
                fi
            else
                cp "$cmd_file" "$target_file"
                echo "  Copied $name/SKILL.md"
            fi
        fi
    done
fi

# Merge MCP config into ~/.gemini/settings.json (user scope)
echo ""
GEMINI_CONFIG_DIR="$HOME/.gemini"
GEMINI_CONFIG="$GEMINI_CONFIG_DIR/settings.json"
MCP_JSON="$PROJECT_ROOT/.mcp.json"

mkdir -p "$GEMINI_CONFIG_DIR"

if [ ! -f "$MCP_JSON" ]; then
    echo "⚠️  Warning: .mcp.json not found in plugin repository, skipping MCP config..."
else
    echo "Merging MCP config into $GEMINI_CONFIG..."

    if [ ! -f "$GEMINI_CONFIG" ]; then
        # No existing settings.json — create one with mcpServers from .mcp.json
        python3 -c "
import json
with open('$MCP_JSON') as f:
    mcp = json.load(f)
settings = {'mcpServers': mcp.get('mcpServers', {})}
with open('$GEMINI_CONFIG', 'w') as f:
    json.dump(settings, f, indent=4)
print('Created $GEMINI_CONFIG with MCP server entries.')
"
    else
        # Merge into existing settings.json
        python3 -c "
import json

with open('$GEMINI_CONFIG') as f:
    settings = json.load(f)
with open('$MCP_JSON') as f:
    mcp = json.load(f)

existing = settings.get('mcpServers', {})
incoming = mcp.get('mcpServers', {})
skipped = []
force_yes = '$FORCE_YES' == 'true'

for name, cfg in incoming.items():
    if name in existing:
        if force_yes:
            existing[name] = cfg
            print(f'  Overwriting mcpServers.{name} (non-interactive mode)...')
        else:
            skipped.append(name)
    else:
        existing[name] = cfg
        print(f'  Added mcpServers.{name}')

settings['mcpServers'] = existing
with open('$GEMINI_CONFIG', 'w') as f:
    json.dump(settings, f, indent=4)

if skipped:
    print('Skipped (already exist): ' + ', '.join(skipped))
    print('To overwrite, run with -y flag or edit $GEMINI_CONFIG manually.')
"
    fi
fi

echo ""
echo "✅ Gemini CLI setup complete!"
echo ""
echo "The following have been set up:"
echo "  - .agents/skills/ (AI skills and commands for Gemini CLI)"
echo "  - MCP config: $GEMINI_CONFIG"
