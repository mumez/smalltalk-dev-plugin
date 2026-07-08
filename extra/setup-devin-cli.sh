#!/bin/bash
# Setup script to copy Smalltalk development plugin files for Devin CLI
# Skills are copied to .agents/skills/ (project scope) or
# ~/.agents/skills/ (user scope).
# MCP config is merged into .devin/config.json (project scope) or
# ~/.config/devin/config.json (user scope).
#
# Usage:
#   ./extra/setup-devin-cli.sh [target-directory]
#   ./extra/setup-devin-cli.sh -y [target-directory]  # Non-interactive mode
#   ./extra/setup-devin-cli.sh --user                 # Install to user scope ($HOME)
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
    SKILLS_DIR="$HOME/.agents/skills"
    DEVIN_CONFIG="$HOME/.config/devin/config.json"
    echo "Setting up Smalltalk development plugin for Devin CLI (user scope)..."
else
    if [ -z "$TARGET_DIR" ]; then
        TARGET_DIR="$PROJECT_ROOT"
    fi
    TARGET_DIR="$(cd "$TARGET_DIR" && pwd)"
    SKILLS_DIR="$TARGET_DIR/.agents/skills"
    DEVIN_CONFIG="$TARGET_DIR/.devin/config.json"
    echo "Setting up Smalltalk development plugin for Devin CLI..."
    echo "Target directory: $TARGET_DIR"
fi

echo "Plugin repository: $PROJECT_ROOT"
echo "Skills directory: $SKILLS_DIR"
echo "MCP config: $DEVIN_CONFIG"
echo ""

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
            echo "⚠️  Warning: Skill $name already exists in $SKILLS_DIR"
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

# Copy each skill directory to skills/
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

# Merge MCP config into .devin/config.json (or ~/.config/devin/config.json)
echo ""
MCP_JSON="$PROJECT_ROOT/.mcp.json"

mkdir -p "$(dirname "$DEVIN_CONFIG")"

if [ ! -f "$MCP_JSON" ]; then
    echo "⚠️  Warning: .mcp.json not found in plugin repository, skipping MCP config..."
else
    echo "Merging MCP config into $DEVIN_CONFIG..."

    if [ ! -f "$DEVIN_CONFIG" ]; then
        # No existing config.json — create one with mcpServers from .mcp.json
        python3 -c "
import json
with open('$MCP_JSON') as f:
    mcp = json.load(f)
config = {'mcpServers': mcp.get('mcpServers', {})}
with open('$DEVIN_CONFIG', 'w') as f:
    json.dump(config, f, indent=4)
print('Created $DEVIN_CONFIG with MCP server entries.')
"
    else
        # Merge into existing config.json
        python3 -c "
import json

with open('$DEVIN_CONFIG') as f:
    config = json.load(f)
with open('$MCP_JSON') as f:
    mcp = json.load(f)

existing = config.get('mcpServers', {})
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

config['mcpServers'] = existing
with open('$DEVIN_CONFIG', 'w') as f:
    json.dump(config, f, indent=4)

if skipped:
    print('Skipped (already exist): ' + ', '.join(skipped))
    print('To overwrite, run with -y flag or edit $DEVIN_CONFIG manually.')
"
    fi
fi

echo ""
echo "✅ Devin CLI setup complete!"
echo ""
echo "The following have been set up:"
echo "  - $SKILLS_DIR (AI skills for Devin CLI)"
echo "  - MCP config: $DEVIN_CONFIG"
