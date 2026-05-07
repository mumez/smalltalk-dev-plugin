#!/bin/bash
# Setup script to copy Smalltalk development plugin files for Codex CLI
# Skills are copied to .agents/skills/ in the target directory (project scope).
# Commands are copied to .agents/skills/<command-name>/SKILL.md (one subdir per command).
# MCP config is appended to ~/.codex/config.toml (user scope).
#
# Usage:
#   ./extra/setup-codex.sh [target-directory]
#   ./extra/setup-codex.sh -y [target-directory]  # Non-interactive mode
#   ./extra/setup-codex.sh --user                 # Install to $HOME (user scope)
#
# If target-directory is not specified, uses the repository root.
# Skills are invoked with $<skill-name> (e.g., $st-init).

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
AGENTS_DIR="$TARGET_DIR/.agents"
SKILLS_DIR="$AGENTS_DIR/skills"

echo "Setting up Smalltalk development plugin for Codex CLI..."
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

# Append MCP config to ~/.codex/config.toml (user scope)
echo ""
CODEX_CONFIG_DIR="$HOME/.codex"
CODEX_CONFIG="$CODEX_CONFIG_DIR/config.toml"

mkdir -p "$CODEX_CONFIG_DIR"

# Helper: append or update a TOML [mcp_servers.<name>] block
append_mcp_entry() {
    local section_key="$1"   # e.g. mcp_servers.smalltalk-validator
    local entry="$2"          # full TOML block text

    local escaped_key
    escaped_key=$(echo "$section_key" | sed 's/\./\\./g')

    if [ -f "$CODEX_CONFIG" ] && grep -q "\[$section_key\]" "$CODEX_CONFIG"; then
        if [ "$FORCE_YES" = true ]; then
            echo "[$section_key] already exists in $CODEX_CONFIG, skipping (non-interactive mode)..."
        else
            echo "⚠️  Warning: [$section_key] already exists in $CODEX_CONFIG"
            read -p "Overwrite entry? (y/N): " -n 1 -r
            echo
            if [[ $REPLY =~ ^[Yy]$ ]]; then
                python3 -c "
import re
with open('$CODEX_CONFIG', 'r') as f:
    content = f.read()
content = re.sub(r'\[${escaped_key}\][^\[]*', '', content).rstrip()
with open('$CODEX_CONFIG', 'w') as f:
    f.write(content + '\n\n${entry}\n')
"
                echo "Updated [$section_key] in $CODEX_CONFIG"
            else
                echo "Skipping [$section_key]..."
            fi
        fi
    else
        if [ -f "$CODEX_CONFIG" ]; then
            echo "" >> "$CODEX_CONFIG"
        fi
        printf '%s\n' "$entry" >> "$CODEX_CONFIG"
        echo "Appended [$section_key] to $CODEX_CONFIG"
    fi
}

append_mcp_entry "mcp_servers.smalltalk-interop" \
'[mcp_servers.smalltalk-interop]
command = "uvx"
args = ["--from", "git+https://github.com/mumez/pharo-smalltalk-interop-mcp-server.git", "pharo-smalltalk-interop-mcp-server"]

[mcp_servers.smalltalk-interop.env]
PHARO_SIS_PORT = "8086"'

append_mcp_entry "mcp_servers.smalltalk-validator" \
'[mcp_servers.smalltalk-validator]
command = "uvx"
args = ["--from", "git+https://github.com/mumez/smalltalk-validator-mcp-server.git@main", "smalltalk-validator-mcp-server"]'

echo ""
echo "✅ Codex CLI setup complete!"
echo ""
echo "The following have been set up:"
echo "  - .agents/skills/ (AI skills and commands for Codex CLI)"
echo "  - MCP config: $CODEX_CONFIG"
echo ""
echo "Invoke skills with \$<skill-name> (e.g., \$st-init)."
