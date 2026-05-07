#!/bin/bash
# Setup script to copy Smalltalk development plugin files for GitHub Copilot
# Skills are copied to .github/skills/ in the target directory (project scope).
# Commands are copied to .github/skills/<command-name>/SKILL.md (one subdir per command).
# MCP config is copied to ~/.copilot/mcp-config.json (user scope).
#
# Usage:
#   ./extra/setup-copilot.sh [target-directory]
#   ./extra/setup-copilot.sh -y [target-directory]  # Non-interactive mode
#   ./extra/setup-copilot.sh --user                 # Install to $HOME (user scope)
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
GITHUB_DIR="$TARGET_DIR/.github"
SKILLS_DIR="$GITHUB_DIR/skills"

echo "Setting up Smalltalk development plugin for GitHub Copilot..."
echo "Plugin repository: $PROJECT_ROOT"
echo "Target directory: $TARGET_DIR"
echo "Skills directory: $SKILLS_DIR"
echo "Commands → .github/skills/<name>/SKILL.md"
echo ""

# Create .github/skills directory if it doesn't exist
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
            echo "⚠️  Warning: Skill $name already exists in .github/skills/"
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

# Copy each skill directory to .github/skills/
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

# Copy each command to .github/skills/<command-name>/SKILL.md
echo ""
echo "Copying commands to .github/skills/<name>/SKILL.md..."
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
                    echo "⚠️  Warning: $name/SKILL.md already exists in .github/skills/"
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

# Copy MCP config to ~/.copilot/mcp-config.json (user scope)
if [ -f "$PROJECT_ROOT/.mcp.json" ]; then
    MCP_TARGET_DIR="$HOME/.copilot"
    mkdir -p "$MCP_TARGET_DIR"
    target_mcp="$MCP_TARGET_DIR/mcp-config.json"

    if [ -f "$target_mcp" ]; then
        if [ "$FORCE_YES" = true ]; then
            echo "Overwriting $target_mcp (non-interactive mode)..."
            cp "$PROJECT_ROOT/.mcp.json" "$target_mcp"
        else
            echo "⚠️  Warning: $target_mcp already exists"
            read -p "Overwrite? (y/N): " -n 1 -r
            echo
            if [[ $REPLY =~ ^[Yy]$ ]]; then
                echo "Copying .mcp.json to $target_mcp..."
                cp "$PROJECT_ROOT/.mcp.json" "$target_mcp"
            else
                echo "Skipping mcp-config.json..."
                echo "Note: You may need to manually merge MCP server configurations."
            fi
        fi
    else
        echo "Copying .mcp.json to $target_mcp..."
        cp "$PROJECT_ROOT/.mcp.json" "$target_mcp"
    fi
else
    echo "⚠️  Warning: .mcp.json not found in plugin repository, skipping MCP config..."
fi

echo ""
echo "✅ GitHub Copilot setup complete!"
echo ""
echo "The following have been set up:"
echo "  - .github/skills/ (AI skills for GitHub Copilot)"
echo "  - .github/skills/<name>/SKILL.md (each command from commands/)"
echo "  - MCP config: ${target_mcp:-~/.copilot/mcp-config.json}"
echo ""
echo "GitHub Copilot may require VS Code restart to recognize the new configuration."
