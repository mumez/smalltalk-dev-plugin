# Other AI Agents Setup Guide

This plugin is designed for Claude Code, but can also be used with other AI agents.

## Quick Install with APM (Recommended)

[APM](https://microsoft.github.io/apm/) provides a unified install across agents without running individual setup scripts:

```bash
apm install -g mumez/smalltalk-dev-plugin --target claude,copilot,kiro
```

Replace `claude,copilot,kiro` with the agents you use. To list all supported targets, run:

```bash
apm targets
```

See the [APM documentation](https://microsoft.github.io/apm/) for details.

## Manual Setup with Scripts

Alternatively, setup scripts are provided in the `extra/` directory. Currently supported:

- [Cursor](https://cursor.com/)
- [Windsurf](https://windsurf.com/)
- [Antigravity](https://antigravity.google/)
- [Antigravity CLI](https://antigravity.google/) *(successor to Gemini CLI)*
- [GitHub Copilot CLI](https://github.com/features/copilot/cli)
- [OpenCode](https://opencode.ai/)
- [Codex CLI](https://github.com/openai/codex)
- [Gemini CLI](https://geminicli.com/) *(obsolete — use Antigravity CLI)*

## Prerequisites

- [Pharo](https://pharo.org/) with [PharoSmalltalkInteropServer](https://github.com/mumez/PharoSmalltalkInteropServer) installed
- This plugin repository cloned locally

## Cursor

### Setup

Run the setup script from the plugin repository root:

```bash
./extra/setup-cursor.sh [target-directory]
```

`target-directory` is the project directory where `.cursor/` will be created. If omitted, the plugin repository root is used.

Non-interactive mode (overwrites without confirmation):

```bash
./extra/setup-cursor.sh -y [target-directory]
```

### What the script does

- Creates `.cursor/` directory structure
- Copies commands (filenames already have `st-` prefix)
- Copies `st-*` skills (e.g. `st-init`, `st-setup-project`) as additional command files
- Copies skills and agents
- Copies `.mcp.json` as `.cursor/mcp.json`
- Creates `hooks.json` for `afterFileEdit` event
- Installs hook script for class comment suggestions

### Notes

- Cursor uses the filename as the command name; command files already use the `st-` prefix
- Restart Cursor after setup to recognize the new configuration

## Windsurf

### Setup

```bash
./extra/setup-windsurf.sh [target-directory]
```

Non-interactive mode:

```bash
./extra/setup-windsurf.sh -y [target-directory]
```

### What the script does

- Creates `.windsurf/` directory structure (skills, workflows, prompts, agents)
- Copies skills and agents
- Copies commands as prompt files and generates workflow files for each
- Copies `st-*` skills (e.g. `st-init`, `st-setup-project`) as additional prompt files and generates workflow files for each
- Copies MCP config to `~/.codeium/windsurf/mcp_config.json`
  - On WSL2, uses the Windows-side path (`%USERPROFILE%\.codeium\windsurf\`)

### Notes

- Workflows are generated as entry points that reference the prompt/agent files
- Restart Windsurf after setup

## Antigravity

### Setup

```bash
./extra/setup-antigravity.sh [target-directory]
```

Non-interactive mode:

```bash
./extra/setup-antigravity.sh -y [target-directory]
```

### What the script does

- Creates `.agent/` directory structure (skills, workflows, prompts, agents)
- Copies skills and agents
- Copies commands as prompt files and generates workflow files for each
- Copies `st-*` skills (e.g. `st-init`, `st-setup-project`) as additional prompt files and generates workflow files for each
- Copies MCP config to `~/.gemini/antigravity/mcp_config.json`
  - On WSL2, uses the Windows-side path (`%USERPROFILE%\.gemini\antigravity\`)

### Notes

- Workflows are generated as entry points that reference the prompt/agent files

## GitHub Copilot CLI

### Setup

```bash
./extra/setup-copilot.sh [target-directory]
```

`target-directory` is the project directory where `.github/skills/` will be created. If omitted, the plugin repository root is used.

Non-interactive mode (overwrites without confirmation):

```bash
./extra/setup-copilot.sh -y [target-directory]
```

User scope (installs to `$HOME`, ignores target-directory):

```bash
./extra/setup-copilot.sh --user
```

### What the script does

- Creates `.github/skills/` directory structure
- Copies skills directly into `.github/skills/`
- Copies each command as `.github/skills/<command-name>/SKILL.md`
- Copies MCP config to `~/.copilot/mcp-config.json`

### Notes

- This setup targets the GitHub Copilot CLI, not the VS Code extension

## OpenCode

### Setup

```bash
./extra/setup-opencode.sh [target-directory]
```

`target-directory` is the project directory where `.agents/`, `.opencode/`, and `opencode.json` will be created. If omitted, the plugin repository root is used.

Non-interactive mode (overwrites without confirmation):

```bash
./extra/setup-opencode.sh -y [target-directory]
```

User scope (installs to `$HOME`, ignores target-directory):

```bash
./extra/setup-opencode.sh --user
```

### What the script does

- Creates `.agents/skills/` and copies skills
- Creates `.opencode/commands/` and copies commands
- Copies `st-*` skills (e.g. `st-init`, `st-setup-project`) as additional command files to `.opencode/commands/`
- Copies MCP config to `opencode.json` in the target directory
  - If `opencode.json` already exists and `jq` is available, merges the `mcp` section automatically (backup created as `opencode.json.bak`)
  - If `jq` is not available, prints the `mcp` section for manual merging

### Notes

- OpenCode MCP format differs from Claude Code: `command` is an array combining command and args, `environment` instead of `env`, and `type: "local"` is required
- The pre-converted MCP config is available at `extra/opencode.json`
- Restart OpenCode after setup to recognize the new configuration

## Codex CLI

### Setup

```bash
./extra/setup-codex.sh [target-directory]
```

`target-directory` is the project directory where `.agents/skills/` will be created. If omitted, the plugin repository root is used.

Non-interactive mode (overwrites without confirmation):

```bash
./extra/setup-codex.sh -y [target-directory]
```

User scope (installs to `$HOME`, ignores target-directory):

```bash
./extra/setup-codex.sh --user
```

### What the script does

- Creates `.agents/skills/` directory structure
- Copies skills directly into `.agents/skills/`
- Copies each command as `.agents/skills/<command-name>/SKILL.md`
- Appends MCP config to `~/.codex/config.toml`

### Notes

- Codex CLI does not support custom commands; commands are placed as skills instead
- Skills are invoked with `$<skill-name>` (e.g., `$st-init`)

## Antigravity CLI

Antigravity CLI is the successor to Gemini CLI. See the [migration guide](https://antigravity.google/docs/gcli-migration).

### Setup

```bash
./extra/setup-antigravity-cli.sh [target-directory]
```

`target-directory` is the project directory where `.agents/skills/` will be created. If omitted, the plugin repository root is used.

Non-interactive mode (overwrites without confirmation):

```bash
./extra/setup-antigravity-cli.sh -y [target-directory]
```

User scope (installs to `~/.gemini/antigravity-cli/skills/`, ignores target-directory):

```bash
./extra/setup-antigravity-cli.sh --user
```

### What the script does

- Creates `.agents/skills/` directory structure (project scope) or `~/.gemini/antigravity-cli/skills/` (user scope)
- Copies skills directly into the skills directory
- Skills double as slash commands; no separate commands handling is needed
- Writes MCP config as a standalone `~/.gemini/config/mcp_config.json` (user scope) or `.agents/mcp_config.json` (project scope)

### Notes

- Skills are invoked as slash commands directly (e.g. `/st-init`)
- MCP config is a standalone JSON file, not merged into `settings.json`

## Gemini CLI *(obsolete)*

> **Note:** Gemini CLI has been replaced by [Antigravity CLI](#antigravity-cli). Use `setup-antigravity-cli.sh` for new setups. `setup-gemini.sh` is kept for existing installations.

### Setup

```bash
./extra/setup-gemini.sh [target-directory]
```

`target-directory` is the project directory where `.agents/skills/` will be created. If omitted, the plugin repository root is used.

Non-interactive mode (overwrites without confirmation):

```bash
./extra/setup-gemini.sh -y [target-directory]
```

User scope (installs to `$HOME`, ignores target-directory):

```bash
./extra/setup-gemini.sh --user
```

### What the script does

- Creates `.agents/skills/` directory structure
- Copies skills directly into `.agents/skills/`
- Copies each command as `.agents/skills/<command-name>/SKILL.md`
- Merges MCP config into `~/.gemini/settings.json`

### Notes

- Gemini CLI does not support custom commands; commands are placed as skills instead
- MCP config uses the same `mcpServers` JSON format as `.mcp.json`

## Limitations

These setup scripts provide a simplified integration. Compared to the native Claude Code plugin experience, the following differences apply:

- **No plugin lifecycle management** - Updates require re-running the setup script
- **Hook support varies** - Each agent has its own hook mechanism; only Cursor has a hook script included
