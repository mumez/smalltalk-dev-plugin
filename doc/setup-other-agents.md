# Other AI Agents Setup Guide

This plugin is designed for Claude Code, but can also be used with other AI agents.

## Install with APM

[APM](https://microsoft.github.io/apm/) provides a unified install across agents without running individual setup scripts:

```bash
apm install -g mumez/smalltalk-dev-plugin --target claude,copilot,kiro
```

Replace `claude,copilot,kiro` with the agents you use. To list all supported targets, run:

```bash
apm targets
```

See the [APM documentation](https://microsoft.github.io/apm/) for details.

> **Note:** APM is updated frequently, but installation may still be incomplete for some targets. If that happens, use the manual setup scripts below instead.

## Manual Setup with Scripts

Alternatively, setup scripts are provided in the `extra/` directory. Currently supported:

- [Cursor](https://cursor.com/)
- [Devin Desktop](https://devin.ai/desktop/) *(formerly Windsurf)*
- [Devin CLI](https://devin.ai/cli)
- [Antigravity](https://antigravity.google/)
- [Antigravity CLI](https://antigravity.google/) *(successor to Gemini CLI)*
- [GitHub Copilot CLI](https://github.com/features/copilot/cli)
- [OpenCode](https://opencode.ai/)
- [Kilo Code](https://kilo.ai/)
- [Codex CLI](https://github.com/openai/codex)
- [Gemini CLI](https://geminicli.com/) *(obsolete — use Antigravity CLI)*

## Prerequisites

- A Smalltalk image with the matching Interop Server installed:
  - [Pharo](https://pharo.org/) with [PharoSmalltalkInteropServer](https://github.com/mumez/PharoSmalltalkInteropServer), or
  - [Squeak](https://squeak.org/) with [SqueakSmalltalkInteropServer](https://github.com/mumez/SqueakSmalltalkInteropServer)
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

## Devin Desktop

### Setup

```bash
./extra/setup-devin-desktop.sh [target-directory]
```

Non-interactive mode:

```bash
./extra/setup-devin-desktop.sh -y [target-directory]
```

### What the script does

- Creates `.devin/` directory structure (skills, workflows, prompts, agents)
- Copies skills and agents
- Copies commands as prompt files and generates workflow files for each
- Copies `st-*` skills (e.g. `st-init`, `st-setup-project`) as additional prompt files and generates workflow files for each
- Copies MCP config to `~/.codeium/windsurf/mcp_config.json`
  - On WSL2, uses the Windows-side path (`%USERPROFILE%\.codeium\windsurf\`)
  - This path is unchanged from Devin Desktop's Windsurf predecessor; it has not yet been officially migrated

### Notes

- Workflows are generated as entry points that reference the prompt/agent files
- Restart Devin Desktop after setup

## Devin CLI

### Setup

```bash
./extra/setup-devin-cli.sh [target-directory]
```

`target-directory` is the project directory where `.agents/skills/` and `.devin/config.json` will be created. If omitted, the plugin repository root is used.

Non-interactive mode (overwrites without confirmation):

```bash
./extra/setup-devin-cli.sh -y [target-directory]
```

User scope (installs to `~/.agents/skills/` and `~/.config/devin/config.json`, ignores target-directory):

```bash
./extra/setup-devin-cli.sh --user
```

### What the script does

- Creates `.agents/skills/` directory structure (project scope) or `~/.agents/skills/` (user scope)
- Copies skills directly into the skills directory
- Skills double as slash commands; no separate commands handling is needed
- Merges MCP config into `.devin/config.json` (project scope) or `~/.config/devin/config.json` (user scope)

### Notes

- Skills are invoked as slash commands directly (e.g. `/st-init`)
- MCP config uses the same `mcpServers` JSON format as `.mcp.json`, merged alongside any other existing keys in `config.json`

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

## Kilo Code

### Setup

```bash
./extra/setup-kilocode.sh [target-directory]
```

`target-directory` is the project directory where `.agents/skills/` will be created. If omitted, the plugin repository root is used.

Non-interactive mode (overwrites without confirmation):

```bash
./extra/setup-kilocode.sh -y [target-directory]
```

User scope (installs to `$HOME`, ignores target-directory):

```bash
./extra/setup-kilocode.sh --user
```

### What the script does

- Creates `.agents/skills/` and copies all skills (including `st-*` skills) directly into it — Kilo Code has no separate commands mechanism
- Copies MCP config to `~/.config/kilo/kilo.json`
  - If `kilo.json` already exists and `jq` is available, merges the `mcp` section automatically (backup created as `kilo.json.bak`)
  - If `jq` is not available, prints the `mcp` section for manual merging

### Notes

- Kilo Code is OpenCode-derived; the MCP config format matches OpenCode's (`command` as an array, `environment` instead of `env`, `type: "local"` required) but with `$schema` set to `https://app.kilo.ai/config.json`
- The MCP config is generated at setup time from `extra/opencode.json` (swapping only the `$schema` value), so the two configs don't need to be maintained separately
- The `kilo.json` config file location (`~/.config/kilo/kilo.json`) is fixed regardless of project or user scope
- Restart Kilo Code after setup to recognize the new configuration

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
