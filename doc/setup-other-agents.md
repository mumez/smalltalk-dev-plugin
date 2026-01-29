# Other AI Agents Setup Guide

This plugin is designed for Claude Code, but can also be used with other AI agents via setup scripts provided in the `extra/` directory. Currently supported:

- **Cursor**
- **Windsurf**
- **Antigravity**

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
- Copies commands with `st-` prefix (e.g., `init.md` -> `st-init.md`)
- Copies skills and agents
- Copies `.mcp.json` as `.cursor/mcp.json`
- Creates `hooks.json` for `afterFileEdit` event
- Installs hook script for class comment suggestions

### Notes

- Cursor uses the filename as the command name, so commands are prefixed with `st-` instead of using the `st:` namespace
- Command references in files are automatically rewritten from `/st:name` to `/st-name`
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
- Copies commands as prompt files
- Generates workflow files for each command and agent
- Copies MCP config to `~/.codeium/windsurf/mcp_config.json`
  - On WSL2, uses the Windows-side path (`%USERPROFILE%\.codeium\windsurf\`)

### Notes

- Workflows are generated as entry points that reference the prompt/agent files
- Command references are rewritten from `/st:name` to `/st-name`
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
- Copies commands as prompt files
- Generates workflow files for each command and agent
- Copies MCP config to `~/.gemini/antigravity/mcp_config.json`
  - On WSL2, uses the Windows-side path (`%USERPROFILE%\.gemini\antigravity\`)

### Notes

- Workflows are generated as entry points that reference the prompt/agent files
- Command references are rewritten from `/st:name` to `/st-name`

## Limitations

These setup scripts provide a simplified integration. Compared to the native Claude Code plugin experience, the following differences apply:

- **No plugin lifecycle management** - Updates require re-running the setup script
- **Hook support varies** - Each agent has its own hook mechanism; only Cursor has a hook script included
- **Command format** - Commands use `/st-name` format instead of `/st:name`
