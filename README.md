# smalltalk-dev Plugin

[Claude Code](https://github.com/anthropics/claude-code) plugin for AI-driven Smalltalk (Pharo/Squeak) development.

> Also available for [Cursor, Devin Desktop, Devin CLI, Antigravity, Antigravity CLI, GitHub Copilot CLI, OpenCode, Kilo Code, Codex CLI, and Gemini CLI](doc/setup-other-agents.md).

## Overview

This plugin provides a comprehensive AI-powered toolkit for Smalltalk development (Pharo or Squeak). It covers the full development lifecycle — from project setup and Tonel file editing, to importing, testing, debugging, and documentation generation.

## Features

- **Skills**: All capabilities are implemented as skills — `st-*` skills are user-invoked commands; other skills provide AI-powered development workflow, debugging expertise, and documentation
- **MCP Integration**: Seamless connection to a Smalltalk image (Pharo or Squeak) and validation servers
- **Hooks**: Automatic suggestions after file changes

## Usage

### Quick Start

The easiest way to use this plugin is to use the **/st-buddy** command:

```bash
# Start Smalltalk Buddy (once per session)
/st-buddy

# Then ask questions naturally
You: "I want to create a Person class with name and age"
AI:  I'll help you create that! [Creates Tonel files and guides you through the process]

You: "How do I test this?"
AI:  Let me run the tests for you... [Executes tests and shows results]

You: "The test failed, can you help?"
AI:  I'll debug this... [Investigates, identifies issue, and fixes it]
```

> https://github.com/user-attachments/assets/7d27ac29-e696-4a7b-8c71-d1404f67c67a

**/st-buddy** is your friendly development partner that:
- Understands what you want to do and routes to the right tools
- Guides you through development, testing, and debugging
- Helps you learn AI-assisted Smalltalk development
- Works naturally through conversation

### Development Workflow

1. **Run /st-buddy** once at the start of your session
2. **Ask questions** naturally about what you want to do
3. **AI implements** and manages the workflow (editing Tonel, importing, testing)
4. **Review results** and continue the conversation
5. **Iterate** until you're satisfied

For experienced users who prefer direct commands, see [Commands.md](doc/Commands.md).

## Prerequisites

### 1. A Smalltalk Image with a Smalltalk Interop Server

Choose Pharo or Squeak, then one of the following options:

#### Option A: Use Docker (Easy, Pharo only)

Run a pre-configured Pharo image using [smalltalk-interop-docker](https://github.com/mumez/smalltalk-interop-docker):

```bash
docker compose up -d
```

#### Option B: Local Setup

Install the Interop Server that matches your Smalltalk dialect:

- Pharo: [PharoSmalltalkInteropServer](https://github.com/mumez/PharoSmalltalkInteropServer)
- Squeak: [SqueakSmalltalkInteropServer](https://github.com/mumez/SqueakSmalltalkInteropServer)

### 2. Claude Code

Install [Claude Code](https://github.com/anthropics/claude-code).

> **Other AI agents**: If you use Cursor, Devin Desktop, Devin CLI, Antigravity, Antigravity CLI, GitHub Copilot CLI, OpenCode, Kilo Code, Codex CLI, or Gemini CLI instead of Claude Code, see [Other AI Agents Setup Guide](doc/setup-other-agents.md).

### 3. uv

Install [uv](https://docs.astral.sh/uv/), which is used in MCP.

## Installation

### Option 1: Install from GitHub (Recommended)

```bash
# Add marketplace from GitHub
claude plugin marketplace add mumez/smalltalk-dev-plugin

# Install the plugin
claude plugin install smalltalk-dev
```

### Option 2: Local Development

For local testing and development:

```bash
# Clone the repository
git clone https://github.com/mumez/smalltalk-dev-plugin.git
cd smalltalk-dev-plugin

# Add local directory as marketplace
claude plugin marketplace add ./

# Install the plugin
claude plugin install smalltalk-dev
```

### Option 3: Universal Install with APM

For users working with multiple AI agents, [APM](https://microsoft.github.io/apm/) provides a unified install across agents:

```bash
apm install -g mumez/smalltalk-dev-plugin --target claude,copilot
```

> **Note**: Requires [APM](https://microsoft.github.io/apm/) to be installed. If you only use Claude Code, Option 1 is simpler.

### Verify Installation

After installation, you should see the custom commands starting with `/st-`.

### Commands (User-invoked Skills)

All commands are implemented as `st-*` skills. **Most users should start with `/st-buddy`** — it guides you and invokes other commands as needed.

- `/st-buddy` - Friendly development assistant (recommended starting point)
- `/st-init` - Load smalltalk-developer skill and explain workflow
- `/st-setup-project` - Set up Smalltalk project structure
- `/st-eval` - Execute Smalltalk code snippets
- `/st-lint` - Check code quality and best practices
- `/st-import` - Import Tonel packages to the Smalltalk image
- `/st-export` - Export packages from the Smalltalk image
- `/st-test` - Run SUnit tests
- `/st-validate` - Validate Tonel syntax

For command details and advanced usage, see [Commands.md](doc/Commands.md).

### Background Skills

Specialized skills that activate automatically based on context (also usable directly):

- **smalltalk-developer** - Development workflow and best practices
- **smalltalk-debugger** - Error handling and debugging procedures
- **smalltalk-usage-finder** - Code usage exploration and analysis
- **smalltalk-implementation-finder** - Implementation discovery and patterns
- **smalltalk-commenter** - CRC-style class documentation generation

**These skills work behind the scenes** when you use /st-buddy, providing specialized knowledge for each task.

#### /smalltalk-commenter (Documentation Skill)

Automatically suggests class documentation improvements:

- Detects undocumented classes after file changes (occasionally, not every time)
- Generates CRC-style class comments
- Can be invoked directly: `/smalltalk-commenter`

**Works quietly in the background** - you'll get occasional suggestions to improve documentation.

### MCP Tools

The plugin exposes all tools from both MCP servers:

**smalltalk-interop** (22 tools):
- `eval`: Execute Smalltalk expressions
- `import_package`, `export_package`: Package management
- `run_class_test`, `run_package_test`: Test execution
- `get_class_source`, `get_method_source`: Code inspection
- `search_implementors`, `search_references`: Code navigation
- And more...

**smalltalk-validator** (5 tools):
- `validate_tonel_smalltalk_from_file`: File validation
- `validate_tonel_smalltalk`: Content validation
- `validate_smalltalk_method_body`: Method validation
- `lint_tonel_smalltalk_from_file`: File linting
- `lint_tonel_smalltalk`: Content linting

## Configuration

The plugin uses two MCP servers:

1. **smalltalk-interop**: Communication with Smalltalk image (Pharo or Squeak)
2. **smalltalk-validator**: Tonel syntax validation

These are configured automatically via `.mcp.json`. You can customize Smalltalk (SisServer is running) port:

```bash
export SIS_PORT=8086  # default
```

## Uninstallation

### Uninstall Plugin

```bash
# Remove the plugin
claude plugin uninstall smalltalk-dev
```

### Remove Marketplace (Optional)

If you also want to remove the marketplace entry:

```bash
claude plugin marketplace remove smalltalk-dev-marketplace
```

**Note**: The marketplace name depends on how you added it. Use `claude plugin marketplace list` to see the exact name.

### Clean Reinstall (Local Development)

When developing locally and need to test changes:

```bash
# Uninstall current version
claude plugin uninstall smalltalk-dev

# Remove marketplace
claude plugin marketplace remove smalltalk-dev-marketplace

# Re-add marketplace
claude plugin marketplace add ./

# Reinstall plugin
claude plugin install smalltalk-dev
```


## Troubleshooting

### "Connection refused" error

Make sure the Smalltalk Interop Server (PharoSmalltalkInteropServer or SqueakSmalltalkInteropServer) is running:

```smalltalk
SisServer current start.
SisServer current.  "Should show running server"
```

### "Package not found" after import

- Verify absolute path is correct
- Check that .st files are in correct directory
- Ensure package.st exists

### Tests fail after import

1. Check test error message
2. Use `/st-validate` to check syntax
3. Use `/st-eval` to debug specific code
4. Fix in Tonel file
5. Re-import and re-test

### Import seems to do nothing

- Check the Smalltalk image's Transcript for error messages
- Verify server port matches configuration: `SisServer teapotConfig`
- Try `/st-eval Smalltalk version` to test connection

## Project Structure

```
smalltalk-dev-plugin/
├── .claude-plugin/
│   ├── plugin.json          # Plugin metadata
│   └── marketplace.json     # Marketplace configuration
├── .mcp.json                # MCP server configuration
├── skills/
│   ├── st-buddy/            # /st-buddy - Friendly development assistant
│   ├── st-init/             # /st-init - Start development session
│   ├── st-setup-project/    # /st-setup-project - Project boilerplate
│   ├── st-eval/             # /st-eval - Execute Smalltalk code
│   ├── st-lint/             # /st-lint - Check code quality
│   ├── st-import/           # /st-import - Import Tonel packages
│   ├── st-export/           # /st-export - Export packages
│   ├── st-test/             # /st-test - Run SUnit tests
│   ├── st-validate/         # /st-validate - Validate Tonel syntax
│   ├── smalltalk-commenter/
│   │   └── SKILL.md         # Documentation specialist skill
│   ├── smalltalk-developer/
│   │   ├── SKILL.md         # Development workflow
│   │   ├── examples/        # Development sessions
│   │   └── references/      # Best practices, Tonel format
│   ├── smalltalk-debugger/
│   │   ├── SKILL.md         # Debugging techniques
│   │   ├── examples/        # Debug scenarios
│   │   └── references/      # Error patterns, inspection, UI debugging
│   ├── smalltalk-usage-finder/
│   │   ├── SKILL.md         # Usage exploration
│   │   ├── examples/        # Usage scenarios
│   │   └── references/      # Usage analysis
│   └── smalltalk-implementation-finder/
│       ├── SKILL.md         # Implementation analysis
│       ├── examples/        # Implementation scenarios
│       └── references/      # Implementation analysis
├── hooks/
│   ├── hooks.json                   # PostToolUse hook configuration
│   └── suggest-class-comment.py    # Hook script for documentation suggestions
├── extra/
│   ├── setup-cursor.sh              # Setup script for Cursor
│   ├── setup-devin-desktop.sh       # Setup script for Devin Desktop
│   ├── setup-devin-cli.sh           # Setup script for Devin CLI
│   ├── setup-antigravity.sh         # Setup script for Antigravity
│   ├── setup-copilot.sh             # Setup script for GitHub Copilot CLI
│   ├── setup-opencode.sh            # Setup script for OpenCode
│   ├── setup-kilocode.sh            # Setup script for Kilo Code
│   ├── setup-codex.sh               # Setup script for Codex CLI
│   ├── setup-antigravity-cli.sh     # Setup script for Antigravity CLI (successor to Gemini CLI)
│   ├── setup-gemini.sh              # Setup script for Gemini CLI (obsolete)
│   ├── opencode.json                # MCP config for OpenCode (also used to derive Kilo Code's config)
│   └── suggest-class-comment_cursor.sh # Hook script for Cursor
├── doc/
│   ├── Commands.md                  # Commands quick reference
│   └── setup-other-agents.md       # Setup guide for other AI agents
├── README.md                # This file
└── claudedocs/
    └── test-scenarios.md    # Testing documentation
```

## Presentation

- [Introducing smalltalk-dev Plugin](https://mumez.github.io/smalltalk-dev-plugin-slides/introducing-smalltalk-dev-plugin-en.html)

## Examples

Projects built with this plugin:

- [smalltalk-dev-plugin-money-example](https://github.com/mumez/smalltalk-dev-plugin-money-example) - Multi-currency Money class with arithmetic operations and exchange rate conversion
- [smalltalk-dev-plugin-graph-example](https://github.com/mumez/smalltalk-dev-plugin-graph-example) - Directed weighted graph with Dijkstra's shortest-path algorithm
- [smalltalk-dev-plugin-gui-example](https://github.com/mumez/smalltalk-dev-plugin-gui-example) - Interactive to-do list app built with the Spec2 framework

## For GUI Users

Prefer a GUI over the CLI? Check out [pharo-agentic-browser](https://github.com/mumez/pharo-agentic-browser) — it lets you use an agent with this plugin installed directly from Pharo's native UI.

## Contributing

Contributions are welcome! Please:

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Test with Claude Code
5. Submit a pull request

## License

MIT License - see LICENSE file for details

## Links

- **MCP Servers**:
  - [smalltalk-interop-mcp-server](https://github.com/mumez/smalltalk-interop-mcp-server) by mumez
  - [smalltalk-validator-mcp-server](https://github.com/mumez/smalltalk-validator-mcp-server) by mumez
- **Claude Code**: [Anthropic](https://www.anthropic.com/)
- **Pharo**: [Pharo Project](https://pharo.org/)
- **Squeak**: [Squeak Project](https://squeak.org/)
