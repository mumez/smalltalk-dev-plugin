# smalltalk-dev Plugin

[Claude Code](https://github.com/anthropics/claude-code) plugin for AI-driven Smalltalk (Pharo) development.

> Also available for [Cursor, Windsurf, and Antigravity](doc/setup-other-agents.md).

## Overview

This plugin provides a minimal, practical toolkit for Smalltalk development using AI agents. It focuses on the essential workflow of editing Tonel files, importing them into Pharo, and running tests.

## Features

- **Commands**: Essential slash commands for import, test, eval, and validation
- **Skills**: AI-powered development workflow and debugging expertise
- **Agents**: Autonomous assistants for guidance and documentation
- **MCP Integration**: Seamless connection to Pharo and validation servers
- **Hooks**: Automatic suggestions after file changes

## Usage

### Quick Start

The easiest way to use this plugin is to use the **/st:buddy** command:

```bash
# Start Smalltalk Buddy (once per session)
/st:buddy

# Then ask questions naturally
You: "I want to create a Person class with name and age"
AI:  I'll help you create that! [Creates Tonel files and guides you through the process]

You: "How do I test this?"
AI:  Let me run the tests for you... [Executes tests and shows results]

You: "The test failed, can you help?"
AI:  I'll debug this... [Investigates, identifies issue, and fixes it]
```

> https://github.com/user-attachments/assets/7d27ac29-e696-4a7b-8c71-d1404f67c67a

**/st:buddy** is your friendly development partner that:
- Understands what you want to do and routes to the right tools
- Guides you through development, testing, and debugging
- Helps you learn AI-assisted Smalltalk development
- Works naturally through conversation

### Development Workflow

1. **Run /st:buddy** once at the start of your session
2. **Ask questions** naturally about what you want to do
3. **AI implements** and manages the workflow (editing Tonel, importing, testing)
4. **Review results** and continue the conversation
5. **Iterate** until you're satisfied

For experienced users who prefer direct commands, see [Commands.md](doc/Commands.md).

## Prerequisites

### 1. Pharo with PharoSmalltalkInteropServer

Install [PharoSmalltalkInteropServer](https://github.com/mumez/PharoSmalltalkInteropServer) in your Pharo image.

### 2. Claude Code

Install [Claude Code](https://github.com/anthropics/claude-code).

> **Other AI agents**: If you use Cursor, Windsurf, or Antigravity instead of Claude Code, see [Other AI Agents Setup Guide](doc/setup-other-agents.md).

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

### Verify Installation

After installation, you should see the custom commands starting with `/st:`.

### Commands

The plugin provides essential commands for Smalltalk development:

- `/st:buddy` - Start your friendly Smalltalk development assistant (recommended starting point)
- `/st:init` - Load smalltalk-developer skill and explain workflow
- `/st:setup-project` - Set up Pharo project structure
- `/st:eval` - Execute Smalltalk code snippets
- `/st:lint` - Check code quality and best practices
- `/st:import` - Import Tonel packages to Pharo
- `/st:export` - Export packages from Pharo
- `/st:test` - Run SUnit tests
- `/st:validate` - Validate Tonel syntax

**Most users should start with /st:buddy** - it will guide you and use the other commands as needed.

For command details and advanced usage, see [Commands.md](doc/Commands.md).

### Skills

The plugin includes specialized AI skills that activate automatically based on your needs:

- **smalltalk-developer** - Development workflow and best practices
- **smalltalk-debugger** - Error handling and debugging procedures
- **smalltalk-usage-finder** - Code usage exploration and analysis
- **smalltalk-implementation-finder** - Implementation discovery and patterns

**These skills work behind the scenes** when you use /st:buddy, providing specialized knowledge for each task.

### Agents

#### @smalltalk-commenter (Documentation Assistant)

Automatically suggests class documentation improvements:

- Detects undocumented classes after file changes (occasionally, not every time)
- Generates CRC-style class comments
- Can be invoked directly: "document my classes"

**Works quietly in the background** - you'll get occasional suggestions to improve documentation.

### MCP Tools

The plugin exposes all tools from both MCP servers:

**pharo-interop** (22 tools):
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

1. **pharo-interop**: Communication with Pharo image
2. **smalltalk-validator**: Tonel syntax validation

These are configured automatically via `.mcp.json`. You can customize the Pharo port:

```bash
export PHARO_SIS_PORT=8086  # default
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

## Best Practices

### Development Workflow
1. **Edit** Tonel files (AI editor is the source of truth)
2. **Lint** code with `/st:lint` to check quality
3. **Import** to Pharo with absolute paths
4. **Test** after every import

### Code Quality
- Use `/st:lint` before importing to catch issues early
- Add class prefixes to avoid name collisions
- Keep methods focused (15 lines standard, 40 for UI/tests)
- Limit instance variables (max 10 per class)
- Access instance variables through methods, not directly

### Path Management
- Always use absolute paths for imports
- Import multiple packages individually

### File Editing
- **AI editor is the source of truth** (Tonel files)
- Avoid editing in Pharo directly
- Use `export_package` only when necessary

### Import Timing
- Re-import after every change
- Import main package before test package
- Don't forget to import test packages

### Testing
- Run tests after every import
- Use `run_class_test` for specific classes
- Use `run_package_test` for full packages

### Debugging
- Use `/st:eval` for quick partial execution
- Capture both results and errors in Array
- Use `printString` for object serialization
- Debug step-by-step

## Troubleshooting

### "Connection refused" error

Make sure PharoSmalltalkInteropServer is running:

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
2. Use `/st:validate` to check syntax
3. Use `/st:eval` to debug specific code
4. Fix in Tonel file
5. Re-import and re-test

### Import seems to do nothing

- Check Pharo Transcript for error messages
- Verify server port matches configuration: `SisServer teapotConfig`
- Try `/st:eval Smalltalk version` to test connection

## Project Structure

```
smalltalk-dev-plugin/
├── .claude-plugin/
│   ├── plugin.json          # Plugin metadata
│   └── marketplace.json     # Marketplace configuration
├── .mcp.json                # MCP server configuration
├── agents/
│   └── smalltalk-commenter.md # Documentation specialist agent
├── commands/
│   ├── buddy.md             # /st:buddy - Friendly development assistant
│   ├── init.md              # /st:init - Start development session
│   ├── setup-project.md     # /st:setup-project - Project boilerplate
│   ├── eval.md              # /st:eval - Execute Smalltalk code
│   ├── import.md            # /st:import - Import Tonel packages
│   ├── export.md            # /st:export - Export packages
│   ├── test.md              # /st:test - Run SUnit tests
│   ├── lint.md              # /st:lint - Check code quality
│   └── validate.md          # /st:validate - Validate Tonel syntax
├── skills/
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
│   └── hooks.json           # PostToolUse hook configuration
├── scripts/
│   └── suggest-class-comment.sh     # PostToolUse hook for documentation suggestions
├── extra/
│   ├── setup-cursor.sh              # Setup script for Cursor
│   ├── setup-windsurf.sh            # Setup script for Windsurf
│   ├── setup-antigravity.sh         # Setup script for Antigravity
│   └── suggest-class-comment_cursor.sh # Hook script for Cursor
├── doc/
│   ├── Commands.md                  # Commands quick reference
│   └── setup-other-agents.md       # Setup guide for other AI agents
├── README.md                # This file
└── claudedocs/
    └── test-scenarios.md    # Testing documentation
```

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
  - [pharo-smalltalk-interop-mcp-server](https://github.com/mumez/pharo-smalltalk-interop-mcp-server) by mumez
  - [smalltalk-validator-mcp-server](https://github.com/mumez/smalltalk-validator-mcp-server) by mumez
- **Claude Code**: [Anthropic](https://www.anthropic.com/)
- **Pharo**: [Pharo Project](https://pharo.org/)
