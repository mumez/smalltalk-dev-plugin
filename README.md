# smalltalk-dev Plugin

Claude Code plugin for AI-driven Smalltalk (Pharo) development.

## Overview

This plugin provides a minimal, practical toolkit for Smalltalk development using AI editors. It focuses on the essential workflow of editing Tonel files, importing them into Pharo, and running tests.

## Features

- **Commands**: Essential slash commands for import, test, eval, and validation
- **Skills**: AI-powered development workflow and debugging expertise
- **MCP Integration**: Seamless connection to Pharo and validation servers
- **Hooks**: Automatic suggestions after file changes

## Usage

### Development Workflow

This plugin enables a natural conversation-based workflow with AI:

1. **Describe what you want**: "Create a Person class with name and age in Pharo Smalltalk"
2. **AI implements**: Claude edits Tonel files and suggests import
3. **Import to Pharo**: Confirm or run `/st:import PackageName /path`
4. **Request testing**: "Test the Person class" or `/st:test PersonTest`
5. **Debug if needed**: "The test failed, debug it" - AI uses `/st:eval` to investigate
6. **Iterate**: Continue conversation to refine implementation

**Example conversation:**
```
You: "I need a JSON parser for Redis responses in Pharo"
AI:  Creates Tonel files, suggests /st:import RediStick-Json /home/user/project/src

You: "Import and test it"
AI:  Runs import, then /st:test RsJsonTest

You: "Test failed with 'key not found' error"
AI:  Uses /st:eval to debug, identifies issue, fixes Tonel, suggests re-import

You: "Re-import and test again"
AI:  Success! All tests pass.
```

## Prerequisites

### 1. Pharo with PharoSmalltalkInteropServer

Install [PharoSmalltalkInteropServer](https://github.com/mumez/PharoSmalltalkInteropServer) in your Pharo image.

### 2. Claude Code

Install [Claude Code](https://github.com/anthropics/claude-code).

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

Five essential commands for Smalltalk development:

- **`/st:eval`** - Execute Smalltalk code snippets for testing and debugging
- **`/st:import`** - Import Tonel packages into Pharo image
- **`/st:export`** - Export packages from Pharo image to Tonel files
- **`/st:test`** - Run SUnit tests
- **`/st:validate`** - Validate Tonel syntax (rarely needed)

For detailed usage and examples, see [Commands.md](Commands.md).

### Skills

#### smalltalk-developer

Automatically activated when working on Smalltalk development tasks. Provides:

- Standard development workflow guidance
- Import/test command suggestions
- Best practices for Tonel editing
- Path and timing recommendations

**Triggers**: "Smalltalk development", "create class", "add method", "import package", etc.

#### smalltalk-debugger

Automatically activated when debugging is needed. Provides:

- Error handling patterns
- Step-by-step debugging procedures
- Object inspection techniques
- Common error solutions

**Triggers**: "test failed", "error", "debug", "inspect", etc.

### MCP Tools

The plugin exposes all tools from both MCP servers:

**pharo-interop** (22 tools):
- `eval`: Execute Smalltalk expressions
- `import_package`, `export_package`: Package management
- `run_class_test`, `run_package_test`: Test execution
- `get_class_source`, `get_method_source`: Code inspection
- `search_implementors`, `search_references`: Code navigation
- And more...

**smalltalk-validator** (3 tools):
- `validate_tonel_smalltalk_from_file`: File validation
- `validate_tonel_smalltalk`: Content validation
- `validate_smalltalk_method_body`: Method validation

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
# For GitHub marketplace
claude plugin marketplace remove smalltalk-dev-marketplace

# For local development marketplace
claude plugin marketplace remove smalltalk-dev-plugin
```

**Note**: The marketplace name depends on how you added it. Use `claude plugin marketplace list` to see the exact name.

### Clean Reinstall (Local Development)

When developing locally and need to test changes:

```bash
# Uninstall current version
claude plugin uninstall smalltalk-dev

# Remove marketplace
claude plugin marketplace remove smalltalk-dev-plugin

# Re-add marketplace
claude plugin marketplace add ./

# Reinstall plugin
claude plugin install smalltalk-dev
```

## Best Practices

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
│   └── plugin.json          # Plugin metadata
├── .mcp.json                # MCP server configuration
├── commands/
│   ├── eval.md              # /st:eval command
│   ├── import.md            # /st:import command
│   ├── test.md              # /st:test command
│   └── validate.md          # /st:validate command
├── skills/
│   ├── smalltalk-developer/
│   │   └── SKILL.md         # Development workflow
│   └── smalltalk-debugger/
│       └── SKILL.md         # Debugging techniques
├── hooks/
│   └── hooks.json           # Event hooks
├── scripts/
│   ├── suggest-import.sh    # FileChange hook
│   └── check-pharo-connection.sh  # SessionStart hook
└── README.md
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
