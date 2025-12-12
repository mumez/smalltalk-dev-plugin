# smalltalk-dev Plugin

Claude Code plugin for AI-driven Smalltalk (Pharo) development.

## Overview

This plugin provides a minimal, practical toolkit for Smalltalk development using AI editors. It focuses on the essential workflow of editing Tonel files, importing them into Pharo, and running tests.

## Features

- **Commands**: Essential slash commands for import, test, and validation
- **Skills**: AI-powered development workflow and debugging expertise
- **MCP Integration**: Seamless connection to Pharo and validation servers
- **Hooks**: Automatic suggestions after file changes

## Prerequisites

### 1. Pharo with PharoSmalltalkInteropServer

Install [PharoSmalltalkInteropServer](https://github.com/mumez/PharoSmalltalkInteropServer) in your Pharo image:

### 2. Claude Code

Install [Claude Code](https://github.com/anthropics/claude-code):

## Installation

### Option 1: Install from GitHub (Recommended)

```bash
claude plugin marketplace add mumez/smalltalk-dev-plugin
claude plugin install smalltalk-dev
```

### Option 2: Local Development

```bash
# Clone the repository
git clone https://github.com/mumez/smalltalk-dev-plugin.git

# Add as local marketplace
claude plugin marketplace add ./smalltalk-dev-plugin

# Install the plugin
claude plugin install smalltalk-dev
```

## Configuration

The plugin uses two MCP servers:

1. **pharo-interop**: Communication with Pharo image
2. **smalltalk-validator**: Tonel syntax validation

These are configured automatically via `.mcp.json`. You can customize the Pharo port:

```bash
export PHARO_SIS_PORT=8086  # default
```

## Usage

### Development Workflow

1. **Edit Tonel files** in your AI editor
2. **Import to Pharo**: `/st:import PackageName /absolute/path/to/src`
3. **Run tests**: `/st:test TestClassName`
4. **Debug if needed** using `eval` tool
5. **Repeat**

### Commands

#### `/st:import [PackageName] [path]`

Import Tonel package into running Pharo image.

```bash
/st:import MyPackage /home/user/project/src
/st:import MyPackage-Tests /home/user/project/src
```

#### `/st:test [TestClass|PackageName]`

Run SUnit tests.

```bash
/st:test MyTestClass
/st:test MyPackage-Tests
```

#### `/st:validate [file_path]` (Optional)

Validate Tonel syntax before import.

```bash
/st:validate /path/to/file.st
```

Note: Modern AI usually generates correct Tonel, so this is rarely needed.

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

## Example Session

```
User: "Create a Person class with name and age accessors"

Claude (smalltalk-developer skill):
1. Creates src/MyPackage/Person.st:
   Class {
       #name : #Person,
       #superclass : #Object,
       #instVars : ['name', 'age'],
       #category : #'MyPackage'
   }
   
   { #category : #accessing }
   Person >> name [ ^ name ]
   
   { #category : #accessing }
   Person >> name: aString [ name := aString ]
   
   { #category : #accessing }
   Person >> age [ ^ age ]
   
   { #category : #accessing }
   Person >> age: anInteger [ age := anInteger ]

2. Suggests: /st:import MyPackage /home/user/project/src
3. Suggests: /st:test PersonTest

User: "Run the import and test"

Claude:
1. Executes: mcp__smalltalk-interop__import_package: 'MyPackage' path: '/home/user/project/src'
2. Executes: mcp__smalltalk-interop__run_class_test: 'PersonTest'
3. Reports test results

User: "The test failed with MessageNotUnderstood"

Claude (smalltalk-debugger skill):
1. Analyzes error message
2. Uses eval to debug:
   | result |
   result := Array new: 2.
   [ | person |
     person := Person new.
     person name: 'John'.
     result at: 1 put: person name.
   ] on: Error do: [:ex | result at: 2 put: ex description].
   ^ result
3. Identifies issue
4. Suggests fix in Person.st
5. Suggests re-import and re-test
```

## Best Practices

### Path Management
- Avoid relative paths
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
- Use `eval` for partial execution
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
3. Use `eval` to debug specific code
4. Fix in Tonel file
5. Re-import and re-test

### Import seems to do nothing

- Check Pharo Transcript for error messages
- Verify server port matches configuration: `SisServer teapotConfig`
- Try `eval: 'Smalltalk version'` to test connection

## Project Structure

```
smalltalk-dev-plugin/
├── .claude-plugin/
│   └── plugin.json          # Plugin metadata
├── .mcp.json                # MCP server configuration
├── commands/
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
