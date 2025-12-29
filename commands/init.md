---
name: st:init
description: Start Smalltalk development session - loads smalltalk-developer skill and explains the development workflow
allowed-tools:
  - Skill
  - mcp__smalltalk-interop__eval
---

# Initialize Smalltalk Development Session

Start a new Pharo Smalltalk development session by loading the `smalltalk-developer` skill and explaining the standard development workflow.

## What This Command Does

1. **Loads smalltalk-developer skill** - Activates AI-driven Smalltalk development assistance
2. **Explains Edit → Import → Test workflow** - Shows the standard development cycle
3. **Verifies Pharo connection** - Checks if PharoSmalltalkInteropServer is running
4. **Introduces available commands** - Lists all `/st:*` commands
5. **Provides quick start guidance** - Helps you begin development immediately

## Usage

```bash
/st:init
```

## Implementation

This command:
1. **Checks for existing project structure** - Verifies if `.project` file or `src/` directory exists
2. **Prompts for setup if needed** - If no project structure found, suggests running `/st:setup-project` first
3. Uses the `Skill` tool to load `smalltalk-developer` skill
4. Runs a connection test using `eval` to verify Pharo is ready
5. Presents the development workflow overview
6. Lists available commands and tools

### Project Detection Logic

Before loading the skill and showing commands, check if a Pharo project exists in the current directory:

```bash
# Check if project structure exists
if [ ! -f ".project" ] && [ ! -d "src" ]; then
  echo "⚠️  No Pharo project structure detected in current directory"
  echo ""
  echo "It looks like you're starting fresh. I recommend setting up a project structure first:"
  echo ""
  echo "  /st:setup-project MyProjectName"
  echo ""
  echo "This will create:"
  echo "  • .project configuration file"
  echo "  • src/ directory with package structure"
  echo "  • BaselineOf class for dependency management"
  echo "  • Core and Tests packages"
  echo ""
  echo "Would you like to run /st:setup-project now, or continue with initialization?"
  exit 0
fi
```

**Detection criteria:**
- ✅ **Project exists**: If `.project` file OR `src/` directory exists → Continue with normal initialization
- ⚠️ **No project**: If neither exists → Show setup-project recommendation and pause

**Important:** This check should happen BEFORE loading the skill and BEFORE the connection test.

## Expected Output

### If No Project Structure Exists

```
⚠️  No Pharo project structure detected in current directory

It looks like you're starting fresh. I recommend setting up a project structure first:

  /st:setup-project MyProjectName

This will create:
  • .project configuration file
  • src/ directory with package structure
  • BaselineOf class for dependency management
  • Core and Tests packages

Would you like to run /st:setup-project now, or continue with initialization?
```

### If Project Structure Exists

After running `/st:init` in a directory with an existing project, you'll see:

- ✅ Smalltalk developer skill loaded
- ✅ Pharo connection verified (or error message if not connected)
- 📚 Development workflow explanation
- 💡 Quick start examples
- 📋 Available commands list

## Development Workflow Overview

The standard Pharo Smalltalk development cycle:

### 1. Edit Tonel Files
- Create or modify `.st` files in your editor
- AI editor is the **source of truth**
- All changes happen in Tonel files first

### 2. Import to Pharo
```bash
/st:import PackageName /absolute/path/to/src
```

### 3. Run Tests
```bash
/st:test PackageNameTest
```

### 4. Debug (if needed)
```bash
/st:eval YourClass new someMethod
```

### 5. Iterate
- Fix issues in Tonel files
- Re-import and re-test
- Repeat until tests pass

## Available Commands

Once initialized, you can use:

- **`/st:import`** - Import Tonel package to Pharo
- **`/st:test`** - Run SUnit tests
- **`/st:eval`** - Execute Smalltalk code for debugging
- **`/st:export`** - Export package from Pharo (when needed)
- **`/st:validate`** - Validate Tonel syntax (optional)

## Quick Start Examples

### Example 1: Create a New Class

```
You: Create a Person class with name and age in Pharo Smalltalk

AI: [Creates Person.st file in Tonel format]
    Suggested: /st:import MyPackage /home/user/project/src

You: /st:import MyPackage /home/user/project/src

AI: ✅ Package imported successfully
    Suggested: Create tests or add methods
```

### Example 2: Debug a Test Failure

```
You: /st:test PersonTest

AI: ❌ Test failed: testFullName
    Error: MessageNotUnderstood: #fullName

You: Debug this error

AI: [smalltalk-debugger skill activates]
    Let me investigate using /st:eval...

    [Finds the issue, suggests fix]
    The Person class is missing the #fullName method.

You: Add the fullName method

AI: [Adds method to Person.st]
    Suggested: /st:import MyPackage /home/user/project/src
```

## Connection Verification

The command will test your Pharo connection by running:

```smalltalk
Smalltalk version
```

If this fails, you'll see instructions to:
1. Start PharoSmalltalkInteropServer in your Pharo image
2. Verify the port configuration (default: 8086)

## When to Use

Use `/st:init` when:
- Starting a new development session
- Beginning work on a Pharo project
- Unsure how to proceed with Smalltalk development
- Need a refresher on the workflow
- Want to verify your environment is ready

## Related Skills

This command loads the **smalltalk-developer** skill, which provides:
- Tonel file editing guidance
- Import/export workflow management
- Test execution patterns
- Best practices for AI-driven Pharo development

Other skills available:
- **smalltalk-debugger** - Activates when tests fail or errors occur
- **smalltalk-usage-finder** - For understanding how to use classes
- **smalltalk-implementation-finder** - For analyzing method implementations

## Notes

- You don't need to run `/st:init` every time - the skill will activate automatically when you work with Smalltalk
- This command is primarily for getting started or refreshing your understanding
- The development workflow applies to all Pharo/Smalltalk projects using Tonel format

## Troubleshooting

If initialization fails:

1. **Pharo not running**: Start your Pharo image
2. **Server not started**: Execute in Pharo:
   ```smalltalk
   SisServer current start
   ```
3. **Port mismatch**: Check `PHARO_SIS_PORT` environment variable (default: 8086)
4. **MCP server issues**: Verify `pharo-smalltalk-interop-mcp-server` is installed
