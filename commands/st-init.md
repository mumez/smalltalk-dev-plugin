---
name: st-init
description: Start Smalltalk development session - loads smalltalk-developer skill and explains the development workflow
allowed-tools:
  - Skill
  - Bash
  - mcp__smalltalk-interop__eval
---

# Initialize Smalltalk Development Session

Start a new Pharo Smalltalk development session by loading the `smalltalk-developer` skill and explaining the standard development workflow.

## Implementation

1. **Check for project structure** - Before anything else, verify `.project` file or `src/` directory exists
2. **Load smalltalk-developer skill** - Use the `Skill` tool
3. **Verify Pharo connection** - Run `Smalltalk version` via eval
4. **Present workflow overview** - Explain the Edit → Import → Test cycle
5. **List available commands**

### Project Detection

```bash
if [ ! -f ".project" ] && [ ! -d "src" ]; then
  echo "No Pharo project structure detected in current directory"
  echo "Run /st-setup-project MyProjectName to create one, or continue with initialization?"
  exit 0
fi
```

- ✅ **Project exists**: `.project` OR `src/` found → continue normally
- ⚠️ **No project**: neither found → show setup recommendation and pause

## Expected Output

### If No Project Structure Exists

```
⚠️  No Pharo project structure detected in current directory

Run /st-setup-project MyProjectName to set up:
  • .project configuration file
  • src/ directory with package structure
  • BaselineOf class and Core/Tests packages

Would you like to run /st-setup-project now, or continue with initialization?
```

### If Project Structure Exists

- ✅ Smalltalk developer skill loaded
- ✅ Pharo connection verified (or error if not connected)
- 📚 Development workflow explanation
- 📋 Available commands list

## Development Workflow

### Edit → Lint → Import → Test → Iterate

```bash
/st-lint PackageName              # Check Smalltalk best practices
/st-import PackageName /abs/path  # Import Tonel files to Pharo
/st-test PackageNameTest          # Run SUnit tests
/st-eval YourClass new someMethod # Debug as needed
```

- Edit `.st` Tonel files in your editor (source of truth)
- Fix issues in Tonel files, re-lint, re-import, repeat until tests pass

## Available Commands

- **`/st-import`** - Import Tonel package to Pharo
- **`/st-test`** - Run SUnit tests
- **`/st-eval`** - Execute Smalltalk code for debugging
- **`/st-export`** - Export package from Pharo (when needed)
- **`/st-lint`** - Validate Smalltalk best practices

## Connection Verification

Run `Smalltalk version` via eval. If it fails:

1. Check if Docker is available: `docker --version`

### Docker Fallback Flow

If connection fails and Docker is available, generate `compose.yml`:

```yaml
services:
  sis-pharo:
    image: mumez/smalltalk-interop-docker
    ports:
      - "5900:5900"
      - "6901:6901"
      - "8086:8086"
    environment:
      PHARO_SIS_PORT: 8086
    volumes:
      - /tmp:/root/screenshots
      - .:/root/repos
```

Then instruct the user to run `docker compose up -d` and re-test the connection.

If Docker is NOT available, instruct the user to:
1. Start PharoSmalltalkInteropServer in their Pharo image
2. Verify the port configuration (default: 8086)

## Related Skills

This command loads **smalltalk-developer**. Other available skills:
- **smalltalk-debugger** - Activates when tests fail or errors occur
- **smalltalk-usage-finder** - For understanding how to use classes
- **smalltalk-implementation-finder** - For analyzing method implementations

## Troubleshooting

1. **Docker** (recommended): Generate `compose.yml` and run `docker compose up -d`
2. **Pharo not running**: Start your Pharo image
3. **Server not started**: Execute in Pharo: `SisServer current start`
4. **Port mismatch**: Check `PHARO_SIS_PORT` env var (default: 8086)
5. **MCP issues**: Verify `pharo-smalltalk-interop-mcp-server` is installed

## Notes

- `/st-init` is primarily for getting started or refreshing your understanding
- The smalltalk-developer skill will activate automatically when working with Smalltalk
