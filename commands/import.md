---
name: st:import
description: Import Tonel package to Pharo image
---

# Import Tonel Package

Import edited Tonel files into running Pharo image.

## Usage

```bash
/st:import MyPackage /home/user/project/src
/st:import MyPackage-Tests /home/user/project/src
/st:import MyPackage  # Uses current directory
```

## Implementation

Uses `import_package` from pharo-interop MCP server.
Always use absolute paths for reliability.

## Notes

- Re-import after every change
- Import main package before test package
- Use absolute paths only

## Examples

```bash
# Import main package
/st:import RediStick-Json /home/user/git/RediStick/src

# Import test package
/st:import RediStick-Json-Tests /home/user/git/RediStick/src
```

## MCP Tool Call

```
mcp__smalltalk-interop__import_package: 'PackageName' path: '/absolute/path/to/src'
```
