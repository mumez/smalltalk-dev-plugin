---
name: st:export
description: Export package from Pharo image to Tonel files
---

# Export Package from Pharo

Export a package from the running Pharo image back to Tonel files.

## Usage

```bash
/st:export MyPackage /home/user/project/src
/st:export MyPackage-Tests /home/user/project/src
```

## Implementation

Uses `export_package` from pharo-interop MCP server.

## When to Use

Export is needed when code changes are made **in Pharo** rather than in Tonel files:

1. **Debugger modifications**: Fixed code in Pharo debugger during debugging session
2. **Code generators**: Used Pharo's code generation tools to create new classes/methods
3. **Interactive development**: Made changes directly in Pharo browser
4. **Prototyping**: Experimented with code in Pharo workspace and want to save results

## Important Notes

- **AI editor is the source of truth**: Normally, edit Tonel files and import to Pharo
- **Export is the exception**: Use only when Pharo has newer code than Tonel files
- **Always use absolute paths**: Like import, export requires absolute paths
- **Overwrites Tonel files**: Export will overwrite existing `.st` files in the directory

## Workflow Examples

### Example 1: After Debugger Fix

```
1. Test fails in Pharo
2. Use debugger to fix the issue
3. /st:export MyPackage /home/user/project/src
4. Review the exported changes in Tonel files
5. Continue development with updated Tonel files
```

### Example 2: After Code Generation

```
1. Use Pharo's class builder to generate boilerplate classes
2. /st:export MyPackage /home/user/project/src
3. AI can now see and work with the generated classes
```

### Example 3: Sync After Interactive Development

```
1. Experiment with code in Pharo browser
2. Once satisfied, export to preserve changes:
   /st:export MyPackage /home/user/project/src
3. Tonel files now reflect Pharo image state
```

## Best Practices

- **Export immediately**: After making changes in Pharo, export right away to avoid losing work
- **Review changes**: Check the exported Tonel files to understand what changed
- **Verify export path**: Ensure you're exporting to the correct source directory
- **Consider git diff**: Use git to review what changed after export

## MCP Tool Call

```
mcp__smalltalk-interop__export_package: 'PackageName' path: '/absolute/path/to/src'
```

## Related Commands

- `/st:import` - Import Tonel files to Pharo (normal workflow)
- `/st:test` - Run tests after export to verify changes
