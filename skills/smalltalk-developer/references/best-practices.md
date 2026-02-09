# Smalltalk Development Best Practices

Essential practices for effective Smalltalk development using AI editors and Pharo.

## Path Management

### Always Use Absolute Paths

**Critical Rule**: When importing packages, always use absolute paths.

**Why?**
- Relative paths can fail depending on working directory
- Absolute paths guarantee consistency across sessions
- MCP server needs reliable file system navigation

**Examples:**

✅ **Correct:**
```
import_package: 'MyPackage' path: '/home/user/myproject/src'
```

❌ **Incorrect:**
```
import_package: 'MyPackage' path: './src'
import_package: 'MyPackage' path: '../myproject/src'
```

### Standard Smalltalk Project Structure

Most Smalltalk projects follow these conventions:

1. **Source directories**: Packages are typically located in:
   - `src/` (most common in modern projects)
   - `repositories/` (older projects, pre-Tonel format)
   - `pharo-local/iceberg/...` (Iceberg working copies)

2. **Project metadata**: Check `.project` file in project root:
   **If `.project` does not exist**, create it using Pharo's format (single quotes, tab indentation):
   ```
   {
   	'srcDirectory' : 'src'
   }
   ```
   The `.project` file is **required** for Pharo to locate the source directory. Always create it when setting up a new project structure.

3. **Typical project layout**:
   ```
   MyProject/
   ├── .project           # Contains srcDirectory configuration
   ├── src/              # Source directory (or 'repositories')
   │   ├── MyPackage/
   │   │   ├── MyClass.st
   │   │   └── package.st
   │   └── MyPackage-Tests/
   │       └── ...
   ├── BaselineOfMyProject/
   │   └── BaselineOfMyProject.st
   └── README.md
   ```

### Path Discovery Strategy

Follow this sequence to find the correct source directory:

1. **Check `.project` file** and read `srcDirectory` field
2. If no `.project`, look for `src/` directory
3. Fall back to `repositories/` for older projects
4. Construct absolute path: `<project_root>/<srcDirectory>`

**Example:**
```bash
# If .project says "srcDirectory": "src"
import_package: 'MyPackage' path: '/home/user/MyProject/src'

# If .project says "srcDirectory": "repositories"
import_package: 'MyPackage' path: '/home/user/MyProject/repositories'
```

### Import Multiple Packages Individually

When working with multiple packages, import each one separately:

```
import_package: 'MyPackage-Core' path: '/home/user/project/src'
import_package: 'MyPackage-Utils' path: '/home/user/project/src'
import_package: 'MyPackage-Tests' path: '/home/user/project/src'
```

## Package Dependencies and Import Order

### Understanding Baselines

Package dependencies are defined in `BaselineOf<ProjectName>` classes.

**Baseline location:**
```
src/
├── BaselineOfMyProject/
│   ├── BaselineOfMyProject.st
│   └── package.st
├── MyPackage-Core/
├── MyPackage-Json/
└── MyPackage-Tests/
```

**Dependency definition example:**
```smalltalk
baseline: spec
    <baseline>
    spec for: #common do: [
        spec
            package: 'MyPackage-Core';
            package: 'MyPackage-Json' with: [
                spec requires: #('MyPackage-Core')
            ];
            package: 'MyPackage-Tests' with: [
                spec requires: #('MyPackage-Core' 'MyPackage-Json')
            ]
    ]
```

### Import Order Strategy

1. **Read Baseline**: Find `BaselineOf<ProjectName>` class
2. **Parse dependencies**: Look for `requires:` declarations
3. **Build dependency graph**: Identify which packages depend on others
4. **Import in order**: Dependencies first, then dependent packages

**Example order:** `MyPackage-Core` → `MyPackage-Json` → `MyPackage-Tests`

**Automatic resolution:**
```bash
# AI should check BaselineOfMyProject>>baseline: and determine order
# Then import in correct sequence automatically:
import_package: 'MyPackage-Core' path: '/path/to/src'
import_package: 'MyPackage-Json' path: '/path/to/src'
import_package: 'MyPackage-Tests' path: '/path/to/src'
```

**Note**: Even without explicit user instruction, check the Baseline to determine correct import sequence.

## File Editing Philosophy

### Single Source of Truth

**The AI editor is the source of truth for code.**

- **Primary workflow**: Edit Tonel files in AI editor → Import to Pharo
- **Avoid**: Editing code directly in Pharo image
- **Reason**: Changes in Pharo won't persist in version control

### When to Export from Pharo

Use `export_package` only in these rare cases:

1. **Initial project setup**: Getting existing code from Pharo into Tonel
2. **Emergency recovery**: Recovering work done accidentally in Pharo
3. **Exploring existing code**: Extracting code from a loaded package

**Standard workflow (99% of time):**
```
Edit .st file → Import → Test → Repeat
```

**Rare export workflow:**
```
Work in Pharo → Export → Review .st file → Commit to git
```

### File Modification Workflow

1. **Read current file** to understand existing code
2. **Edit in AI editor** using Edit tool
3. **Import to Pharo** immediately after editing
4. **Test** to verify changes work
5. **Repeat** as needed

## Import Timing

### Re-import After Every Change

**Critical Rule**: Always re-import after editing Tonel files.

**Why?**
- Pharo image doesn't automatically reload files
- Changes in `.st` files are not reflected until imported
- Testing without import means testing old code

### Import Order Best Practices

1. **Main package first**: Import core package before dependencies
2. **Test package last**: Import test packages after main packages
3. **Don't skip test packages**: Common mistake to forget test import

**Example sequence:**
```
1. Edit MyPackage-Core/MyClass.st
2. Import MyPackage-Core
3. Edit MyPackage-Core-Tests/MyClassTest.st
4. Import MyPackage-Core-Tests
5. Run tests
```

### When Changes Don't Appear

If imported code doesn't seem to work:

1. **Verify import succeeded**: Check Pharo Transcript for errors
2. **Check file saved**: Ensure .st file was written to disk
3. **Verify correct path**: Double-check absolute path
4. **Re-import**: Try importing again
5. **Check Pharo**: Inspect class in Pharo to see if it updated

## Test Execution

### Always Test After Import

**Standard workflow:**
```
Edit → Import → Test
```

Never skip the test step. Even small changes can have unexpected effects.

### Test Granularity

**For specific classes:**
```
run_class_test: 'MyClassTest'
```

**For entire packages:**
```
run_package_test: 'MyPackage-Tests'
```

**Choose based on:**
- **Class test**: Fast feedback for focused changes
- **Package test**: Comprehensive validation for wide-ranging changes

### Test-Driven Development

Recommended TDD workflow:

1. **Write test** in `MyClassTest.st`
2. **Import test package**
3. **Run test** (should fail)
4. **Implement feature** in `MyClass.st`
5. **Import main package**
6. **Run test** (should pass)
7. **Refactor** if needed
8. **Re-import and re-test**

## Error Handling and Debugging

### When Tests Fail

1. **Read error message carefully**: Understand what failed
2. **Don't immediately re-import**: Fix the code first
3. **Use `/st:eval` for debugging**: Test snippets before full import
4. **Fix in Tonel file**: Never fix in Pharo directly
5. **Re-import**: After fixing the `.st` file
6. **Re-test**: Verify the fix worked

### Common Import Errors

**"Package not found"**
- Check absolute path is correct
- Verify `package.st` exists in directory
- Ensure package name matches directory name

**"Syntax error"**
- Use `validate_tonel_smalltalk_from_file` before import
- Check for missing brackets, quotes, or periods
- Verify method syntax follows Tonel format

**"Dependency not found"**
- Import dependencies first
- Check Baseline for required packages
- Ensure all referenced classes are available

## Project Organization

### Package Naming Conventions

- **Main package**: `ProjectName-Core` or `ProjectName`
- **Sub-packages**: `ProjectName-Feature` (e.g., `RediStick-Json`)
- **Test packages**: `ProjectName-Tests` or `ProjectName-Feature-Tests`
- **Baseline**: `BaselineOfProjectName`

### Class Naming Conventions

- **Prefix system**: Use 2-3 letter prefix for project (e.g., `Rs` for RediStick)
- **Descriptive names**: `RsConnection`, `RsClient`, `RsJsonSerializer`
- **Test classes**: Add `Test` suffix (e.g., `RsConnectionTest`)

### Method Protocol (Category) Guidelines

Organize methods into logical categories:

- **accessing**: Simple getters and setters
- **operations**: Business logic and computations
- **initialization**: Setup methods
- **private**: Internal helper methods
- **testing**: Boolean query methods (isXxx, hasXxx)
- **printing**: String representation methods

## Version Control Integration

### What to Commit

**Always commit:**
- All `.st` files in package directories
- `package.st` files
- `.project` file
- `BaselineOf` files

**Never commit:**
- Pharo image files (`.image`, `.changes`)
- `pharo-local/` directory
- Build artifacts

### Git Workflow with Tonel

1. **Edit Tonel files** in AI editor
2. **Import and test** in Pharo
3. **Commit changes** to git
4. **Push** to repository

**Good commit message:**
```
Add JSON serialization support to RediStick

- Implement RsJsonSerializer with to/from JSON methods
- Add tests for JSON conversion in RsJsonTest
- Update baseline to include RediStick-Json package
```

## Performance Considerations

### Import Performance

- **Batch imports**: Import all changed packages in sequence
- **Avoid repeated imports**: Import once per editing session
- **Cache Baseline reading**: Read dependency info once, reuse for multiple imports

### Testing Performance

- **Run specific tests**: Use class tests during development
- **Run package tests**: Use for pre-commit validation
- **Skip unchanged tests**: Focus on modified code areas

## Common Pitfalls and Solutions

### Pitfall 1: Using Relative Paths
**Problem**: Imports fail when working directory changes
**Solution**: Always use absolute paths

### Pitfall 2: Wrong Import Order
**Problem**: Import fails due to missing dependencies
**Solution**: Check Baseline and import in dependency order

### Pitfall 3: Forgetting to Re-import
**Problem**: Tests fail with old code
**Solution**: Always import after every Tonel file change

### Pitfall 4: Not Importing Test Packages
**Problem**: Tests don't run or run old versions
**Solution**: Import both main and test packages

### Pitfall 5: Editing in Pharo
**Problem**: Changes lost or not in version control
**Solution**: Always edit Tonel files, never Pharo image

### Pitfall 6: Skipping Tests
**Problem**: Bugs discovered late in development
**Solution**: Test after every import, no exceptions

### Pitfall 7: Ignoring Error Messages
**Problem**: Repeated failures without understanding root cause
**Solution**: Read error messages carefully, use `/st:eval` to debug

### Pitfall 8: Forgetting .project File When Creating Project Structure
**Problem**: Creating `src/` directories and `package.st` files but forgetting the `.project` file in the project root
**Solution**: When creating a new project structure from scratch, **always** create the `.project` file first. Use Pharo's format with single quotes:
```
{
	'srcDirectory' : 'src'
}
```
**Tip**: Use `/st:setup-project` command which handles all required files automatically.

## Summary Checklist

Before every commit, verify:

- ✅ All Tonel files saved
- ✅ All packages imported with absolute paths
- ✅ All tests passing
- ✅ Baseline updated if dependencies changed
- ✅ No direct edits in Pharo image
- ✅ Import order respects dependencies
- ✅ Test packages imported and tested
