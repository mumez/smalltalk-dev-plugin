---
name: smalltalk-developer
description: Comprehensive Pharo Smalltalk development workflow guide with AI-driven Tonel editing. Provides expertise in Tonel file format syntax (class definitions with name, superclass, instVars, category, method categories, class comment placement), package structure (package.st placement, directory organization, BaselineOf dependencies), development workflow (Edit → Import → Test cycle with absolute paths, re-import timing, test execution), and Pharo best practices (CRC format documentation, method categorization conventions). Use when working with Pharo Smalltalk projects, creating or editing Tonel .st files, organizing packages and dependencies, resolving import order issues, writing class comments, implementing standard Pharo development patterns (Singleton, Settings, etc.), or troubleshooting Tonel syntax.
---

# Smalltalk Developer Workflow

Implement the standard workflow for Pharo Smalltalk development using AI editors and the Tonel file format.

## Prerequisites

Before starting development, ensure the environment is ready:

- **No project structure** (no `src/` or `.project`): Use `smalltalk-dev:st-setup-project` skill to create boilerplate
- **Pharo not connected** or session not initialized: Use `smalltalk-dev:st-init` skill to verify connection and set up environment

## Core Development Cycle

The fundamental workflow for Smalltalk development consists of three steps that repeat:

### 1. Edit Tonel Files

Edit Tonel files directly in the AI editor. The AI editor is the **single source of truth** for code.

**Key principle**: All code changes happen in `.st` files, not in the Pharo image.

**Tonel basics:**
- One class per `.st` file
- File name matches class name
- Each package directory contains `package.st`

For detailed Tonel syntax and examples, see [Tonel Format Reference](references/tonel-format.md).

Follow the [Style Guide](references/style-guide.md) for idiomatic Smalltalk.

Refer to [Implementation Patterns](references/patterns.md) for common patterns (Singleton, Settings, etc.).

### 2. Import to Pharo

**Before importing**, run lint to check code quality:

```
/st-lint src/MyPackage   # Check Smalltalk best practices
```

Then import the package into the running Pharo image using absolute paths:

```
mcp__smalltalk-interop__import_package: 'MyPackage' path: '/home/user/project/src'
```

**Critical rules:**
- ✅ **Lint before import** - Check code quality first
- ✅ Always use **absolute paths** (never relative)
- ✅ Re-import after **every change**
- ✅ Import packages in **dependency order**
- ✅ Import test packages **after main packages**

### 3. Run Tests

After import, run tests to verify changes:

```
mcp__smalltalk-interop__run_class_test: 'MyClassTest'
mcp__smalltalk-interop__run_package_test: 'MyPackage-Tests'
```

If tests fail, return to step 1, fix the Tonel file, and repeat.

## Essential Best Practices

### Path Management

**Always use absolute paths for imports:**
```
✅ /home/user/myproject/src
❌ ./src
❌ ../myproject/src
```

**Finding the source directory:**
1. Check `.project` file for `srcDirectory` field
2. Common locations: `src/`, `repositories/`
3. Construct full path: `<project_root>/<srcDirectory>`

See [Best Practices Reference](references/best-practices.md#path-management) for details.

### Package Dependencies

Check `BaselineOf<ProjectName>` to determine correct import order:

```smalltalk
baseline: spec
    <baseline>
    spec for: #common do: [
        spec
            package: 'MyPackage-Core';
            package: 'MyPackage-Json' with: [
                spec requires: #('MyPackage-Core')
            ]
    ]
```

**Import order**: Dependencies first → `MyPackage-Core` → `MyPackage-Json`

See [Best Practices: Dependencies](references/best-practices.md#package-dependencies-and-import-order) for complete guide.

### Import Timing

**Re-import after every change** - Pharo doesn't automatically reload files.

**Standard sequence:**
1. Edit `MyPackage/MyClass.st`
2. Import `MyPackage`
3. Edit `MyPackage-Tests/MyClassTest.st`
4. Import `MyPackage-Tests`
5. Run tests

### File Editing Philosophy

The AI editor is the source of truth:

- ✅ Edit `.st` files → Import to Pharo
- ❌ Edit in Pharo → Export to `.st` files

Use `export_package` only for:
- Initial project setup
- Emergency recovery
- Exploring existing code

See [Best Practices: File Editing](references/best-practices.md#file-editing-philosophy) for rationale.

## Common Patterns

### Pattern 1: Creating New Class

```
1. Ensure .project file exists in project root (create if missing - see Best Practices)
2. Create src/MyPackage/MyClass.st with class definition
3. import_package: 'MyPackage' path: '/absolute/path/src'
4. run_class_test: 'MyClassTest'
```

### Pattern 2: Adding Methods

```
1. Read existing src/MyPackage/MyClass.st
2. Add new method to file
3. import_package: 'MyPackage' path: '/absolute/path/src'
4. run_class_test: 'MyClassTest'
```

### Pattern 3: Multi-Package Development

```
1. Check BaselineOf for dependency order
2. Import packages in correct sequence
3. Import test packages last
4. Run comprehensive tests
```

### Pattern 5: Renaming a Class

Renaming a class only in the Tonel file and importing leaves the old class in the Pharo image. Always rename in Pharo first, then import.

```
1. Rename the class in Pharo via eval:
   (OldClassName rename: 'NewClassName') printString

2. Update the Tonel filename and class name in the .st file

3. Import the package
   import_package: 'MyPackage' path: '/absolute/path/src'
```

**Why?** Tonel import creates or updates classes by name. If the old name still exists in the image and the new name is introduced as a new class, both will coexist — and any references to the old class remain broken.

### Pattern 4: Installing External Packages from GitHub

When a required package is **not present locally**, use `install_project` — never use Metacello directly via `eval`:

```
mcp__smalltalk-interop__install_project
  project_name: 'MyLib'
  repository_url: 'github://owner/MyLib:branch/src'
```

Parameters:
- `project_name` (required): the baseline/project name
- `repository_url` (required): Metacello-style URL (e.g. `github://owner/repo:branch/src`)
- `load_groups` (optional): comma-separated groups to load

**Critical rule**: If a class or package is missing and is not in the local source tree, **always reach for `install_project` first**. Do NOT try to load it with `Metacello new baseline:... load` via `eval` — that approach bypasses the managed workflow and can corrupt the image state.

For complete examples, see [Development Session Examples](examples/development-sessions.md).

## Automation

When this skill is active, automatically suggest:

1. **Import commands** after editing Tonel files
2. **Test commands** after successful import
3. **Debugging procedures** when tests fail

## Quick Reference

### MCP Tools

**Install external package (GitHub/remote):**
```
mcp__smalltalk-interop__install_project
  project_name: 'MyLib'
  repository_url: 'github://owner/MyLib:branch/src'
```
> Use this when a package is not present locally. Never use Metacello via eval for this purpose.

**Import local package:**
```
mcp__smalltalk-interop__import_package: 'PackageName' path: '/absolute/path'
```

**Test:**
```
mcp__smalltalk-interop__run_class_test: 'TestClassName'
mcp__smalltalk-interop__run_package_test: 'PackageName-Tests'
```

**Debug:**
```
mcp__smalltalk-interop__eval: 'Smalltalk code here'
```

**Validate (optional):**
```
mcp__smalltalk-validator__validate_tonel_smalltalk_from_file: '/path/to/file.st'
```

### Common Commands

- `/st-import PackageName /path` - Import package
- `/st-test TestClass` - Run tests
- `/st-eval code` - Execute Smalltalk snippet
- `/st-validate file.st` - Validate syntax

## Troubleshooting

### Import Fails

**"Package not found":**
- Verify absolute path is correct (in Docker, use container-side path: typically `/root/repos` — check `compose.yml` for volume mounts)
- Check `package.st` exists
- Ensure package name matches directory

**"Syntax error":**
- Run `validate_tonel_smalltalk_from_file` first
- Check Tonel syntax (brackets, quotes, periods)

**"Dependency not found" / missing external package:**
- If the package is not in the local source tree, install it from GitHub:
  ```
  mcp__smalltalk-interop__install_project
    project_name: 'MyLib'
    repository_url: 'github://owner/MyLib:branch/src'
  ```
- Do **NOT** try `Metacello new baseline: ... load` via `eval` — use `install_project` instead
- If it is a local package, check Baseline for required packages and import dependencies first

### Tests Fail

1. Read error message carefully
2. Use `/st-eval` to debug incrementally
3. Fix in Tonel file (not Pharo)
4. Re-import and re-test

See [Best Practices: Error Handling](references/best-practices.md#error-handling-and-debugging) for complete guide.

## Complete Documentation

This skill provides focused guidance for the core workflow. For comprehensive information:

- **[Tonel Format Reference](references/tonel-format.md)** - Complete Tonel syntax guide
- **[Style Guide](references/style-guide.md)** - Idiomatic Smalltalk patterns and concise coding practices
- **[Best Practices](references/best-practices.md)** - Detailed practices and patterns
- **[Development Examples](examples/development-sessions.md)** - Real-world session workflows
- **[Implementation Patterns](references/patterns.md)** - Common patterns (Singleton, etc.)

## Summary Workflow

```
Edit .st file
    ↓
Lint code (/st-lint)
    ↓
Import package (absolute path)
    ↓
Run tests
    ↓
Tests pass? → Done
    ↓
Tests fail? → Debug with /st-eval → Fix .st file → Re-import
```

**Remember**: The cycle is Edit → Lint → Import → Test. Never skip lint, import, or tests.
