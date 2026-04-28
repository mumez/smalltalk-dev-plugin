---
name: st-lint
description: Lint Tonel files for Smalltalk best practices before import
allowed-tools:
  - mcp__smalltalk-validator__lint_tonel_smalltalk_from_file
  - Glob
  - Bash
---

# Lint Tonel Files

Lint Tonel `.st` files for Smalltalk best practices using the smalltalk-validator MCP server.

## Usage

```
/st-lint src/MyPackage/MyClass.st   # single file
/st-lint src/MyPackage              # entire package directory
/st-lint src                        # all packages under src/
```

## Steps

### 1. Check meta files (manual check, MCP cannot validate these)

Before linting, check the target directory for required Tonel meta files:

- `.project` — Tonel project descriptor (required at repo root or package root)
- `src/.properties` — Tonel format descriptor (required in the `src/` directory)
- `package.st` — package descriptor (required in each package directory)

If any of these is missing, show a warning. See `/st-setup-project` for the expected structure.

The expected format for `src/.properties` is:

```
{
	#format : #tonel
}
```

### 2. Collect `.st` files

- If a single `.st` file is given: use it directly (skip if it is `package.st`)
- If a directory is given: use Glob to find all `**/*.st` files under it, excluding `package.st`
- Resolve each path to an absolute path before passing to the MCP tool

### 3. Lint each file

Call `mcp__smalltalk-validator__lint_tonel_smalltalk_from_file` with the absolute path for each file, one at a time in order.

### 4. Report results

Show a summary per file: file path, warning count, error count, and each issue message.
Exit status: 0 = clean, warnings only = 1, errors found = 2.
