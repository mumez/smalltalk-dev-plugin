---
name: st-lint
description: Smalltalk best-practices linter for Tonel files. Use when validating .st files before importing to Pharo, checking code quality after editing, or diagnosing style issues in a package.
allowed-tools: mcp__smalltalk-validator__lint_tonel_smalltalk_from_file Glob Bash
---

# Lint Tonel Files

Validate Tonel `.st` files for Smalltalk best practices before importing to Pharo.

## When to Use

- Before every `/st-import` — lint catches issues that would silently corrupt the image
- After editing `.st` files — confirm no style regressions
- When import fails with unexpected behavior — lint may reveal the root cause

## Step 1: Check Meta Files (manual — MCP cannot validate these)

Before linting, verify required Tonel meta files exist:

| File | Location | Required |
|------|----------|---------|
| `.project` | repo root | ✅ |
| `src/.properties` | `src/` directory | ✅ |
| `package.st` | each package directory | ✅ |

If any are missing, **warn the user, suggest `/st-setup-project`, and continue linting** — lint results are still useful even when meta files are absent, since the linter operates on `.st` file syntax independently.

Expected `src/.properties` content:
```
{
	#format : #tonel
}
```

## Step 2: Collect `.st` Files

Resolve the target to a list of absolute paths:

- **Single file** (`src/MyPackage/MyClass.st`): use directly, skip if it is `package.st`
- **Package directory** (`src/MyPackage`): Glob `**/*.st`, exclude `package.st`
- **src root** (`src`): Glob `**/*.st` across all packages, exclude `package.st`

Always convert to **absolute paths** before passing to the MCP tool.

## Step 3: Lint Each File

Call `mcp__smalltalk-validator__lint_tonel_smalltalk_from_file` with the absolute path, one file at a time.

## Step 4: Report Results

Show a summary per file:

```
src/MyPackage/MyClass.st — ✅ clean
src/MyPackage/AnotherClass.st — ⚠️ 2 warnings
  • [warning] Method #doSomething has no comment
  • [warning] Temporary variable 'x' shadows outer scope
src/MyPackage/BrokenClass.st — ❌ 1 error
  • [error] Syntax error near ']'
```

**Exit status semantics:**

| Result | Meaning | Action |
|--------|---------|--------|
| 0 — clean | No issues | Proceed to import |
| 1 — warnings only | Style issues | Proceed to import, consider fixing |
| 2 — errors found | Syntax/structural errors | Fix before importing |

## Interpreting Common Issues

| Issue | Likely Cause | Fix |
|-------|-------------|-----|
| `Syntax error near '...'` | Unclosed bracket/paren, missing period | Check the indicated line |
| `Method has no comment` | Missing method comment | Add a brief comment |
| `Temporary variable shadows outer scope` | Variable name collision | Rename the temp var |
| `Missing package.st` | package.st not found | Create it: `Package { #name : 'PkgName' }` |

## Related Skills

- `smalltalk-dev:st-setup-project` — Create missing meta files
- `smalltalk-dev:smalltalk-developer` — Full Edit → Lint → Import → Test workflow
