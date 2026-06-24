---
name: st-import
description: Import Tonel package into running Pharo image. Use when loading edited .st files into Pharo after code changes.
allowed-tools: mcp__smalltalk-interop__import_package mcp__smalltalk-validator__validate_tonel_smalltalk_from_file
---

# Import Tonel Package

Import edited Tonel files into the running Pharo image.

## Usage

```
/st-import PackageName /absolute/path/to/src
```

## Steps

### With arguments: import a single package

1. Call `import_package` with the package name and absolute path to the `src/` directory
2. Report success or error from the result

### Without arguments: import all local packages in dependency order

When no arguments are given, discover and import the full project automatically:

1. **Locate the source directory** — read `.project` from the repository root. It contains JSON-like Pharo syntax with a `srcDirectory` key (e.g., `'srcDirectory' : 'src'`). Resolve the value relative to the repository root to get the absolute path to the source directory.

2. **Determine import order** — find the `BaselineOf` package inside the source directory (a directory whose name starts with `BaselineOf`). Read the `BaselineOfXXX.class.st` file inside it and parse the `baseline: spec` method. Extract:
   - All `package: 'PkgName'` declarations → the full list of packages to import
   - All `requires: #('Dep1' 'Dep2' ...)` clauses → the dependency edges between packages

   Topologically sort the packages so that each package is imported only after all its dependencies have been imported. Import the `BaselineOf` package first (before all others), then the sorted application packages.

3. **Import each package** — call `import_package` for each package in the computed order, passing the absolute path to the source directory. Report the result of each import as you go.

## Notes

- Always use absolute paths
- Use the **package parent directory** as the path — do not include the package name in the path
- Import main package before test package
- Re-import after every change

## Examples

```
/st-import RediStick-Json /home/user/git/RediStick/src
/st-import RediStick-Json-Tests /home/user/git/RediStick/src
```
