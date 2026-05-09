---
name: st-import
description: Import Tonel package into running Pharo image. Use when loading edited .st files into Pharo after code changes.
allowed-tools:
  - mcp__smalltalk-interop__import_package
  - mcp__smalltalk-validator__validate_tonel_smalltalk_from_file
---

# Import Tonel Package

Import edited Tonel files into the running Pharo image.

## Usage

```
/st-import PackageName /absolute/path/to/src
```

## Steps

1. Call `import_package` with package name and absolute path to the `src/` directory
2. Report success or error from the result

## Notes

- Always use absolute paths
- Import main package before test package
- Re-import after every change

## Examples

```
/st-import RediStick-Json /home/user/git/RediStick/src
/st-import RediStick-Json-Tests /home/user/git/RediStick/src
```
