---
name: st-export
description: Export package from the Smalltalk image (Pharo or Squeak) to Tonel files. Use when code was changed directly in the Smalltalk image (debugger, browser, or code generation) and needs to be synced back to Tonel files.
allowed-tools: mcp__smalltalk-interop__export_package
---

# Export Package from the Smalltalk Image

Export a package from the running Smalltalk image back to Tonel files.

## Usage

```
/st-export PackageName /absolute/path/to/src
```

## Steps

1. Call `export_package` with package name and absolute path to the `src/` directory
2. Report the exported files or any error

## When to Use

Export is the exception, not the norm. Use only when the Smalltalk image has newer code than Tonel files:

- Fixed code in the debugger
- Generated boilerplate using Smalltalk tools
- Made changes directly in the class browser

## Notes

- Normally, edit Tonel files and import — AI editor is the source of truth
- Export overwrites existing `.st` files in the directory
- Always use absolute paths

## Examples

```
/st-export MyPackage /home/user/project/src
/st-export MyPackage-Tests /home/user/project/src
```
