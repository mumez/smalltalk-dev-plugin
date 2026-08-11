---
name: st-test
description: Run SUnit tests in the running Smalltalk image (Pharo or Squeak). Use when verifying changes after import, or when checking results for a specific test class or package.
allowed-tools: mcp__smalltalk-interop__run_class_test mcp__smalltalk-interop__run_package_test mcp__smalltalk-interop__list_packages
---

# Run SUnit Tests

Execute SUnit tests after importing changes to the Smalltalk image.

## Usage

```
/st-test TestClassName       # run a single test class
/st-test PackageName-Tests   # run all tests in a package
```

## Steps

### With arguments: run a specific test class or package

1. Determine whether the argument is a class name or a package name
   - Class name (e.g. `RsJsonTest`) → call `run_class_test`
   - Package name (e.g. `RediStick-Json-Tests`) → call `run_package_test`
2. Report pass/fail counts and any failures

### Without arguments: run all test packages in the project

When no arguments are given, discover and run all test packages automatically:

1. **Locate the source directory** — read `.project` from the repository root and extract the `srcDirectory` value. Resolve it to an absolute path.

2. **Identify test packages** — find the `BaselineOf` package inside the source directory (a directory starting with `BaselineOf`) and read its `BaselineOfXXX.class.st`. Parse the `baseline: spec` method and collect all packages whose names end with `-Tests` (or follow the project's test-naming convention).

3. **Check what is already imported** — call `list_packages` to get the list of packages currently loaded in the Smalltalk image.

4. **Run tests for imported packages** — for each test package that is present in the image, call `run_package_test` and report the pass/fail results.

5. **Report unimported packages** — for any test package found in the BaselineOf but *not* loaded in the image, skip running tests and report it to the user with a note that they can run `/st-import` first to load it.

## Examples

```
/st-test RsJsonTest
/st-test RediStick-Json-Tests
```
