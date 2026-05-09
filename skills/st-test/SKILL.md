---
name: st-test
description: Run SUnit tests in the running Pharo image. Use when verifying changes after import, or when checking results for a specific test class or package.
allowed-tools: mcp__smalltalk-interop__run_class_test mcp__smalltalk-interop__run_package_test
---

# Run SUnit Tests

Execute SUnit tests after importing changes to Pharo.

## Usage

```
/st-test TestClassName       # run a single test class
/st-test PackageName-Tests   # run all tests in a package
```

## Steps

1. Determine whether the argument is a class name or a package name
   - Class → call `run_class_test`
   - Package → call `run_package_test`
2. Report pass/fail counts and any failures

## Examples

```
/st-test RsJsonTest
/st-test RediStick-Json-Tests
```
