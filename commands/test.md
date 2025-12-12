---
name: st:test
description: Run SUnit tests
---

# Run SUnit Tests

Execute tests after importing changes.

## Usage

```bash
/st:test MyTestClass
/st:test MyPackage-Tests
```

## Implementation

Uses `run_class_test` or `run_package_test` from pharo-interop MCP server.

## Notes

- Run tests after every import
- Test class: `run_class_test`
- Test package: `run_package_test`

## Examples

```bash
# Run specific test class
/st:test RsJsonTest

# Run all tests in package
/st:test RediStick-Json-Tests
```

## MCP Tool Calls

```
# For test class
mcp__smalltalk-interop__run_class_test: 'TestClassName'

# For test package
mcp__smalltalk-interop__run_package_test: 'PackageName-Tests'
```
