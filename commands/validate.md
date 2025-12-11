---
name: validate
description: Validate Tonel syntax (optional, modern AI usually generates correct Tonel)
---

# Validate Tonel

Optional validation before import. Modern AI usually generates correct Tonel.

## Usage

```bash
/st:validate /path/to/file.st
/st:validate /path/to/package/MyClass.st
```

## Implementation

Uses `validate_tonel_smalltalk_from_file` from smalltalk-validator MCP server.

## Notes

- Usually unnecessary with modern AI
- Use only for complex syntax
- Validates both structure and method bodies

## Options

```bash
# Validate with options
/st:validate /path/to/file.st --structure-only
```

## Examples

```bash
# Validate single file
/st:validate /home/user/project/src/MyPackage/MyClass.st

# Validate method body directly
mcp__smalltalk-validator__validate_smalltalk_method_body: '^ self name asUppercase'
```

## MCP Tool Calls

```
# Validate file
mcp__smalltalk-validator__validate_tonel_smalltalk_from_file: '/path/to/file.st'

# Validate content
mcp__smalltalk-validator__validate_tonel_smalltalk: 'Class { #name : #MyClass ... }'

# Validate method only
mcp__smalltalk-validator__validate_smalltalk_method_body: '^ 42'
```
