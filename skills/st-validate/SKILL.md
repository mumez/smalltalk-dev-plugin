---
name: st-validate
description: Validate Tonel file syntax before importing to Pharo. Use when suspecting syntax errors in .st files, or when a file was edited manually and correctness is uncertain.
allowed-tools: mcp__smalltalk-validator__validate_tonel_smalltalk_from_file mcp__smalltalk-validator__validate_tonel_smalltalk mcp__smalltalk-validator__validate_smalltalk_method_body
---

# Validate Tonel Syntax

Validate Tonel `.st` files before importing to Pharo. Modern AI usually generates correct Tonel, so this is optional.

## Usage

```
/st-validate /absolute/path/to/MyClass.st   # validate a file
/st-validate 'Tonel source text'            # validate content directly
```

## Steps

1. Call `validate_tonel_smalltalk_from_file` for a file path, or `validate_tonel_smalltalk` for raw content
2. Report any syntax errors or confirm the file is valid

## Notes

- Use `validate_smalltalk_method_body` to validate a single method body in isolation
- Validation errors must be fixed before importing

## Examples

```
/st-validate /home/user/project/src/MyPackage/MyClass.st
```
