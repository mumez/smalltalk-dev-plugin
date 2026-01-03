# Smalltalk Style Guide

This guide covers common coding patterns and idioms in Pharo Smalltalk to help write more idiomatic code.

## Concise Collection Access

Smalltalk provides convenient accessor methods that are more readable than numeric indexing.

### Array Access Patterns

**Use named accessors instead of numeric indices:**

```smalltalk
❌ Verbose:
nameArray at: 1
nameArray at: 2
nameArray at: (nameArray size)

✅ Concise:
nameArray first
nameArray second
nameArray last
```

**Why**: Named accessors clearly express intent and are less error-prone.

### Additional Collection Accessors

```smalltalk
collection first          "First element"
collection second         "Second element"
collection third          "Third element (if available)"
collection fourth         "Fourth element (if available)"
collection last           "Last element"
collection allButFirst    "All except first"
collection allButLast     "All except last"
```

## Nil-Safe Branching

Smalltalk provides specialized messages for nil checking that combine the test and action.

### Nil Check Patterns

**Use specialized nil messages instead of explicit checks:**

```smalltalk
❌ Verbose:
value isNil ifTrue: [ self doSomething ]
value notNil ifTrue: [ self doSomething ]

✅ Concise:
value ifNil: [ self doSomething ]
value ifNotNil: [ self doSomething ]
```

### Nil-Safe Branching with Values

**With value transformation:**

```smalltalk
❌ Verbose:
result := value isNil
    ifTrue: [ 'default' ]
    ifFalse: [ value asString ]

✅ Concise:
result := value
    ifNil: [ 'default' ]
    ifNotNil: [ :v | v asString ]
```

**Note**: `ifNotNil:` passes the non-nil value to the block.

### Combined Nil Messages

```smalltalk
value ifNil: [ 'default' ] ifNotNil: [ :v | v asString ]
value ifNotNil: [ :v | v process ] ifNil: [ 'none' ]
```

## Boolean Simplification

### Avoid Redundant Comparisons

```smalltalk
❌ Verbose:
flag = true
flag = false
flag == true

✅ Concise:
flag
flag not
```

### Conditional Returns

```smalltalk
❌ Verbose:
condition ifTrue: [ ^ true ].
^ false

✅ Concise:
^ condition
```

## Common Idioms Summary

| Pattern | Verbose | Concise |
|---------|---------|---------|
| First element | `array at: 1` | `array first` |
| Last element | `array at: array size` | `array last` |
| Nil check | `x isNil ifTrue: [...]` | `x ifNil: [...]` |
| Not-nil check | `x notNil ifTrue: [...]` | `x ifNotNil: [...]` |
| Boolean comparison | `flag = true` | `flag` |
| Negation | `flag = false` | `flag not` |

## When to Use These Patterns

✅ **Use concise patterns when:**
- Code intent is clearer
- Reduced verbosity improves readability
- Standard Pharo idioms exist

❌ **Avoid when:**
- Numeric index is semantically meaningful (e.g., `matrix at: row at: column`)
- Performance-critical code benefits from explicit indexing
- Custom collection classes don't support standard accessors

## See Also

- **[Pharo with Style](https://github.com/SquareBracketAssociates/Booklet-PharoWithStyle/blob/master/Chapters/withStyle.md)** - Comprehensive style guide for Pharo Smalltalk
- [Pharo by Example](https://books.pharo.org/) - Comprehensive Smalltalk patterns
- [Best Practices Reference](best-practices.md) - General development practices
- [Tonel Format Reference](tonel-format.md) - File format syntax
