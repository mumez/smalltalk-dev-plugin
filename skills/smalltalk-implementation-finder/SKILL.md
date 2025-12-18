---
name: smalltalk-implementation-finder
description: Use this skill when user asks "who implements X?", "find implementors of Y", "how is this method implemented?", "show implementations", "which classes override Z?", "abstract method implementations", or needs to analyze method implementations across classes, discover implementation patterns, understand subclass responsibilities, or assess refactoring impact by tracing implementations through class hierarchies.
model_selection:
  enabled: false
triggers:
  - "find implementors"
  - "who implements"
  - "show implementations"
  - "implementations of"
  - "which classes override"
  - "abstract method"
  - "subclass responsibility"
  - "implementation pattern"
  - "search implementors"
tool_permissions:
  allowed_tools:
    - search_implementors
    - get_method_source
    - get_class_source
    - eval
    - search_references
    - search_classes_like
---

# Smalltalk Implementation Finder

Find and analyze method implementations across class hierarchies to understand abstract methods, implementation patterns, and assess refactoring opportunities.

## Core Use Cases

### 1. Understanding Abstract Method Implementations

**Goal**: See how abstract methods (`self subclassResponsibility`) are implemented in concrete subclasses.

**Primary Tool**: `search_implementors` - Finds all classes implementing a method

**Workflow**:
```
1. Find all implementors: search_implementors(method_name)
2. Optionally scope by hierarchy: eval("ClassA allSubclasses")
3. Get source for relevant implementors: get_method_source
4. Analyze patterns and idioms
```

**Example**: Understanding `printOn:` implementations

```
search_implementors("printOn:")
→ Returns hundreds of implementations

# Focus on Collection hierarchy
eval("Collection allSubclasses")

get_method_source("Array", "printOn:")
→ Shows: "#(" prefix, recursive printOn:, ")" suffix

get_method_source("Dictionary", "printOn:")
→ Shows: "a Dictionary(" prefix, key->value format, ")" suffix

Pattern identified:
- Start with class identifier
- Recursively call printOn: on elements
- Use appropriate delimiters
```

**Why useful**: Learn idiomatic patterns by studying existing implementations.

### 2. Learning Implementation Idioms

**Goal**: Discover common patterns and best practices for implementing specific methods.

**Common Idioms**:

**Idiom: `hash` methods use `bitXor:`**
```smalltalk
Point>>hash
    ^ x hash bitXor: y hash

Association>>hash
    ^ key hash bitXor: value hash
```

**Idiom: `initialize` calls super first**
```smalltalk
OrderedCollection>>initialize
    super initialize.
    array := Array new: 10
```

**Idiom: `=` checks class then compares fields**
```smalltalk
Point>>= aPoint
    self class = aPoint class ifFalse: [^ false].
    ^ x = aPoint x and: [y = aPoint y]
```

**Workflow for discovering idioms**:
```
1. search_implementors(method_name)
2. Sample 5-10 well-known class implementations
3. get_method_source for each
4. Identify common patterns
5. Apply pattern to your implementation
```

### 3. Assessing Signature Change Impact

**Goal**: Understand how many implementations would be affected by changing a method signature.

**Workflow**:
```
1. Find implementors: search_implementors(method_name)
   → Count implementations to update

2. Find references: search_references(method_name)
   → Count call sites to update

3. Assess impact: High/Medium/Low
4. Make informed decision
```

**Example**: Changing `at:put:` signature

```
search_implementors("at:put:")
→ 50+ implementations

search_references("at:put:")
→ 500+ call sites

Impact: VERY HIGH
Recommendation: Don't change. Add new method instead.
```

**Impact levels**:
- **Low**: < 5 implementors, < 20 references
- **Medium**: 5-20 implementors, 20-100 references
- **High**: 20+ implementors, 100+ references

### 4. Discovering Refactoring Opportunities

**Goal**: Find duplicate implementations that could be consolidated.

**Workflow**:
```
1. Find implementors in same hierarchy
2. Compare implementations
3. Identify patterns:
   - Identical → Pull up to superclass
   - Similar → Parameterize and share
   - Scattered → Consider redesign
```

**Example**: Duplicate `isEmpty` implementations

```
get_method_source("Array", "isEmpty")
→ "^ self size = 0"

get_method_source("OrderedCollection", "isEmpty")
→ "^ self size = 0"

get_method_source("Set", "isEmpty")
→ "^ self size = 0"

Opportunity: All identical. Pull up to Collection superclass.
```

**Refactoring indicators**:
- ✅ Exact same code in 3+ subclasses
- ✅ Similar code with minor variations
- ❌ Many `subclassResponsibility` stubs

### 5. Narrowing Usage Search

**Goal**: Filter method references by which class's implementation is actually being called.

**Problem**: `search_references` returns ALL calls to ANY method with that name.

**Solution**: Combine with implementor analysis.

**Workflow**:
```
1. Find implementors: search_implementors(method_name)
   → Get list of implementing classes

2. Find references: search_references(method_name)
   → Get all call sites

3. Filter by context:
   - Check variable names
   - Check receiver types
   - Match with implementor classes
```

**Example**: Finding Collection>>select: usage

```
search_implementors("select:")
→ [Collection, Dictionary, Interval, ...]

search_references("select:")
→ 1000+ call sites

Filter by checking receiver:
  "dataArray select: [:item | ...]"
  → dataArray is Array → uses Collection>>select:

  "settingsDict select: [:setting | ...]"
  → settingsDict is Dictionary → uses Dictionary>>select:
```

## Quick Reference

### MCP Tools

**Find implementations**:
```
mcp__smalltalk-interop__search_implementors: 'methodName'
```

**Get method source**:
```
mcp__smalltalk-interop__get_method_source: class: 'ClassName' method: 'methodName'
```

**Eval for hierarchy**:
```
mcp__smalltalk-interop__eval: 'Collection allSubclasses'
```

**Find references**:
```
mcp__smalltalk-interop__search_references: 'methodName'
```

### Analysis Patterns

| Goal | Primary Tool | Pattern |
|------|--------------|---------|
| Learn idiom | search_implementors | Sample 5-10 → identify pattern |
| Impact assessment | search_implementors + search_references | Count both → assess risk |
| Find duplicates | search_implementors + get_method_source | Compare → find identical |
| Narrow usage | search_implementors → search_references | Filter by receiver type |

## Best Practices

### 1. Scope by Hierarchy

Focus on relevant class hierarchies:

✅ **Good**:
```
eval("Collection allSubclasses")
→ Filter implementors to Collection hierarchy
```

❌ **Bad**: Analyze all 500+ implementors without filtering

### 2. Sample Representative Classes

Choose well-known classes for learning:

✅ **Good**: Array, Dictionary, Point (common, well-implemented)
❌ **Bad**: Obscure internal classes (may have non-standard patterns)

### 3. Count Before Analyzing

Get overview before diving deep:

✅ **Good**:
```
1. Count implementors
2. If < 10, analyze all
3. If > 10, sample top classes
```

❌ **Bad**: Try to analyze 500 implementations

### 4. Check Super Implementation

Understand inherited behavior:

✅ **Good**: Check superclass implementation first
❌ **Bad**: Only look at subclass implementations

### 5. Consider Polymorphism

Remember same method name ≠ same implementation:

✅ **Good**: "Collection>>select: and Dictionary>>select: behave differently"
❌ **Bad**: "select: works this way" (which class?)

## Common Workflows

### Workflow 1: Implement Abstract Method

```
Problem: Need to implement printOn: in new class

1. search_implementors("printOn:")
2. Filter by similar classes
3. get_method_source for 3-5 examples
4. Identify pattern:
   - Class identifier prefix
   - Recursive element printing
   - Closing delimiter
5. Apply pattern to your class
```

### Workflow 2: Assess Refactoring

```
Problem: Want to change at:put: signature

1. search_implementors("at:put:")
   → 50+ implementations

2. search_references("at:put:")
   → 500+ call sites

3. Decision: Too high impact. Don't change.
   Alternative: Add new method at:put:ifAbsent:
```

### Workflow 3: Find Duplication

```
Problem: Suspect duplicate isEmpty implementations

1. search_implementors("isEmpty")
2. get_method_source for Collection subclasses
3. Compare:
   Array: "^ self size = 0"
   Set: "^ self size = 0"
   OrderedCollection: "^ self size = 0"

4. Refactor: Pull up to Collection superclass
```

For detailed analysis techniques and comprehensive examples, see:

- **[Implementation Analysis Reference](references/implementation-analysis.md)** - Detailed techniques
- **[Implementation Scenarios](examples/implementation-scenarios.md)** - Real-world examples

## Summary

**Key workflow**: Find implementors → Get source → Analyze patterns → Apply learnings

**Primary use cases**:
1. **Learn idioms** - Study existing implementations
2. **Assess impact** - Count implementations + references
3. **Find duplication** - Compare implementations
4. **Narrow search** - Filter by implementor class

**Remember**: Implementations across a hierarchy reveal design patterns and idioms. Use them to write better, more idiomatic code.
