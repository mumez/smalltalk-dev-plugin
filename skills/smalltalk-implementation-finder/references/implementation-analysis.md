# Implementation Analysis Reference

Comprehensive guide to analyzing method implementations across class hierarchies in Pharo Smalltalk.

## Core Analysis Techniques

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

## MCP Tools Reference

### Find implementations

```
mcp__smalltalk-interop__search_implementors: 'methodName'
```

Returns all classes that implement the specified method selector.

**Example**:
```
search_implementors("printOn:")
→ [Object, Collection, Array, Dictionary, Point, ...]
```

### Get method source

```
mcp__smalltalk-interop__get_method_source: class: 'ClassName' method: 'methodName'
```

Retrieves the source code of a specific method in a class.

**Example**:
```
get_method_source("Point", "printOn:")
→ "printOn: aStream
    aStream nextPut: $(; print: x; nextPut: $@; print: y; nextPut: $)"
```

### Eval for hierarchy exploration

```
mcp__smalltalk-interop__eval: 'Collection allSubclasses'
```

Execute Smalltalk code to explore class hierarchies.

**Useful expressions**:
```smalltalk
Collection allSubclasses      "All subclasses"
Point superclass              "Get superclass"
Object allSubclasses size     "Count subclasses"
```

### Find references

```
mcp__smalltalk-interop__search_references: 'methodName'
```

Find all senders of a method (combines well with implementor analysis).

## Analysis Patterns

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

## Advanced Techniques

### Tracing Template Method Pattern

**Pattern**: Superclass defines template, subclasses implement hooks.

**Detection**:
```
1. Find abstract method in superclass:
   get_method_source("Collection", "do:")

2. Find hook methods it calls:
   Search for "self hookMethod"

3. Find hook implementations:
   search_implementors("hookMethod")
```

### Identifying Concrete vs Abstract

**Concrete implementation** - Does actual work:
```smalltalk
Array>>at: index
    ^ elements at: index
```

**Abstract implementation** - Delegates to subclasses:
```smalltalk
Collection>>do: aBlock
    self subclassResponsibility
```

### Cross-Hierarchy Comparison

**Goal**: Compare how different hierarchies solve same problem.

**Example**: Comparing iteration across hierarchies

```
# Collections use do:
Collection>>do: aBlock
    [internal iteration]

# Streams use next
Stream>>next
    [sequential access]

# Files use nextLine
File>>nextLine
    [line-based iteration]
```

**Insight**: Different abstractions for different use cases.

## Performance Considerations

### When to Use search_implementors

✅ **Use when**:
- Learning how to implement a method
- Assessing refactoring impact
- Finding duplicate code

❌ **Don't use when**:
- You know exactly which class you need
- Looking for usage, not implementation
- Need to search by other criteria

### Caching Results

For repeated analysis:
```
1. Cache implementor list
2. Batch get_method_source calls
3. Analyze offline
```

### Filtering Strategies

**By package**:
```
search_classes_like("MyApp*")
→ Filter to your application classes
```

**By hierarchy**:
```
eval("MyBaseClass allSubclasses")
→ Focus on specific hierarchy
```

**By pattern**:
```
# Find classes with "Test" in name
search_classes_like("*Test")
```

## Summary

**Key principle**: Implementations across a hierarchy reveal design patterns and idioms. Use them to write better, more idiomatic code.

**Primary workflow**:
1. Find implementors
2. Get source
3. Analyze patterns
4. Apply learnings

**Remember**: Always scope your analysis to relevant hierarchies and representative classes to avoid information overload.
