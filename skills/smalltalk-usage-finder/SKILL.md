---
name: smalltalk-usage-finder
description: Understanding class responsibilities, usage patterns, and package overviews in Smalltalk code
model_selection:
  enabled: false
triggers:
  - "class responsibility"
  - "how to use"
  - "usage of"
  - "find usage"
  - "find examples"
  - "package overview"
  - "example method"
tool_permissions:
  allowed_tools:
    - get_class_comment
    - get_class_source
    - get_method_source
    - search_references_to_class
    - search_references
    - search_classes_like
    - search_methods_like
    - list_classes
    - list_methods
---

# Smalltalk Usage Finder

This skill helps you understand how Smalltalk classes, methods, and packages are used in practice. It provides systematic workflows for discovering class responsibilities, finding usage examples, and generating package overviews.

## Core Workflows

### 1. Understanding Class Responsibilities

**Goal**: Discover what a class is responsible for and its intended purpose.

**Primary Tool**: `get_class_comment`

**Workflow**:

```
1. Verify the class name (if uncertain): search_classes_like(class_name_query)
2. Get the class comment: get_class_comment(class_name)
3. Present the responsibility clearly
```

**Example**: For "What is Point's responsibility?"

```smalltalk
# Step 1: Verify (if needed)
search_classes_like("Point")
→ ["Point", "PointArray", "PointCollection", ...]

# Step 2: Get comment
get_class_comment("Point")
→ "I represent an x-y pair of numbers usually designating a location on the screen."
```

**Best Practice**: Class comments are the authoritative source. Always start here.

---

### 2. Finding Class Usage Patterns

**Goal**: Understand how to use a class by examining real usage examples.

**Primary Tools**: `search_references_to_class`, `get_method_source`, `list_methods`

**Workflow**:

```
1. Check for example methods (exampleXXX):
   a. list_methods(package_name) or search_methods_like("example")
   b. Filter for methods starting with "example"
   c. get_method_source(class_name, example_method, is_class_method: true)

2. Find real-world usage:
   a. search_references_to_class(class_name)
   b. Select top 3-5 relevant references
   c. get_method_source for each reference
   d. Identify patterns: constructors, initialization, common messages

3. Synthesize usage guide
```

**Example**: For "How do I use Point?"

```smalltalk
# Step 1: Check for examples
search_methods_like("example")
→ Find: "Point class>>exampleGrid"

get_method_source("Point", "exampleGrid", is_class_method: true)
→ Shows grid creation using x@y syntax

# Step 2: Find real-world usage
search_references_to_class("Point")
→ References from Rectangle, Morph, etc.

get_method_source("Rectangle", "center")
→ "center ^ (origin + corner) // 2"
→ Pattern: Point arithmetic

# Step 3: Synthesize
"Point is created using @ (e.g., 100@200).
Common usage: x@y, point x, point y, point1 + point2"
```

**Pattern Recognition**:
- Constructors: `new`, `x:y:`, `@` message
- Common messages: `x`, `y`, `+`, `-`, `*`, `/`
- Initialization sequences

**Tip on Example Methods**:
- Usually class-side methods in `examples` category
- Names: `exampleSimple`, `example1`, `exampleWithData`
- Not all classes have them; if missing, use reference search

---

### 3. Finding Method Usage

**Goal**: Understand how a specific method is used in context.

**Primary Tools**: `search_references`, `get_method_source`

**Challenge**: Polymorphism means the same method name appears in multiple classes. Use variable naming and context to identify the correct class.

**Workflow**:

```
1. Verify method exists (if uncertain): search_methods_like(method_name)
2. Find all references: search_references(method_name_or_symbol)
3. Filter for target class:
   a. get_method_source for each reference
   b. Look for variable naming clues (e.g., "aPoint" suggests Point)
   c. Check type comments (e.g., "aPoint <Point>")
4. Present usage examples with context
```

**Example**: For "How to use Point>>distanceTo:?"

```smalltalk
# Step 1: Verify
search_methods_like("distanceTo")
→ ["Point>>distanceTo:", "Vector>>distanceTo:", ...]

# Step 2: Find references
search_references("distanceTo:")
→ Multiple matches

# Step 3: Filter
get_method_source("Morph", "nearestMorphAt:")
→ "distance := point1 distanceTo: point2"
→ Variable "point1" suggests Point ✓

get_method_source("Circle", "contains:")
→ "^ center distanceTo: aPoint <= radius"
→ Variables "center" and "aPoint" are Points ✓

# Step 4: Present
"Point>>distanceTo: calculates Euclidean distance.
Usage: point1 distanceTo: point2 → Float
Example: (100@100) distanceTo: (200@200) → 141.42..."
```

**Heuristics for Filtering**:
- Variable names: `point`, `aPoint`, `pt` → Point class
- Type hints: `"aPoint <Point>"`
- Method context: geometric operations → likely Point

---

### 4. Generating Package Overviews

**Goal**: Understand the overall structure and purpose of a package.

**Primary Tools**: `list_classes`, `get_class_comment`

**Workflow**:

```
1. Get all classes: list_classes(package_name)
2. For each class (top 10-20 if many):
   a. get_class_comment(class_name)
   b. Optionally: get_class_source(class_name) for structure
3. Alternative: Read src/PackageName/ Tonel files directly
4. Generate overview: purpose, main classes, relationships, entry points
```

**Example**: For "What is Collections-Streams overview?"

```smalltalk
# Step 1: Get classes
list_classes("Collections-Streams")
→ ["Stream", "ReadStream", "WriteStream", "FileStream", ...]

# Step 2: Get comments
get_class_comment("Stream")
→ "I represent an accessor for a sequence of objects."

get_class_comment("ReadStream")
→ "I can only read from my collection."

# Step 3: Generate overview
"Collections-Streams provides streaming access to collections and files.
Main classes:
- Stream: Abstract base for sequential access
- ReadStream/WriteStream: Read-only and write-only streams
- FileStream: External file streaming
Entry points: ReadStream on: aCollection"
```

**Tips**:
- Focus on top 10-20 classes for large packages
- Group related classes (base + implementations)
- Look for test classes for usage examples

---

### 5. Handling Ambiguous Names

**Goal**: Handle incomplete or incorrect class/method names.

**Primary Tools**: `search_classes_like`, `search_methods_like`

**Workflow**:

```
1. Fuzzy search:
   - Classes: search_classes_like(partial_name)
   - Methods: search_methods_like(partial_name)
2. If multiple matches: present options, ask for clarification
3. If no matches: try broader search
4. Once identified: proceed with normal workflow
```

**Example**: User asks "How to use Dict?"

```smalltalk
search_classes_like("Dict")
→ ["Dictionary", "IdentityDictionary", "SmallDictionary", ...]

Present options:
"Found several classes matching 'Dict':
1. Dictionary - Main dictionary class
2. IdentityDictionary - Uses identity comparison
3. SmallDictionary - Optimized for small sets
Which one?"
```

**Common Partial Names**:
- "Str" → String, Stream, StringBuilder
- "Col" → Collection, Color, Column
- "Arr" → Array, OrderedCollection, ByteArray

---

## Tool Reference

### Class Inspection

- **`get_class_comment(class_name)`** → Class documentation
- **`get_class_source(class_name)`** → Complete class definition

### Method Inspection

- **`get_method_source(class_name, method_name, is_class_method=false)`** → Method source code

### Search Tools

- **`search_references_to_class(class_name)`** → Methods referencing this class
- **`search_references(method_name_or_symbol)`** → Methods calling this method
- **`search_classes_like(query)`** → Fuzzy class name search (prefix match)
- **`search_methods_like(query)`** → Fuzzy method name search (prefix match)

### Package Inspection

- **`list_classes(package_name)`** → All classes in package
- **`list_methods(package_name)`** → All methods (format: "Class>>#method")

---

## Best Practices

1. **Verify Names First**: Use fuzzy search when uncertain about exact names
2. **Limit Results**: Focus on top 5-10 references, not hundreds
3. **Prioritize Examples**: Show `exampleXXX` methods first if available
4. **Trust Class Comments**: Use them as primary documentation
5. **Provide Context**: Include surrounding code, not just the method call line
6. **Handle Polymorphism**: Use variable names and context to filter
7. **Be Explicit**: Clarify ambiguity with user rather than guessing

---

## Common Pitfalls

**Pitfall 1: Assuming Exact Names**
- Problem: User says "Point" but means "PointArray"
- Solution: Always use `search_classes_like` to verify

**Pitfall 2: Reference Overload**
- Problem: 500 search results overwhelming analysis
- Solution: Limit to top 5-10, filter by relevance

**Pitfall 3: Polymorphic Confusion**
- Problem: Same method in multiple classes
- Solution: Check variable names and method context

**Pitfall 4: Missing Examples**
- Problem: Only checking instance-side or one category
- Solution: Check both class-side and instance-side

**Pitfall 5: Ignoring Package Context**
- Problem: Confused by similar names in different packages
- Solution: Always include package name in search

---

## Summary

Systematic workflows for understanding Smalltalk code usage:

1. **Class Responsibilities**: `get_class_comment`
2. **Class Usage**: `search_references_to_class` + example methods
3. **Method Usage**: `search_references` + context filtering
4. **Package Overviews**: `list_classes` + comments
5. **Ambiguous Names**: `search_classes_like` / `search_methods_like`

**Key Principle**: Verify → Search → Inspect → Synthesize
