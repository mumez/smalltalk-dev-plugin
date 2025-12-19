# Usage Analysis Techniques

Comprehensive techniques for analyzing how Smalltalk code is used in practice.

## Tool Reference

### Class Inspection Tools

#### get_class_comment(class_name)
**Purpose**: Get class documentation and responsibility description
**Returns**: String with class comment
**Usage**: Primary source for understanding class purpose

```
mcp__smalltalk-interop__get_class_comment: 'Point'
→ "I represent an x-y pair of numbers usually designating a location on the screen."
```

**Best for**: Understanding what a class does

#### get_class_source(class_name)
**Purpose**: Get complete class definition including instance variables
**Returns**: Full class definition in Tonel format
**Usage**: Understanding class structure and variables

```
mcp__smalltalk-interop__get_class_source: 'Point'
→ Complete class definition with x, y instance variables
```

**Best for**: Understanding class implementation details

### Method Inspection Tools

#### get_method_source(class_name, method_name, is_class_method=false)
**Purpose**: Get source code of a specific method
**Parameters**:
- `class_name` - Class containing the method
- `method_name` - Method selector
- `is_class_method` - true for class-side methods, false for instance methods

```
mcp__smalltalk-interop__get_method_source: class: 'Point' method: 'distanceTo:' is_class_method: false
→ Method source showing distance calculation
```

**Best for**: Understanding method implementation

### Search Tools

#### search_references_to_class(class_name)
**Purpose**: Find all methods that reference a class
**Returns**: List of references with package, class, method
**Usage**: Discovering how a class is used throughout codebase

```
mcp__smalltalk-interop__search_references_to_class: 'Point'
→ [
  {"package": "Graphics", "class": "Rectangle", "method": "center"},
  {"package": "Morphic", "class": "Morph", "method": "position:"}
]
```

**Best for**: Finding class usage patterns

#### search_references(method_name_or_symbol)
**Purpose**: Find all methods that call a specific method
**Returns**: List of references with package, class, method
**Note**: Finds all classes with methods of this name (polymorphism)

```
mcp__smalltalk-interop__search_references: 'distanceTo:'
→ All methods calling distanceTo: (could be Point, Vector, etc.)
```

**Best for**: Finding method usage across all classes

#### search_classes_like(query)
**Purpose**: Fuzzy search for classes by name prefix
**Returns**: List of matching class names
**Usage**: When exact class name is uncertain

```
mcp__smalltalk-interop__search_classes_like: 'Dict'
→ ["Dictionary", "IdentityDictionary", "SmallDictionary", ...]
```

**Best for**: Discovering classes by partial name

#### search_methods_like(query)
**Purpose**: Fuzzy search for methods by name prefix
**Returns**: List of matching method selectors
**Usage**: Finding methods when exact name uncertain

```
mcp__smalltalk-interop__search_methods_like: 'example'
→ ["exampleGrid", "example1", "exampleSimple", ...]
```

**Best for**: Finding example methods or partial method names

### Package Inspection Tools

#### list_classes(package_name)
**Purpose**: Get all classes in a package
**Returns**: List of class names
**Usage**: Package structure discovery

```
mcp__smalltalk-interop__list_classes: 'Collections-Streams'
→ ["Stream", "ReadStream", "WriteStream", "FileStream", ...]
```

**Best for**: Understanding package contents

#### list_methods(package_name)
**Purpose**: Get all methods in a package
**Returns**: List in format "ClassName>>#methodName"
**Usage**: Finding methods within package scope

```
mcp__smalltalk-interop__list_methods: 'Collections-Streams'
→ ["Stream>>#next", "ReadStream>>#peek", ...]
```

**Best for**: Comprehensive package method listing

---

## Analysis Patterns

### Pattern 1: Top-Down Analysis (Class → Usage)

Start with class definition, then find how it's used.

**Steps**:
1. Get class comment for responsibility
2. Get class source for structure
3. Search references to class for usage
4. Analyze reference patterns

**When to use**: Learning a new class

**Example workflow**:
```
1. get_class_comment('Point')
2. get_class_source('Point')
3. search_references_to_class('Point')
4. get_method_source for top 5 references
5. Synthesize usage patterns
```

### Pattern 2: Bottom-Up Analysis (Method → Context)

Start with method name, find where and how it's used.

**Steps**:
1. Verify method exists with search_methods_like
2. Find all references
3. Get source for each reference
4. Filter by context and variable names
5. Extract usage patterns

**When to use**: Understanding specific method usage

**Example workflow**:
```
1. search_methods_like('distanceTo')
2. search_references('distanceTo:')
3. get_method_source for relevant references
4. Filter for target class (e.g., Point)
5. Show usage examples
```

### Pattern 3: Example-Driven Analysis

Find and use example methods as primary documentation.

**Steps**:
1. Search for example methods
2. Get source of examples
3. Extract usage patterns from examples
4. Supplement with reference search if needed

**When to use**: Classes with good example methods

**Example workflow**:
```
1. search_methods_like('example')
2. Filter for target class
3. get_method_source for each example
4. Present examples as usage guide
```

### Pattern 4: Package-Level Analysis

Understand entire package structure and relationships.

**Steps**:
1. List all classes in package
2. Get comment for each major class
3. Identify class relationships
4. Find entry points and main classes

**When to use**: Understanding new package or codebase area

**Example workflow**:
```
1. list_classes('PackageName')
2. get_class_comment for top classes
3. Identify base classes and implementations
4. Generate package overview
```

---

## Advanced Techniques

### Handling Polymorphism

Many methods exist in multiple classes (e.g., `distanceTo:` in Point, Vector, etc.).

**Filtering strategies**:

1. **Variable naming**:
   - `aPoint` → Point class
   - `vector` → Vector class
   - `rect` → Rectangle class

2. **Type hints in comments**:
   ```smalltalk
   method: aPoint
       "aPoint <Point>"
   ```

3. **Context clues**:
   - Geometric operations → likely Point
   - Collection operations → likely Collection/Array
   - Stream operations → likely Stream

4. **Method categories**:
   - `accessing` → basic operations
   - `arithmetic` → numeric operations
   - `comparing` → comparison methods

### Reference Filtering

When `search_references` returns hundreds of results:

**Strategies**:

1. **Limit by relevance**:
   - Select top 5-10 most relevant
   - Prioritize core packages over tests
   - Focus on common use cases

2. **Group by pattern**:
   - Constructor patterns
   - Accessor patterns
   - Operation patterns

3. **Filter by package**:
   - Look at main package first
   - Check test package for examples
   - Skip internal implementation packages

### Example Method Discovery

Example methods follow conventions:

**Naming patterns**:
- `exampleSimple` - Basic usage
- `example1`, `example2` - Multiple examples
- `exampleWithData` - Data-driven example
- `exampleGrid` - Specific scenario

**Location**:
- Always class-side (class methods)
- Usually in `examples` protocol/category
- May be in `documentation` category

**Finding examples**:
```
1. list_methods('PackageName')
2. Filter for patterns: example*, Example*
3. Get source for matches
4. Verify they're class-side
```

### Dealing with Ambiguity

When class or method name is unclear:

**Resolution workflow**:

1. **Fuzzy search**:
   ```
   search_classes_like('Str')
   → [String, Stream, StringBuilder, ...]
   ```

2. **Present options**:
   ```
   "Found 3 classes matching 'Str':
   1. String - Character sequences
   2. Stream - Sequential access
   3. StringBuilder - Efficient string building
   Which one are you interested in?"
   ```

3. **Use context**:
   - If user mentioned "text", likely String
   - If mentioned "read/write", likely Stream
   - If mentioned "performance", likely StringBuilder

4. **Confirm before proceeding**:
   - Always verify with user
   - Don't assume based on partial match

---

## Best Practices

### 1. Verify Before Searching

Always verify class/method names with fuzzy search when uncertain:

✅ **Good**:
```
1. search_classes_like('Dict')
2. Confirm: "Dictionary"
3. get_class_comment('Dictionary')
```

❌ **Bad**:
```
1. get_class_comment('Dict')  # Fails
```

### 2. Limit Search Results

Focus on top 5-10 results for analysis:

✅ **Good**:
```
references = search_references_to_class('Point')
# Analyze top 5 most relevant
```

❌ **Bad**:
```
# Analyze all 500 references
```

### 3. Prioritize Example Methods

Check for examples before searching references:

✅ **Good**:
```
1. search_methods_like('example')
2. If examples exist, use them
3. If not, search references
```

❌ **Bad**:
```
1. Immediately search all references
```

### 4. Provide Context

Show surrounding code, not just the line:

✅ **Good**:
```smalltalk
nearestMorphAt: aPoint
    | distance nearest |
    distance := self position distanceTo: aPoint.
    nearest := self findNearest.
    ^ nearest
```

❌ **Bad**:
```smalltalk
distance := self position distanceTo: aPoint.
```

### 5. Handle Polymorphism Explicitly

Clarify which class when multiple matches:

✅ **Good**:
```
"distanceTo: found in 3 classes:
1. Point (geometric distance)
2. Vector (vector distance)
3. GeoPoint (geographic distance)

For Point, usage: point1 distanceTo: point2"
```

❌ **Bad**:
```
"distanceTo: calculates distance" (which class?)
```

### 6. Trust Class Comments

Class comments are authoritative documentation:

✅ **Good**:
```
1. Start with get_class_comment
2. Use as primary source
3. Supplement with usage examples
```

❌ **Bad**:
```
1. Ignore comments
2. Only look at implementation
```

### 7. Be Explicit About Ambiguity

Ask for clarification rather than guessing:

✅ **Good**:
```
"Found 2 possible matches:
1. Stream (sequential access)
2. FileStream (file I/O)
Which one?"
```

❌ **Bad**:
```
"Assuming you meant Stream..." (might be wrong)
```

---

## Common Pitfalls

### Pitfall 1: Assuming Exact Names

**Problem**: User says "Point" but means "PointArray"

**Why it happens**: Users don't always know exact class names

**Solution**:
```
Always verify with search_classes_like:
1. search_classes_like('Point')
2. Show all matches
3. Confirm with user
```

### Pitfall 2: Reference Overload

**Problem**: 500 search results overwhelming analysis

**Why it happens**: Popular classes/methods have many references

**Solution**:
```
Limit and filter:
1. Take top 10 results
2. Filter by relevance
3. Group by pattern
4. Present summary, not all details
```

### Pitfall 3: Polymorphic Confusion

**Problem**: Same method in multiple classes, wrong class analyzed

**Why it happens**: Method names are not unique

**Solution**:
```
Use context clues:
1. Check variable names
2. Look at method category
3. Consider package context
4. Verify with user if uncertain
```

### Pitfall 4: Missing Examples

**Problem**: Only checking instance-side or one category

**Why it happens**: Example methods are class-side, easy to miss

**Solution**:
```
Check both sides:
1. search_methods_like('example')
2. Verify is_class_method: true
3. Check 'examples' and 'documentation' categories
```

### Pitfall 5: Ignoring Package Context

**Problem**: Confused by similar names in different packages

**Why it happens**: Same class names can exist in different packages

**Solution**:
```
Always include package:
1. Note package name in results
2. Clarify which package if multiple matches
3. Consider package context in analysis
```

### Pitfall 6: Over-Analyzing

**Problem**: Spending too much time on implementation details

**Why it happens**: Getting lost in code instead of focusing on usage

**Solution**:
```
Stay focused on usage:
1. What does it do? (class comment)
2. How to create? (constructors)
3. Common operations? (frequent methods)
4. Don't dive into implementation
```

### Pitfall 7: No Synthesis

**Problem**: Showing raw search results without interpretation

**Why it happens**: Forgetting to synthesize findings

**Solution**:
```
Always synthesize:
1. Gather information
2. Identify patterns
3. Create usage guide
4. Provide clear examples
```

---

## Quick Reference

### Analysis Workflow

```
Question → Verify names → Search → Inspect → Filter → Synthesize → Present
```

### Tool Selection Matrix

| Goal | Primary Tool | Secondary Tool |
|------|--------------|----------------|
| Class responsibility | get_class_comment | get_class_source |
| Class usage | search_references_to_class | list_methods |
| Method usage | search_references | get_method_source |
| Package overview | list_classes | get_class_comment |
| Find examples | search_methods_like | get_method_source |
| Fuzzy search | search_classes_like | search_methods_like |

### Common Analysis Patterns

| User Question | Analysis Pattern |
|---------------|------------------|
| "What does X do?" | Top-Down (class → usage) |
| "How to use method Y?" | Bottom-Up (method → context) |
| "Show examples of X" | Example-Driven |
| "What's in package Z?" | Package-Level |

---

## Summary

Key principles for effective usage analysis:

1. **Verify first** - Always confirm names before searching
2. **Limit scope** - Focus on top 5-10 most relevant results
3. **Prioritize examples** - Use example methods when available
4. **Provide context** - Show surrounding code, not isolated lines
5. **Handle polymorphism** - Use variable names and context to filter
6. **Trust documentation** - Class comments are authoritative
7. **Synthesize findings** - Don't just dump search results
8. **Be explicit** - Clarify ambiguity, don't assume

Remember: The goal is understanding usage, not analyzing implementation details.
