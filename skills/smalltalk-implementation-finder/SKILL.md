---
name: smalltalk-implementation-finder
description: Finding and analyzing method implementations across class hierarchies in Smalltalk
model_selection:
  enabled: false
triggers:
  - "find implementors"
  - "who implements"
  - "implementations of"
  - "abstract method"
  - "subclass responsibility"
  - "implementation pattern"
  - "refactoring opportunity"
  - "method signature"
  - "implementation idiom"
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

This skill helps you find and analyze method implementations across class hierarchies. It's particularly useful for understanding abstract methods, identifying implementation patterns, discovering refactoring opportunities, and assessing the impact of signature changes.

## Core Use Cases

### 1. Understanding Abstract Method Implementations

**Goal**: See how abstract methods (marked with `self subclassResponsibility`) are concretely implemented across subclasses.

**Primary Tool**: `mcp__smalltalk-interop__search_implementors`

**Workflow**:

```
1. Find all implementors of the abstract method:
   search_implementors(method_name)

2. Get the superclass hierarchy to filter relevant implementations:
   eval("ClassA allSubclasses")

3. For each relevant implementor:
   get_method_source(class_name, method_name)

4. Analyze patterns and learn implementation approaches
```

**Example**: Understanding how to implement `printOn:`

```smalltalk
# Step 1: Find implementors
search_implementors("printOn:")
→ Returns hundreds of implementations

# Step 2: Get hierarchy context
eval("Object allSubclasses select: [:c | c name includesSubstring: 'Collection']")
→ Focus on Collection hierarchy

# Step 3: Examine implementations
get_method_source("Array", "printOn:")
→ Shows: "printOn: aStream
          aStream nextPutAll: '#('.
          self do: [:each | each printOn: aStream. aStream space].
          aStream nextPutAll: ')'"

get_method_source("Dictionary", "printOn:")
→ Shows: "printOn: aStream
          aStream nextPutAll: 'a Dictionary('.
          self associations do: [:assoc |
            assoc key printOn: aStream.
            aStream nextPutAll: '->'.
            assoc value printOn: aStream] separatedBy: [aStream space].
          aStream nextPut: $)"

# Step 4: Learn pattern
"Common pattern for printOn::
- Start with class identifier
- Use recursive printOn: for elements
- Use separators between items
- End with closing delimiter"
```

**Why This Helps**: When implementing an abstract method in your subclass, seeing existing implementations provides:
- Idiomatic patterns for that method
- Common edge cases to handle
- Expected behavior and format

---

### 2. Learning Implementation Idioms

**Goal**: Understand common patterns and idioms for specific method implementations.

**Primary Tool**: `mcp__smalltalk-interop__search_implementors`

**Common Idioms to Discover**:

#### Idiom 1: `hash` Methods Use `bitXor:`

```smalltalk
search_implementors("hash")
→ Find multiple implementations

get_method_source("Point", "hash")
→ "hash ^ x hash bitXor: y hash"

get_method_source("Association", "hash")
→ "hash ^ key hash bitXor: value hash"

# Pattern: Combine element hashes with bitXor:
```

#### Idiom 2: `initialize` Calls Super First

```smalltalk
search_implementors("initialize")

get_method_source("OrderedCollection", "initialize")
→ "initialize
     super initialize.
     array := Array new: 10"

get_method_source("WriteStream", "initialize")
→ "initialize
     super initialize.
     position := 0"

# Pattern: Always call super initialize first
```

**Workflow for Learning Idioms**:

```
1. Search for method implementors: search_implementors(method_name)
2. Sample 5-10 implementations from well-known classes
3. Get source for each: get_method_source(class_name, method_name)
4. Identify common patterns:
   - Call sequences (e.g., super first)
   - Helper methods used (e.g., bitXor:)
   - Conditional checks (e.g., class matching)
5. Document the idiom for your implementation
```

---

### 3. Assessing Signature Change Impact

**Goal**: Understand how many implementations would be affected by changing a method signature.

**Primary Tools**: `mcp__smalltalk-interop__search_implementors`, `mcp__smalltalk-interop__search_references`

**Workflow**:

```
1. Find all implementors:
   search_implementors(method_name)

2. Find all callers:
   search_references(method_name)

3. Assess impact:
   - Count of implementors = methods to update
   - Count of references = call sites to update
   - Review critical callers to understand usage

4. Decision: Is the change worth the impact?
```

**Example**: Changing `at:put:` signature

```smalltalk
# Step 1: Find implementors
search_implementors("at:put:")
→ Returns 50+ implementations (Dictionary, Array, etc.)

# Step 2: Find references
search_references("at:put:")
→ Returns 500+ call sites

# Step 3: Assess
"Changing at:put: signature would affect:
- 50+ class implementations to update
- 500+ call sites to update
- Core collection protocol (very high impact)

Recommendation: Do NOT change this signature.
Consider adding a new method instead."
```

**Use Case**: Before renaming or changing signatures, use this workflow to:
- Estimate refactoring effort
- Identify critical dependencies
- Make informed decisions about API changes

---

### 4. Discovering Refactoring Opportunities

**Goal**: Find duplicate or similar implementations that could be consolidated.

**Primary Tool**: `mcp__smalltalk-interop__search_implementors`

**Workflow**:

```
1. Find all implementors: search_implementors(method_name)
2. Get source for implementors in same hierarchy
3. Compare implementations:
   - Identical implementations = can be pulled up to superclass
   - Similar implementations = can be parameterized and shared
4. Propose refactoring
```

**Example**: Finding duplicate `isEmpty` implementations

```smalltalk
# Step 1: Find implementors
search_implementors("isEmpty")
→ Returns implementations in multiple Collection subclasses

# Step 2: Get sources
get_method_source("Array", "isEmpty")
→ "isEmpty ^ self size = 0"

get_method_source("OrderedCollection", "isEmpty")
→ "isEmpty ^ self size = 0"

get_method_source("Set", "isEmpty")
→ "isEmpty ^ self size = 0"

# Step 3: Identify duplication
"All implementations are identical: ^ self size = 0

Refactoring opportunity:
Move isEmpty to Collection superclass and remove from subclasses.
This eliminates 3+ duplicate implementations."
```

**Red Flags for Refactoring**:
- Exact same implementation in multiple subclasses → Pull up to superclass
- Similar implementations with small variations → Parameterize the variation
- Many classes overriding with `self subclassResponsibility` → Consider redesign

---

### 5. Narrowing Usage Search with Implementors

**Goal**: When searching for method usage, use implementors to narrow down which class's method is being called.

**Primary Tools**: `mcp__smalltalk-interop__search_implementors`, `mcp__smalltalk-interop__search_references`, `mcp__smalltalk-interop__get_method_source`

**Problem**: `search_references` returns all calls to any method with that name, regardless of which class implements it.

**Solution**: Combine implementor search with reference analysis.

**Workflow**:

```
1. Find implementors: search_implementors(method_name)
   → Get list of classes that implement this method

2. Find all references: search_references(method_name)
   → Get all call sites

3. Filter references by implementor context:
   - Get source for each reference
   - Match variable types with implementor classes
   - Focus on relevant call sites

4. Present filtered usage
```

**Example**: Finding usage of `Collection>>select:`

```smalltalk
# Step 1: Find implementors
search_implementors("select:")
→ ["Collection", "Dictionary", "Interval", "Set", ...]

# Step 2: Find references
search_references("select:")
→ Returns 1000+ call sites

# Step 3: Filter by checking receiver types
get_method_source("SomeClass", "filterData")
→ "filterData
     ^ self dataArray select: [:item | item isValid]"
→ "dataArray" is an Array (Collection subclass) ✓

get_method_source("ConfigManager", "activeSettings")
→ "activeSettings
     ^ self settingsDict select: [:setting | setting isActive]"
→ "settingsDict" is a Dictionary ✓ (but Dictionary has specialized select:)

# Step 4: Present filtered results
"Collection>>select: is used primarily with:
- Arrays for filtering elements
- OrderedCollections for filtering sequences
- Sets for subset selection

Dictionary>>select: has specialized behavior for key-value filtering."
```

**Why This Helps**: Narrowing by implementors reduces noise and helps you understand:
- Which variant of a polymorphic method is being used
- Context-specific behavior differences
- Actual usage patterns per class

---

## Advanced Technique: Scoping by Class Hierarchy

**Challenge**: Smalltalk allows unrelated classes to define methods with the same signature. To focus on relevant implementations, scope your search to a specific class hierarchy.

**Workflow**:

```
1. Get the class hierarchy:
   eval("ClassA subclasses")         → Direct subclasses
   eval("ClassA allSubclasses")      → All descendants
   eval("ClassA superclass")         → Direct superclass
   eval("ClassA allSuperclasses")    → All superclasses

2. Find all implementors:
   search_implementors(method_name)

3. Filter implementors by hierarchy:
   - Keep only classes in the hierarchy from step 1
   - Discard unrelated classes

4. Analyze only relevant implementations
```

**Example**: Focusing on `Stream` hierarchy for `next`

```smalltalk
# Step 1: Get Stream hierarchy
eval("Stream allSubclasses")
→ ["ReadStream", "WriteStream", "FileStream", "SocketStream", ...]

# Step 2: Find all implementors
search_implementors("next")
→ Returns 100+ implementations (including Iterator, Random, etc.)

# Step 3: Filter to Stream hierarchy only
# Manually filter results to Stream subclasses:
["ReadStream", "WriteStream", "FileStream", "SocketStream"]

# Step 4: Analyze filtered set
get_method_source("ReadStream", "next")
→ Stream-specific implementation

get_method_source("FileStream", "next")
→ File-specific implementation

# Result: Focused analysis on Stream variants only
```

**When to Use Hierarchy Scoping**:
- Implementing an abstract method in a specific hierarchy
- Understanding variations within a family of classes
- Avoiding confusion from unrelated classes with same method names
- Refactoring within a specific subsystem

---

## Tool Reference

### Primary Tool

- **`search_implementors(method_name)`**
  - Returns: List of all classes that implement this method
  - Format: `[{class, method, package}, ...]`
  - Use: Finding all implementations of a method selector

### Supporting Tools

- **`get_method_source(class_name, method_name, is_class_method=false)`**
  - Returns: Method source code
  - Use: Inspecting individual implementations

- **`eval(code)`**
  - Returns: Result of Smalltalk code execution
  - Use: Getting class hierarchies, filtering collections

- **`search_references(method_name)`**
  - Returns: All call sites of this method
  - Use: Assessing impact, finding usage

- **`get_class_source(class_name)`**
  - Returns: Complete class definition
  - Use: Understanding class structure and hierarchy

---

## Best Practices

1. **Sample Representative Implementations**: Don't examine all 500 implementations. Sample 5-10 from well-known, well-designed classes.

2. **Focus on Your Hierarchy**: Use `eval` to get relevant subclasses and filter implementors to that hierarchy.

3. **Check Both Class and Instance Side**: Use `is_class_method: true` to check class-side implementations when relevant.

4. **Combine with References**: For complete understanding, check both implementors (who defines it) and references (who uses it).

5. **Document Idioms**: When you discover a common pattern, document it for your team as a coding standard.

6. **Assess Impact Before Changing**: Always run implementor/reference search before changing method signatures.

---

## Common Pitfalls

**Pitfall 1: Too Many Results**
- Problem: `search_implementors("at:")` returns 200+ classes
- Solution: Filter by hierarchy or sample from major classes only

**Pitfall 2: Ignoring Class Hierarchy**
- Problem: Comparing implementations from unrelated classes
- Solution: Use `eval` to get relevant hierarchy first

**Pitfall 3: Not Checking Super Implementation**
- Problem: Missing context from parent class implementation
- Solution: Check superclass implementation when method calls `super`

**Pitfall 4: Only Looking at One Example**
- Problem: Single implementation may have quirks
- Solution: Review 3-5 implementations to find common patterns

**Pitfall 5: Forgetting Class-Side Methods**
- Problem: Missing class-side implementations
- Solution: Check `is_class_method: true` for class constructors and utilities

---

## Summary

Use `search_implementors` to:

1. **Learn Abstract Method Implementation**: See how `subclassResponsibility` is fulfilled
2. **Discover Implementation Idioms**: Identify common patterns (hash with bitXor:, initialize with super, etc.)
3. **Assess Change Impact**: Count affected implementations before signature changes
4. **Find Refactoring Opportunities**: Spot duplicate implementations to consolidate
5. **Narrow Usage Search**: Combine with references to filter by class context

**Key Workflow**:
1. Find implementors → 2. Filter by hierarchy → 3. Sample implementations → 4. Identify patterns → 5. Apply learnings

**Key Principle**: Learn from existing implementations before creating your own.
