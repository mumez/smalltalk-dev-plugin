# Usage Analysis Scenarios

Real-world examples of discovering and understanding Smalltalk code usage.

## Scenario 1: Understanding Point Class

### User Question
"What is Point responsible for? How do I use it?"

### Analysis Process

**Step 1: Discover Class Responsibility**

```
mcp__smalltalk-interop__get_class_comment: 'Point'
```

**Result**:
```
"I represent an x-y pair of numbers usually designating a location on the screen.
I have methods for arithmetic, comparing, and transforming."
```

**Step 2: Check for Example Methods**

```
mcp__smalltalk-interop__search_methods_like: 'example'
```

**Result**:
```
["Point class>>exampleGrid", "Point class>>exampleSimple", ...]
```

**Step 3: Get Example Source**

```
mcp__smalltalk-interop__get_method_source: class: 'Point' method: 'exampleGrid' is_class_method: true
```

**Result**:
```smalltalk
exampleGrid
    "Point exampleGrid"
    | grid |
    grid := (1 to: 10) collect: [:x |
        (1 to: 10) collect: [:y | x@y]
    ].
    ^ grid
```

**Step 4: Find Real-World Usage**

```
mcp__smalltalk-interop__search_references_to_class: 'Point'
```

**Result** (top 5):
```
1. Rectangle>>center - Geometric center calculation
2. Morph>>position: - Setting morph position
3. Canvas>>drawLine:to: - Drawing operations
4. Form>>extent - Image dimensions
5. Cursor>>hotSpot: - Cursor positioning
```

**Step 5: Inspect Usage Examples**

```
mcp__smalltalk-interop__get_method_source: class: 'Rectangle' method: 'center'
```

**Result**:
```smalltalk
center
    "Return the center point of the rectangle"
    ^ (origin + corner) // 2
```

**Key pattern**: Point arithmetic with + and //

**Step 6: Synthesize Usage Guide**

**Response to user**:
```
Point represents x-y coordinates on screen.

Creation:
- Using @ operator: 100@200
- Using constructor: Point x: 100 y: 200

Common operations:
- Get coordinates: point x, point y
- Arithmetic: point1 + point2, point1 - point2
- Integer division: point // 2
- Comparison: point1 < point2

Example:
  center := (100@100 + 200@200) // 2.
  "=> 150@150"
```

---

## Scenario 2: Finding Method Usage with Polymorphism

### User Question
"How to use distanceTo: method?"

### Challenge
Multiple classes implement `distanceTo:` (Point, Vector, GeoPoint).

### Analysis Process

**Step 1: Verify Method Exists**

```
mcp__smalltalk-interop__search_methods_like: 'distanceTo'
```

**Result**:
```
["Point>>distanceTo:", "Vector>>distanceTo:", "GeoPoint>>distanceTo:", ...]
```

**Multiple matches found** - Need to determine which one user wants.

**Step 2: Find All References**

```
mcp__smalltalk-interop__search_references: 'distanceTo:'
```

**Result** (25 matches across different classes)

**Step 3: Filter by Context**

Get source for top matches and look for context clues:

**Reference 1**: Morph>>nearestMorphAt:
```smalltalk
nearestMorphAt: aPoint
    | distance nearest |
    distance := self position distanceTo: aPoint.
    "Variable 'aPoint' suggests Point class"
    ...
```

**Reference 2**: Circle>>contains:
```smalltalk
contains: aPoint
    ^ center distanceTo: aPoint <= radius
    "Variables 'center' and 'aPoint' are Points"
```

**Reference 3**: PathFinder>>shortestPath
```smalltalk
shortestPath
    | distance |
    distance := startNode position distanceTo: endNode position.
    "Context: geometric path finding → Point"
```

**Step 4: Identify Pattern**

All top references use Point, not Vector or GeoPoint.
Context: geometric operations, screen coordinates.

**Step 5: Get Point Implementation**

```
mcp__smalltalk-interop__get_method_source: class: 'Point' method: 'distanceTo:'
```

**Result**:
```smalltalk
distanceTo: aPoint
    "Answer the distance from this point to aPoint"
    | dx dy |
    dx := aPoint x - x.
    dy := aPoint y - y.
    ^ (dx * dx + (dy * dy)) sqrt
```

**Step 6: Synthesize Response**

**Response to user**:
```
Point>>distanceTo: calculates Euclidean distance between two points.

Usage:
  distance := point1 distanceTo: point2

Returns: Float (distance in coordinate units)

Example:
  | p1 p2 distance |
  p1 := 100@100.
  p2 := 200@200.
  distance := p1 distanceTo: p2.
  "=> 141.421... (sqrt of 20000)"

Note: There are also distanceTo: methods in Vector and GeoPoint classes.
For geometric screen coordinates, use Point.
```

---

## Scenario 3: Package Overview Generation

### User Question
"What's in the Collections-Streams package?"

### Analysis Process

**Step 1: List All Classes**

```
mcp__smalltalk-interop__list_classes: 'Collections-Streams'
```

**Result** (15 classes):
```
["Stream", "PositionableStream", "ReadStream", "WriteStream",
 "ReadWriteStream", "FileStream", "StandardFileStream",
 "LimitedWriteStream", "RWBinaryOrTextStream", ...]
```

**Step 2: Get Key Class Comments**

**Stream** (base class):
```
mcp__smalltalk-interop__get_class_comment: 'Stream'
→ "I represent an accessor for a sequence of objects. This sequence is referred to as my 'contents'."
```

**ReadStream**:
```
mcp__smalltalk-interop__get_class_comment: 'ReadStream'
→ "I represent a stream that can only read from its collection."
```

**WriteStream**:
```
mcp__smalltalk-interop__get_class_comment: 'WriteStream'
→ "I represent a stream that can write into its collection."
```

**FileStream**:
```
mcp__smalltalk-interop__get_class_comment: 'FileStream'
→ "I represent a stream for reading and writing files."
```

**Step 3: Identify Class Hierarchy**

```
Stream (abstract)
  ├─ PositionableStream
  │   ├─ ReadStream
  │   ├─ WriteStream
  │   └─ ReadWriteStream
  └─ FileStream
      └─ StandardFileStream
```

**Step 4: Find Entry Points**

Look for class-side constructors:

```
mcp__smalltalk-interop__list_methods: 'Collections-Streams'
→ Filter for class-side creation methods:
  - ReadStream class>>on:
  - WriteStream class>>on:
  - FileStream class>>fileNamed:
```

**Step 5: Synthesize Package Overview**

**Response to user**:
```
Collections-Streams Package Overview

Purpose: Provides streaming access to collections and files for sequential reading/writing.

Main Classes:

1. Stream (Abstract Base)
   - Defines protocol for sequential access
   - Position tracking, reading, writing

2. PositionableStream
   - Adds positioning (skip, peek, reset)
   - Base for collection streams

3. ReadStream
   - Read-only access to collections
   - Usage: ReadStream on: aCollection

4. WriteStream
   - Write-only access to collections
   - Usage: WriteStream on: (String new)

5. ReadWriteStream
   - Both reading and writing
   - Maintains position for both operations

6. FileStream
   - External file access
   - Binary and text modes
   - Usage: FileStream fileNamed: 'file.txt'

Entry Points:
- ReadStream on: aCollection
- WriteStream on: (String new: 100)
- FileStream fileNamed: 'path/to/file.txt'

Common Patterns:
- stream next           "Read one element"
- stream next: 10       "Read 10 elements"
- stream nextPut: item  "Write one element"
- stream contents       "Get all contents"
- stream atEnd          "Check if done"

Example Usage:
  | stream |
  stream := ReadStream on: 'Hello World'.
  stream next.     "=> $H"
  stream next: 5.  "=> 'ello '"
  stream atEnd.    "=> false"
```

---

## Scenario 4: Handling Ambiguous Names

### User Question
"How to use Dict?"

### Analysis Process

**Step 1: Fuzzy Search**

```
mcp__smalltalk-interop__search_classes_like: 'Dict'
```

**Result**:
```
["Dictionary", "IdentityDictionary", "SmallDictionary",
 "WeakKeyDictionary", "OrderedDictionary", ...]
```

**Multiple matches** - Need to clarify with user.

**Step 2: Get Brief Descriptions**

Get comments for top matches:

**Dictionary**:
```
"I associate keys with values. I store associations between keys and values,
 allowing efficient lookup by key."
```

**IdentityDictionary**:
```
"I am like Dictionary but use identity comparison (==) instead of equality (=)."
```

**SmallDictionary**:
```
"I am optimized for small number of elements. For less than 10 items,
 I'm faster than Dictionary."
```

**Step 3: Present Options to User**

**Response**:
```
Found several classes matching 'Dict':

1. Dictionary (most common)
   - General-purpose key-value storage
   - Uses equality (=) for key comparison
   - Best for: Most use cases

2. IdentityDictionary
   - Uses identity (==) for comparison
   - Best for: When keys are objects, not values

3. SmallDictionary
   - Optimized for < 10 elements
   - Best for: Small, temporary collections

4. WeakKeyDictionary
   - Keys don't prevent garbage collection
   - Best for: Caches, weak references

5. OrderedDictionary
   - Maintains insertion order
   - Best for: When order matters

Which one are you interested in?
```

**Step 4: After User Clarifies**

User says: "Dictionary"

Now proceed with normal analysis:
1. Get class comment
2. Find usage examples
3. Search references
4. Synthesize usage guide

---

## Scenario 5: Finding Examples for Learning

### User Question
"Show me examples of how to use OrderedCollection"

### Analysis Process

**Step 1: Search for Example Methods**

```
mcp__smalltalk-interop__search_methods_like: 'example'
```

Filter results for OrderedCollection:
```
["OrderedCollection class>>exampleSimple",
 "OrderedCollection class>>exampleAddRemove",
 "OrderedCollection class>>exampleIteration"]
```

**Step 2: Get Example Methods**

**Example 1: Basic Usage**
```
mcp__smalltalk-interop__get_method_source: class: 'OrderedCollection' method: 'exampleSimple' is_class_method: true
```

Result:
```smalltalk
exampleSimple
    "OrderedCollection exampleSimple"
    | collection |
    collection := OrderedCollection new.
    collection add: 'first'.
    collection add: 'second'.
    collection add: 'third'.
    ^ collection
    "=> an OrderedCollection('first' 'second' 'third')"
```

**Example 2: Add and Remove**
```smalltalk
exampleAddRemove
    "OrderedCollection exampleAddRemove"
    | collection |
    collection := OrderedCollection withAll: #(1 2 3 4 5).
    collection addFirst: 0.
    collection addLast: 6.
    collection remove: 3.
    ^ collection
    "=> an OrderedCollection(0 1 2 4 5 6)"
```

**Example 3: Iteration**
```smalltalk
exampleIteration
    "OrderedCollection exampleIteration"
    | collection result |
    collection := OrderedCollection with: 1 with: 2 with: 3.
    result := collection collect: [:each | each * 2].
    ^ result
    "=> an OrderedCollection(2 4 6)"
```

**Step 3: Synthesize Response**

**Response to user**:
```
OrderedCollection Examples

1. Basic Creation and Adding:
   collection := OrderedCollection new.
   collection add: 'item'.
   collection add: 'another'.

2. Creating with Initial Elements:
   collection := OrderedCollection withAll: #(1 2 3).
   collection := OrderedCollection with: 'a' with: 'b'.

3. Adding at Specific Positions:
   collection addFirst: 'first'.
   collection addLast: 'last'.
   collection add: 'middle' afterIndex: 2.

4. Removing Elements:
   collection remove: 'item'.
   collection removeFirst.
   collection removeLast.
   collection removeAt: 3.

5. Iteration:
   collection do: [:each | Transcript show: each; cr].
   result := collection collect: [:each | each * 2].
   filtered := collection select: [:each | each > 5].

6. Accessing Elements:
   firstItem := collection first.
   lastItem := collection last.
   thirdItem := collection at: 3.

7. Testing:
   collection isEmpty.
   collection size.
   collection includes: 'item'.

Try it yourself:
   OrderedCollection exampleSimple.
   OrderedCollection exampleAddRemove.
   OrderedCollection exampleIteration.
```

---

## Scenario 6: Method Usage in Specific Context

### User Question
"How is binaryWriteStreamDo: used?"

### Analysis Process

**Step 1: Verify Method Exists**

```
mcp__smalltalk-interop__search_methods_like: 'binaryWriteStreamDo'
```

Result:
```
["FileReference>>binaryWriteStreamDo:",
 "AbstractFileReference>>binaryWriteStreamDo:", ...]
```

**Step 2: Get Method Implementation**

```
mcp__smalltalk-interop__get_method_source: class: 'FileReference' method: 'binaryWriteStreamDo:'
```

Result:
```smalltalk
binaryWriteStreamDo: aBlock
    "Pass a binary write stream on me to the block. Close the stream afterwards."
    | stream |
    stream := self binaryWriteStream.
    ^ [ aBlock value: stream ]
        ensure: [ stream close ]
```

**Pattern identified**: Resource management with ensure:

**Step 3: Find Real Usage**

```
mcp__smalltalk-interop__search_references: 'binaryWriteStreamDo:'
```

Top results:
1. ImageWriter>>saveImage:
2. DataExporter>>exportToFile:
3. CacheManager>>saveCache

**Step 4: Get Usage Examples**

```
mcp__smalltalk-interop__get_method_source: class: 'ImageWriter' method: 'saveImage:'
```

Result:
```smalltalk
saveImage: aFileName
    | fileRef |
    fileRef := aFileName asFileReference.
    fileRef binaryWriteStreamDo: [:stream |
        self writeHeader: stream.
        self writePixels: stream.
        self writeFooter: stream
    ]
```

**Pattern**: Block-based resource management

**Step 5: Synthesize Usage Guide**

**Response to user**:
```
binaryWriteStreamDo: - Safe Binary File Writing

Purpose:
Opens a binary write stream, executes your block, and ensures the stream is closed.

Signature:
  fileReference binaryWriteStreamDo: [:stream | ... ]

Benefits:
- Automatic stream closing (even on error)
- Clean resource management
- No need for manual close

Usage Pattern:
  fileRef := 'output.bin' asFileReference.
  fileRef binaryWriteStreamDo: [:stream |
      stream nextPutAll: data.
      stream nextPut: byte
  ]

Real-World Example (from ImageWriter):
  fileRef binaryWriteStreamDo: [:stream |
      self writeHeader: stream.
      self writePixels: stream.
      self writeFooter: stream
  ]

Compare with manual approach:
  "Don't do this - stream might not close on error"
  stream := fileRef binaryWriteStream.
  stream nextPutAll: data.
  stream close.

  "Do this - guaranteed to close"
  fileRef binaryWriteStreamDo: [:stream |
      stream nextPutAll: data
  ]

Related methods:
- binaryReadStreamDo: - For reading binary files
- writeStreamDo: - For text files
- readStreamDo: - For reading text files
```

---

## Common Analysis Patterns

### Pattern: Class Responsibility Discovery
1. Get class comment
2. Verify with class source
3. Find usage examples
4. Synthesize responsibility statement

### Pattern: Method Usage Discovery
1. Search for method implementations
2. Find references
3. Filter by context
4. Extract usage patterns
5. Present with examples

### Pattern: Package Exploration
1. List all classes
2. Get comments for key classes
3. Identify hierarchy
4. Find entry points
5. Generate overview

### Pattern: Example-Driven Learning
1. Search for example methods
2. Get example source
3. Execute examples
4. Explain patterns
5. Provide variations

---

## Summary

Key strategies for successful usage analysis:

1. **Start with class comments** - Authoritative source
2. **Look for examples first** - Best documentation
3. **Limit results** - Focus on top 5-10 references
4. **Provide context** - Show surrounding code
5. **Handle ambiguity** - Clarify with user
6. **Synthesize findings** - Don't just show raw data
7. **Include practical examples** - Make it usable

Remember: The goal is helping users understand HOW to use code, not explaining WHAT the code does internally.
