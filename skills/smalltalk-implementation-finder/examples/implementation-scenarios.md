# Implementation Scenarios

Real-world examples and workflows for analyzing method implementations in Pharo Smalltalk.

## Scenario 1: Implementing Abstract Method

### Problem

You need to implement `printOn:` in a new `Person` class but aren't sure of the conventional pattern.

### Solution Workflow

**Step 1: Find existing implementations**
```
search_implementors("printOn:")
→ [Object, Collection, Array, Dictionary, Point, Date, ...]
```

**Step 2: Sample representative classes**

Choose well-known, well-implemented classes:
- Point (simple domain object)
- Array (collection)
- Date (value object)

**Step 3: Get source for each**

```
get_method_source("Point", "printOn:")
→ "printOn: aStream
    aStream nextPut: $(; print: x; nextPut: $@; print: y; nextPut: $)"

get_method_source("Date", "printOn:")
→ "printOn: aStream
    aStream nextPutAll: self printString"

get_method_source("Array", "printOn:")
→ "printOn: aStream
    aStream nextPutAll: '#('.
    self do: [:each | each printOn: aStream. aStream space].
    aStream nextPutAll: ')'"
```

**Step 4: Identify pattern**

Common pattern:
- Use `nextPut:` for single characters
- Use `nextPutAll:` for strings
- Use `print:` for objects
- Recursively call `printOn:` on components

**Step 5: Apply to your class**

```smalltalk
Person>>printOn: aStream
    aStream
        nextPutAll: 'a Person(';
        nextPutAll: firstName;
        space;
        nextPutAll: lastName;
        nextPut: $)
```

### Result

Idiomatic implementation learned from existing code.

---

## Scenario 2: Assessing Refactoring Impact

### Problem

You want to change the signature of `at:put:` to `at:put:ifAbsent:` but need to assess the impact.

### Solution Workflow

**Step 1: Count implementations**
```
search_implementors("at:put:")
→ [Dictionary, Array, OrderedCollection, ByteArray, ...]
→ Count: 50+ classes
```

**Step 2: Count references**
```
search_references("at:put:")
→ [Class1>>method1, Class2>>method2, ...]
→ Count: 500+ call sites
```

**Step 3: Assess impact**

| Metric | Count | Impact |
|--------|-------|--------|
| Implementors | 50+ | HIGH |
| References | 500+ | VERY HIGH |

**Step 4: Make decision**

❌ **Don't change** - Too high impact

✅ **Alternative**: Add new method `at:put:ifAbsent:` without changing existing

### Result

Informed decision to avoid risky refactoring.

---

## Scenario 3: Finding Duplicate Code

### Problem

You suspect multiple classes have identical `isEmpty` implementations that could be consolidated.

### Solution Workflow

**Step 1: Find implementors**
```
search_implementors("isEmpty")
→ [Collection, Array, Set, OrderedCollection, Dictionary, ...]
```

**Step 2: Focus on Collection hierarchy**
```
eval("Collection allSubclasses")
→ [Array, Set, OrderedCollection, Dictionary, Bag, ...]
```

**Step 3: Get source for each**

```
get_method_source("Array", "isEmpty")
→ "isEmpty
    ^ self size = 0"

get_method_source("OrderedCollection", "isEmpty")
→ "isEmpty
    ^ self size = 0"

get_method_source("Set", "isEmpty")
→ "isEmpty
    ^ self size = 0"

get_method_source("Dictionary", "isEmpty")
→ "isEmpty
    ^ self size = 0"
```

**Step 4: Compare implementations**

All four are **identical**!

**Step 5: Refactor**

✅ **Pull up to Collection superclass**

```smalltalk
Collection>>isEmpty
    "Answer whether the receiver contains any elements."
    ^ self size = 0
```

**Step 6: Remove subclass implementations**

Remove from Array, Set, OrderedCollection, Dictionary.

### Result

Code duplication eliminated, maintenance simplified.

---

## Scenario 4: Learning Hash Implementation Idiom

### Problem

You need to implement `hash` for a new `Rectangle` class with `origin` and `corner` instance variables.

### Solution Workflow

**Step 1: Find similar classes**

Look for classes with multiple instance variables:
- Point (x, y)
- Association (key, value)
- Interval (start, stop, step)

**Step 2: Get their implementations**

```
get_method_source("Point", "hash")
→ "hash
    ^ x hash bitXor: y hash"

get_method_source("Association", "hash")
→ "hash
    ^ key hash bitXor: value hash"

get_method_source("Interval", "hash")
→ "hash
    ^ start hash bitXor: stop hash"
```

**Step 3: Identify idiom**

**Idiom**: Combine field hashes with `bitXor:`

```smalltalk
hash
    ^ field1 hash bitXor: field2 hash
```

**Step 4: Apply to your class**

```smalltalk
Rectangle>>hash
    ^ origin hash bitXor: corner hash
```

### Result

Idiomatic implementation following Smalltalk conventions.

---

## Scenario 5: Narrowing Usage Search

### Problem

You want to find all usages of `Collection>>select:` specifically, not `Dictionary>>select:` or other variants.

### Solution Workflow

**Step 1: Find all implementors**
```
search_implementors("select:")
→ [Collection, Dictionary, Interval, ...]
```

**Step 2: Find all references**
```
search_references("select:")
→ 1000+ call sites across entire system
```

**Step 3: Filter by receiver type**

Look for variable names and types:
```
# These use Collection>>select:
dataArray select: [:item | ...]
users select: [:user | ...]
items select: [:each | ...]

# These use Dictionary>>select:
settingsDict select: [:setting | ...]
config select: [:entry | ...]
```

**Step 4: Validate with context**

Check variable declarations or initialization:
```smalltalk
| dataArray |
dataArray := Array new: 10.
dataArray select: [:item | ...] "Uses Collection>>select:"
```

### Result

Precise understanding of which implementation is being called.

---

## Scenario 6: Understanding Template Method Pattern

### Problem

You see `Collection>>do:` is abstract. How do subclasses implement it?

### Solution Workflow

**Step 1: Get abstract implementation**
```
get_method_source("Collection", "do:")
→ "do: aBlock
    self subclassResponsibility"
```

**Step 2: Find concrete implementations**
```
search_implementors("do:")
→ [Array, OrderedCollection, Set, Dictionary, ...]
```

**Step 3: Compare strategies**

```
get_method_source("Array", "do:")
→ "do: aBlock
    1 to: self size do: [:index |
        aBlock value: (self at: index)]"

get_method_source("LinkedList", "do:")
→ "do: aBlock
    | node |
    node := firstNode.
    [node notNil] whileTrue: [
        aBlock value: node value.
        node := node nextNode]"

get_method_source("Set", "do:")
→ "do: aBlock
    tally = 0 ifTrue: [^ self].
    1 to: array size do: [:index |
        (array at: index) ifNotNil: [:element |
            aBlock value: element]]"
```

**Step 4: Understand pattern**

Each implementation:
- Uses its internal storage mechanism
- Calls `aBlock value:` for each element
- Same interface, different strategy

### Result

Understanding of Template Method pattern and polymorphic iteration.

---

## Scenario 7: Comparing Cross-Hierarchy Implementations

### Problem

How do different hierarchies handle equality testing?

### Solution Workflow

**Step 1: Sample different hierarchies**

```
# Numbers
get_method_source("Integer", "=")
get_method_source("Float", "=")

# Collections
get_method_source("Array", "=")
get_method_source("Set", "=")

# Domain objects
get_method_source("Point", "=")
get_method_source("Date", "=")
```

**Step 2: Identify patterns**

**Pattern 1: Type check first**
```smalltalk
Point>>= aPoint
    self class = aPoint class ifFalse: [^ false].
    ^ x = aPoint x and: [y = aPoint y]
```

**Pattern 2: Element-wise comparison**
```smalltalk
Array>>= anArray
    self size = anArray size ifFalse: [^ false].
    self with: anArray do: [:a :b |
        a = b ifFalse: [^ false]].
    ^ true
```

**Pattern 3: Primitive comparison**
```smalltalk
Integer>>= anInteger
    <primitive: 7>
    ^ super = anInteger
```

**Step 3: Choose appropriate pattern**

For your domain object, use Pattern 1 (type check first).

### Result

Understanding of different equality testing strategies.

---

## Scenario 8: Assessing Method Signature Change

### Problem

Should you change `size` to accept an optional argument?

### Solution Workflow

**Step 1: Find current implementations**
```
search_implementors("size")
→ 100+ classes
```

**Step 2: Check references**
```
search_references("size")
→ 5000+ call sites
```

**Step 3: Analyze impact**

Current signature: `size` (zero arguments)
Proposed: `size:` or `sizeWithOptions:`

**Impact**:
- All 100+ implementations need updating
- All 5000+ call sites might break
- Primitive methods involved

**Step 4: Alternative approaches**

✅ **Add new method**: `sizeWithOptions:`
✅ **Keep `size` as is**
❌ **Change signature** - Too high impact

### Result

Decision to add new method instead of changing existing.

---

## Common Workflow Patterns

### Pattern 1: Learn by Example

```
1. search_implementors(method_name)
2. Sample well-known classes
3. get_method_source for each
4. Identify common idiom
5. Apply to your implementation
```

### Pattern 2: Impact Assessment

```
1. Count implementors
2. Count references
3. Assess risk (Low/Medium/High)
4. Make informed decision
```

### Pattern 3: Find Duplication

```
1. search_implementors in same hierarchy
2. Compare implementations
3. Identify duplicates
4. Refactor to superclass
```

### Pattern 4: Narrow Search

```
1. Find implementors
2. Find references
3. Filter by receiver type
4. Validate with context
```

## Key Takeaways

1. **Learn from existing code** - Implementations reveal idioms
2. **Assess before changing** - Count implementors and references
3. **Scope your search** - Focus on relevant hierarchies
4. **Sample wisely** - Choose representative, well-implemented classes
5. **Consider impact** - High impact = find alternative approach

## Next Steps

For detailed analysis techniques, see [Implementation Analysis Reference](../references/implementation-analysis.md).
