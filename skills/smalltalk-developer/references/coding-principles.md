# Smalltalk Coding Principles

Fundamental principles to keep in mind when implementing Smalltalk (Pharo/Squeak) code. Unlike the [Style Guide](style-guide.md), which covers concrete idioms, these principles guide the design decisions behind every class and method you write.

## 1. Care About Names

### Choose Names That Reveal Intent

- Prefer clear, unambiguous names, even if they are long.
- Pay special attention to **method names**:
  - The selector alone should convey the intent, without reading the implementation.
  - Parameter names should reveal the type or role of each argument.

**Why it matters more in Smalltalk**: Smalltalk is dynamically typed, so there are no declared types to infer what a parameter expects. Selectors and parameter names are the primary source of that information.

**Example:**

```smalltalk
❌ Unclear:
fail: aReason with: aReasonBlock
```

- From the selector `fail:with:`, it is impossible to guess what to pass to each argument.
- `aReason` looks like an instance of a `Reason` class, but the implementation actually expects a string.
- `aReasonBlock` suggests a block that does something with the reason. In reality, it is the main block executed on failure, and the reason string is only passed as supplementary information.

```smalltalk
✅ Clear:
onFailedDo: failedBlock reason: reasonString
```

- The selector `onFailedDo:reason:` alone is suggestive: `Do:` implies a block closure, and `reason:` implies something describing the reason.
- The parameter names `failedBlock` and `reasonString` confirm the types and roles.

### Avoid Ambiguous Names

A name that can be read in two ways forces readers to check the implementation. Avoid it.

**Example 1: action or setter?**

```smalltalk
❌ Ambiguous:
{ #category : 'operations' }
MyClass >> finishStartConfig: aConfig [ ... ]
```

Is this an action that finishes the start config, or just a setter for a `finishStartConfig` attribute? Rename according to what it actually does:

- **If it is an action** (starts finishing `aConfig`):
  ```smalltalk
  ✅ { #category : 'actions' }
  MyClass >> startFinishingOf: aConfig [ ... ]
  ```
- **If it is a plain setter**: keep it as a setter and put it in the `'accessing'` category.

**Example 2: hidden heavy work behind a getter-like name**

```smalltalk
❌ Looks like a simple getter:
configFileOrDefault
```

Because it looks like a cheap accessor, callers casually wrote `self configFileOrDefault` everywhere. In reality, it read the config file from disk and created a default one if the file did not exist — a heavy operation with side effects.

```smalltalk
✅ Reveals reading and creating:
readConfigFileIfAbsentCreateDefault
```

**Rule of thumb**: If a method does I/O, creates resources, or has side effects, make that visible in the name.

### Avoid Redundant Names

Do not repeat context that is already obvious.

```smalltalk
❌ Redundant (in the Topic class):
Topic >> topicId
Topic >> topicName

✅ Concise:
Topic >> id
Topic >> name
```

It is already clear that these are attributes of a `Topic`, so `id` and `name` are enough.

When the context is **not** obvious, qualified names are appropriate. For example, in `TopicGoalManager`, which deals with both topics and goals, `topicId` and `goalId` are the right names.

## 2. Clarify Responsibilities

### Clarify Class Responsibilities

Keep each class's responsibility clear and focused.

**Warning sign**: A class description like this:

> Class A does xxx, and also yyy. It also plays the role of zzz.

Ask whether yyy and zzz truly belong to class A. They may be better handled by other classes.

**Test**: If you cannot explain what the class does in a single sentence, its responsibility is becoming blurred. This also applies when writing CRC-style class comments — a hard-to-write "Responsibility" section is a sign the class needs to be split.

### Clarify Method Responsibilities

Do not combine multiple operations of different natures in one method.

**Example:**

```smalltalk
❌ Mixed: computes a hash (idempotent) AND writes it to a file (side effect)
MyClass >> computeAndSaveHash [
	| hash |
	hash := self contents sha256.
	self hashFile writeStreamDo: [ :stream | stream nextPutAll: hash ].
	^ hash
]
```

```smalltalk
✅ Separated:
MyClass >> contentsHash [
	"Idempotent: safe to call any time"
	^ self contents sha256
]

MyClass >> saveHash [
	"Side effect: writes to the hash file"
	self hashFile writeStreamDo: [ :stream | stream nextPutAll: self contentsHash ]
]
```

**Why**: Once separated, callers can invoke the idempotent method with confidence, without worrying about unintended side effects. Each method also becomes easier to test and reuse.

## Summary

| Principle | Check |
|-----------|-------|
| Intent-revealing names | Can the selector and parameter names alone tell what to pass and what happens? |
| No ambiguity | Could the name be read as both an action and an accessor? Does it hide heavy work or side effects? |
| No redundancy | Does the name repeat context already given by the class? |
| Class responsibility | Can you describe the class in one sentence? |
| Method responsibility | Does the method mix pure computation with side effects? |
