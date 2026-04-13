# Smalltalk Implementation Patterns

Useful implementation patterns.

## Singleton

Use when exactly one instance of a class should exist for the lifetime of the image (e.g., registries, connection pools, application-wide services).

- Use a **class instance variable** (not a class variable, which would be shared with subclasses).

```smalltalk
Class {
	#name : 'MySingleton',
	#superclass : 'Object',
	#instVars : [],
	#classInstVars : [
		'current'
	],
	...
}
```

- Name the class instance variable `current`, `default`, or `soleInstance`.

- Define an `initialize` class method to reset the variable (to `nil`).

```smalltalk
{ #category : 'class initialization' }
MySingleton class >> initialize [
	current := nil.
]
```

- Define a lazy initialization accessor to return the singleton instance.

```smalltalk
{ #category : 'accessing' }
MySingleton class >> current [
	^ current ifNil: [ current := self new ]
]
```

## Settings

Use when a component needs configurable parameters with safe access, default values, and the ability to copy/customize a baseline configuration (e.g., server settings, client options).

- Do **not** use a bare `Dictionary` for settings — it accepts any key/value and has no encapsulation.
- Define a dedicated Settings class with a dictionary-like interface. This enables safe access and lazily initialized default values.

```smalltalk
Class {
	#name : 'MySettings',
	#superclass : 'Object',
	#instVars : [
		'settingsDict'
	],
	#classInstVars : [
		'default'
	],
	...
}
```

- Expose dictionary-like primitives internally (`actions-dictionary` category).

```smalltalk
{ #category : 'actions-dictionary' }
MySettings >> at: key ifAbsentPut: aBlock [
	^ self settingsDict at: key ifAbsentPut: aBlock
]

{ #category : 'actions-dictionary' }
MySettings >> at: key put: value [
	^ self settingsDict at: key put: value
]

{ #category : 'actions-dictionary' }
MySettings >> keys [
	^ self settingsDict keys
]
```

- Define typed accessors for each setting. The getter uses lazy initialization via `ifAbsentPut:`.

```smalltalk
{ #category : 'accessing' }
MySettings >> port [
	^ self at: #port ifAbsentPut: [ self defaultPort ]
]

{ #category : 'accessing' }
MySettings >> port: aNumber [
	self at: #port put: aNumber
]
```

- Define default values in a separate `defaults` category.

```smalltalk
{ #category : 'defaults' }
MySettings >> defaultPort [
	^ 8081
]
```

- Define `asDictionary` for interoperability (legacy interfaces, JSON serialization, etc.).

```smalltalk
{ #category : 'converting' }
MySettings >> asDictionary [
	^ self keys inject: Dictionary new into: [ :dict :key |
		dict at: key put: (self perform: key);
		yourself ]
]
```

- Define `defaultCopied` to obtain a customizable copy of the default instance. The original default is not affected.

```smalltalk
{ #category : 'instance creation' }
MySettings class >> defaultCopied [
	^ self new initFrom: self default
]

{ #category : 'initialization' }
MySettings >> initFrom: otherSettings [
	self initialize.
	otherSettings settingsDict keysAndValuesDo: [ :k :v |
		self settingsDict at: k put: v ]
]
```


## Options builder

Use when a method accepts a parameter object (e.g., `RsSearchOptions`) and you want to avoid forcing the caller to instantiate that class directly. A builder block keeps call sites concise and decouples them from the options class name.

Suppose we have a method:
`RsSearcher >> search: indexName query: query options: options`
where `options` is an `RsSearchOptions` instance.

Without the pattern, the caller must know and instantiate `RsSearchOptions` explicitly:

```smalltalk
| options |
options := RsSearchOptions new.
options offset: 10; limit: 20; returnFields: #('score' 'values').
searcher search: 'idx' query: 'st*' options: options
```

Add a companion method that accepts a builder block instead:

```smalltalk
searcher search: 'idx' query: 'st*' optionBy: [ :opts |
	opts offset: 10; limit: 20; returnFields: #('score' 'values') ]
```

`RsSearcher` delegates to the original method after building the options:

```smalltalk
{ #category : 'actions' }
RsSearcher >> search: indexName query: query optionBy: aBlock [
	| options |
	options := RsSearchOptions in: aBlock.
	^ self search: indexName query: query options: options
]
```

`RsSearchOptions` provides an `in:` class-side factory that evaluates the block:

```smalltalk
{ #category : 'instance creation' }
RsSearchOptions class >> in: aBuilderBlock [
	| instance |
	instance := self new.
	aBuilderBlock value: instance.
	^ instance
]
```

The caller no longer needs to reference `RsSearchOptions` directly. The interface is more fluent and type-safe — passing an unrelated options object becomes impossible.
