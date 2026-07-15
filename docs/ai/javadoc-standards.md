# Javadoc Standards

Javadoc is the authoritative contract record for public API. Write it so that a human reading only the Javadoc, without the implementation, understands exactly what the method promises, what it costs, and what it might do unexpectedly.

## When to Write Javadoc

- Every class, interface, enum, and annotation in `skyve-core`, regardless of visibility; these form the stable framework API.
- Every method or constructor in `skyve-core`, regardless of visibility and any module that other modules or downstream applications call directly.
- Package-level `package-info.java` when the package has a non-obvious purpose or threading model.
- Skip generated sources (`src/generated/java`, `src/generatedTest/java`); they are throwaway artefacts.

## Lead Sentence

Open with a short third-person verb phrase that states what the element does, not what it is.

```java
/** Returns the resolved customer configuration, loading it on first access. */
/** Persists all pending changes for the current thread's persistence context. */
/** Builds a fully-populated fixture graph suitable for domain round-trip tests. */
```

Never restate the element name: `/** Gets the name. */` adds nothing.

## Contracts and Invariants

Document preconditions, postconditions, and invariants: things that must be true before the call, after it, and throughout the object's lifetime.

```java
/**
 * Resolves and returns the binding value at {@code path} relative to {@code bean}.
 *
 * <p>Precondition: {@code path} must be a valid dot-separated Skyve binding expression;
 * behaviour is undefined for malformed paths.
 *
 * <p>Invariant: the returned value reflects the bean's state at the moment of the call;
 * subsequent mutations to {@code bean} are not reflected in the returned object.
 *
 * @param bean  the root domain object; must not be {@code null}
 * @param path  dot-separated binding expression; must not be {@code null} or empty
 * @return the resolved value, or {@code null} if any intermediate binding is {@code null}
 */
```

## Side Effects

Name every observable effect beyond the return value. Use a `Side effects:` paragraph for contractual effects and `@implNote` when the effect is an implementation detail a caller need not rely on.

```java
/**
 * Saves the document instance and flushes the current Hibernate session.
 *
 * <p>Side effects: writes to the database; invalidates the second-level cache entry
 * for this bean's entity type; publishes a {@code PostSaveEvent} on the Skyve event bus.
 *
 * @throws DomainException if a validation rule is violated before the flush
 */
```

Always be explicit about thread-local state mutations, cache writes, file system writes, and external service calls.

## Complexity

Document Big-O time and space when it is non-obvious or when a caller might accidentally embed the call in a loop.

```java
/**
 * Returns all documents accessible to {@code role} within {@code module}.
 *
 * <p>Complexity: O(d) time where d is the number of documents in the module;
 * results are not cached - avoid calling this inside a per-request loop.
 */
```

Use `O(1)`, `O(n)`, `O(n log n)`, `O(n^2)` notation and qualify what `n` represents.

## Data Structure Choice

When a specific collection type matters to the caller (ordering, deduplication, identity semantics, modification rights), document it.

```java
/**
 * Returns the attributes declared on this document in declaration order.
 *
 * <p>Uses a {@link java.util.LinkedList} internally to allow O(1) insertion at either
 * end during metadata merging; callers that need random access should copy to an
 * {@link java.util.ArrayList}.
 *
 * @return a mutable, ordered list; never {@code null}
 */
```

If the caller must not modify the returned collection, say so explicitly.

## Threading

State the threading model whenever a class or method has non-trivial concurrency behaviour. Use one of these standard phrases in class-level Javadoc:

- *Thread-confined:* instances must not be shared across threads (for example `AbstractPersistence`).
- *Thread-safe:* all public methods may be called concurrently without external synchronisation.
- *Guarded by `<lock>`:* access to the named state is guarded by the named monitor.
- *Not thread-safe:* callers are responsible for external synchronisation.

For methods with specific threading constraints:

```java
/**
 * Executes the batch job step.
 *
 * <p>Threading: must be called from the job-execution thread. This method reads and
 * writes the thread-local persistence context; do not call from a fork/join worker
 * or a parallel-stream callback.
 */
```

Document `volatile` fields, lock ordering, and `ThreadLocal` ownership in `@implNote` when the detail is implementation-specific rather than part of the public contract.

## Tags Reference

| Tag | Use |
|-----|-----|
| `@param name` | Every non-obvious parameter; include null-safety and valid range |
| `@return` | Every non-void method; state null-safety and ownership |
| `@throws ExceptionType` | Every checked exception; important unchecked ones (for example `IllegalArgumentException`, `ObjectNotFoundException`) |
| `@implNote` | Implementation detail a maintainer should know but a caller need not rely on |
| `@implSpec` | Contract that subclasses or overriders must honour |
| `@since x.y` | New API added to a stable module; use the Skyve version string |
| `@see` | Related types or methods worth cross-referencing |
| `{@code ...}` | Inline code, type names, literals |
| `{@link ...}` | Cross-references to other types or members |

Omit `@param` and `@return` only when the name and lead sentence make their meaning completely unambiguous.

## What Not to Do

- Do not restate the method name or signature in prose.
- Do not add placeholder Javadoc (`@param name the name`).
- Do not document generated sources.
- Do not use HTML block elements (`<table>`, `<ul>`) unless a plain paragraph cannot express the structure.
- Do not describe the *how* (implementation steps) unless the algorithm choice itself is part of the contract.
