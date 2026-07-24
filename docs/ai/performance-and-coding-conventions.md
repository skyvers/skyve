# Performance and Coding Conventions

Skyve must be performant. Apply these rules to all production Java code; they are not optional style preferences.

## Threading

- All shared mutable state must be explicitly thread-safe. Choose `synchronized`, `volatile`, `java.util.concurrent` types, or immutability; pick the lightest mechanism that is correct and document the choice.
- Never silently share thread-local state across method boundaries. `ThreadLocal` fields must document their lifecycle: who initialises, who cleans up, and under what exception path.
- Prefer stateless collaborators. A class that holds no mutable instance state is thread-safe by construction.

## Complexity and Allocation

- Be Big-O aware. Before writing any loop over a collection, know what `n` is and whether the body has hidden O(n) or O(n log n) operations (for example `List.contains`, `String.format`, getter chains that recompute). Nested loops over metadata collections are a common source of O(n^2) behaviour in Skyve.
- Do not allocate objects inside tight loops when a local reference declared outside the loop will do.
- Prefer pre-sized collections when the final size is known or estimable: `new ArrayList<>(size)`, `new HashMap<>(size * 4 / 3 + 1)`.
- Avoid autoboxing in hot paths. Use primitive arrays or `int`/`long` locals rather than `Integer`/`Long` wrappers when the value is only needed locally.

## Defensive Collection Copies

- Use defensive copies sparingly. Do not copy a `List` or other collection automatically in constructors, setters, getters, or return values; prefer retaining or returning the existing mutable collection when the contract permits it.
- Treat allocation and traversal as real costs. A defensive copy is O(n), creates garbage, and may break callers or extension points that intentionally rely on shared mutability.
- Make a defensive copy only for a concrete ownership reason, such as preventing an untrusted caller from violating an invariant, taking a stable snapshot, or transferring data across a thread or lifecycle boundary where later mutation would be unsafe.
- Copy once at the ownership boundary and choose shallow or deep copying deliberately. Document the collection's ownership, mutability, and any non-obvious copy cost in the API contract.
- Do not introduce `List.copyOf()`, unmodifiable wrappers, or similar constructs solely as a precaution or to satisfy a generic static-analysis recommendation. They change mutability and may also change null-handling semantics, so use them only when that behaviour is required.

## Nullability and Optional

- Prefer `jakarta.annotation.Nonnull` and `jakarta.annotation.Nullable` to `Optional<T>` for ordinary parameters, fields, getters, and return values. Make the nullability contract explicit without adding wrapper allocation or chaining overhead.
- On every method declaration, place the return nullability annotation after the modifiers and before the return type; for a generic method, the annotation must precede the type-parameter list. Annotate every reference-typed parameter individually with either `@Nonnull` or `@Nullable` immediately before its type. Do not annotate `void` returns or primitive types. For example: `public static @Nonnull <T> T resolve(@Nonnull String key, @Nullable T fallback, int attempts)`.
- Use nullability annotations instead of runtime non-null checks for internal contracts. Do not add `assert value != null`, `Objects.requireNonNull(value)`, or equivalent guards merely to restate a `@Nonnull` contract; they consume processing cycles for a condition that the compiler or static analyser can check.
- Allow an ordinary `NullPointerException` to propagate when annotated internal code violates a non-null contract. At that boundary, an NPE exposes the programming error directly and usually does not benefit from an earlier duplicate check.
- Do not use `Optional<T>` merely to avoid returning `null`, and do not use it as a parameter or field type unless absence is itself a meaningful value in the model. Never return `null` from a method whose declared return type is `Optional<T>`.
- Use `Optional<T>` only when it materially improves the API or represents additional state, such as a composable lookup result or distinguishing a cached miss from a key that has not been queried. Preserve existing `Optional` contracts unless deliberately changing the API.
- For multi-valued results, return an empty collection rather than `null` or `Optional<Collection<T>>`, unless an existing contract assigns a distinct meaning to absence.
- Nullability annotations document and support static analysis; they do not perform runtime validation. Validate untrusted input and required public API arguments only where the boundary requires a specific failure mode, exception type, or message. Do not add runtime checks by default; usually a propagating `NullPointerException` is the correct outcome.

## Utility Reuse

- Do not reinvent utility behaviour. Before adding a helper method, search Skyve for an established utility with the required semantics and use it consistently.
- Prefer utilities in this order: Skyve's public utility APIs; suitable utilities from libraries already on the owning module's dependency path; then a small class-local helper only when the behaviour is genuinely specific to that class.
- Do not generate near-identical private helpers in multiple classes, hand-code a weaker version of an existing utility, or add a dependency merely to obtain a trivial helper.
- Verify semantic fit rather than choosing a utility by name alone. Null handling, mutation, escaping, locale, ordering, and exception behaviour must match the calling contract.
- When a generally useful capability is missing, identify it as a Skyve utility gap. In this repository, add or propose a single well-named utility in the module that owns the concern, with focused tests and a documented contract. In downstream applications, explicitly suggest contributing the capability to Skyve instead of allowing application-local copies to proliferate.
- Keep a private helper when the logic is domain-specific, used only locally, or would make a shared utility less cohesive. Shared utility APIs must represent a stable reusable concept, not merely reduce a few lines at one call site.

## String Input Normalisation

- At input boundaries, pass strings through `Util.processStringValue()` before using or storing them. This includes request parameters, metadata and configuration values, imported data, external-service responses, and other text entering the application or framework.
- Use the processed value throughout downstream logic so `null` is the single representation of absent text. Do not repeat separate checks for `null`, empty strings, and whitespace-only strings in business logic or tests.
- `Util.processStringValue()` trims leading and trailing whitespace and returns `null` when the trimmed result is empty. Within Skyve implementation code that cannot depend on the public `Util` facade, use `UtilImpl.processStringValue()` with the same semantics.
- Do not normalise opaque or fixed-format values when whitespace is meaningful, such as passwords, signed values, raw payloads, or fixed-width records.

## OWASP Sanitisation and Escaping

- Treat sanitisation and output escaping as separate controls. Sanitise untrusted input to restrict its permitted content, then escape it for the exact context in which it is rendered. Sanitisation does not replace contextual output escaping.
- Normalise nullable text with `Util.processStringValue()` before sanitising it. Use `OWASP.sanitise(Sanitisation.text, value)` for untrusted plain text. Where rich HTML is an intentional feature, choose the most restrictive policy that supports it: `basic`, `simple`, then `relaxed`. Use `Sanitisation.none` or a `null` policy only for explicitly trusted content.
- Escape at the final output boundary with the context-specific helper: `OWASP.escapeHtml()` for HTML text, `OWASP.escapeJsString()` for JavaScript string content, and `OWASP.escapeJsonString()` for JSON string content. Prefer the renderer, component, or JSON serializer's built-in contextual escaping when one already owns that boundary.
- Do not reuse a value escaped for one context in another context, and do not pre-escape stored domain values. Keep stored values sanitised but otherwise raw so each output boundary can apply the correct encoding exactly once.
- Use `OWASP.sanitiseFileName()` before using an untrusted value as a file name. This does not authorise a path supplied by the caller; application code must still control the directory and path construction.
- Pass untrusted values through `OWASP.sanitiseLog()` before including them in log messages to prevent control-character and log-forging injection. This method does not redact secrets or sensitive data; do not log such values in the first place.
- Preserve Skyve's escaped-by-default view rendering. Set metadata escape flags to `false` only for trusted markup, following [View Boilerplate Escaping](../view-boilerplate-escaping.md).

## References Over Repeated Getter Calls

Assign the result of a getter (or any non-trivial accessor) to a local variable when it is used more than once in the same method. Repeated getter calls can hide computation, trigger lazy loading, or break consistency if the underlying state mutates mid-method.

```java
// Wrong - getAttributes() may be O(n) or trigger lazy init on every call
for (int i = 0; i < document.getAttributes().size(); i++) {
    process(document.getAttributes().get(i));
}

// Right - one call, one reference
List<Attribute> attributes = document.getAttributes();
int size = attributes.size();
for (int i = 0; i < size; i++) {
    process(attributes.get(i));
}
```

This also applies to chained calls: assign intermediate results to named locals so each accessor appears exactly once and the intent is readable.

## Loops Over Streams

Prefer `for` loops over the Stream API in production framework code.

- `for` loops are easier to read, step through in a debugger, and reason about under exceptions.
- Streams introduce lambda allocation, capture overhead, and can obscure `throws` declarations.
- Use streams only when the pipeline genuinely simplifies the logic (for example `collect`, `flatMap` over nested structures) and the call site is not on a hot path.
- Never use a parallel stream unless you have measured that it helps and you have verified the pipeline is safe under concurrent execution.

```java
// Prefer
for (Module module : modules) {
    if (module.isActive()) {
        register(module);
    }
}

// Avoid in framework hot paths
modules.stream().filter(Module::isActive).forEach(this::register);
```

## Injection Agnosticism

Skyve framework classes must not depend on a specific injection container (CDI, Spring, Guice, and so on).

- Write classes so that all collaborators can be supplied via constructor or setter without a container. This keeps the class testable in plain JUnit without a full application context.
- Where existing code uses CDI or Spring annotations, leave them in place; do not gratuitously remove working wiring, but do not add new hard dependencies on a particular container API.
- If a collaborator is optional or environment-specific, guard access behind an interface or factory rather than an `@Inject` field.
- Introduce an explicit test seam (package-private setter or constructor overload) when a class must be instantiated directly in tests.
