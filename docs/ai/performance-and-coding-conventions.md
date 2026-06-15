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
