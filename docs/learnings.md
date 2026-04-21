# Learnings

`docs/learnings.md` is for durable engineering wisdom that should survive refactors and still apply across Skyve modules. This file should stay framework- and language-relevant: if a lesson only applies to one document, one generator, or one package, it belongs closer to that code or in [docs/architecture.md](architecture.md).

Use the scope ladder when deciding where an insight should live:

- Repo-wide and durable: put it in `docs/learnings.md`.
- Architecture or policy for Skyve as a framework: put it in [docs/architecture.md](architecture.md).
- Module- or subsystem-specific guidance: keep it with module-local docs.
- Metadata-file rules: prefer comments or adjacent docs near the metadata source of truth.
- Symbol-local contract: put it in code docblocks near the class or method.
- Naming or API smell: prefer reshaping the API over writing more prose.

Quick test: if the insight remains true after moving packages around or adding new modules, it probably belongs here. If it depends on the current implementation of one subsystem, it probably does not.

## Engineering Discipline

- Every warning is a mismatch between intent and reality. Fix it immediately or stop and design a clean fix.
- Do not guess. Inspect the code, metadata, generated artefacts, and runtime path before proposing a change.
- Derive contracts from essential data flow. Remove unused inputs and document the invariants that remain.
- Recover intent before deleting code. In Skyve, seemingly dead code may still be referenced by metadata, reflection, generation, CDI/Spring wiring, or customer override paths.
- Generalise local cleanups into pattern audits. If one metadata loader, extension point, or generated artefact is wrong, nearby code often has the same flaw.
- Fix prerequisites cleanly. If goal X needs metadata generation, repository validation, or packaging cleanup first, do that work properly.
- Do not defer quality fixes that expose broken invariants, stale generation, or runtime drift.
- Treat documentation silence as "no". Do not invent framework behaviour unless you are also updating the source-of-truth docs and tests.
- Look for an existing mechanism before adding a new one. In Skyve, the right answer is often "use metadata, generation, or an existing service hook".
- Operational logs are data stores too. Never persist secrets (reset tokens, one-time codes, magic links, credentials); redact by default and require explicit opt-in for sensitive payload storage.

## Skyve-Specific Learnings

- Metadata is usually the source of truth. If generated Java, admin artefacts, or runtime behaviour disagree with metadata, fix the metadata or generator before patching downstream output.
- Generated sources are not ownership boundaries. Their presence explains current behaviour, but the real design usually lives in metadata, generator templates, or handwritten extension classes.
- The root Maven reactor is the authoritative build graph. Do not assume every repository directory participates in CI just because it exists on disk.
- `skyve-core` should stay focused on contracts, metadata, and abstractions. Push concrete runtime integrations outward into `skyve-ext` or delivery-specific modules unless there is a strong architectural reason not to.
- Backward compatibility is part of the design. Changes to metadata interpretation, generator output, or framework APIs can break downstream applications without any compile failure in this repository.
- Mixed test generations are intentional for now. Preserve local conventions unless the task is specifically to modernize the test layer.
- Customer overrides, role-based security, and routing are framework features, not application special cases. Fixes that bypass them are usually wrong even when they appear smaller.
- When framework code persists records outside normal request/user flows, required identity fields must still be valid. Use a deterministic synthetic framework user when a real user context is missing or incomplete.
- For cross-cutting behavior (normalisation, test overrides, dispatch logging), route through one shared service entry point so all call sites inherit the same guarantees.

## Architecture And Refactorability

- Prefer designs where metadata interpretation, decision logic, and effects are separable. This makes generation and testing cheaper.
- Keep code generation deterministic. If a generated output changes, the input or template should explain why.
- Design modules around ownership boundaries. `skyve-core` defines, `skyve-ext` implements, `skyve-web` delivers, `skyve-war` assembles.
- Refactor toward clearer boundaries as soon as you see leakage. Metadata concerns leaking into web code, or servlet concerns leaking into core, get more expensive the longer they stay.
