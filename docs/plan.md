# Skyve Framework Planning and Delivery Guide

This repository is the Java implementation of the Skyve low-code framework, a multi-module Maven project that provides metadata-driven application infrastructure for persistence, security, UI, content, reporting, and more.

This file defines:
1. how an AI agent should plan a Skyve framework milestone or sprint
2. how an AI agent should execute a milestone
3. the required structure of per-milestone plan files
4. the current milestone roadmap (maintained at the bottom of this file)

Milestones can represent framework features, API changes, coverage improvements, refactoring efforts, bug fixes, metadata enhancements, or tooling improvements.

---

# Project principles

The framework should optimise for:
- backward compatibility over expedient internal shortcuts
- metadata-driven behaviour over procedural special cases
- maintainability over clever abstractions
- correct dependency direction (`skyve-core` defines, `skyve-ext` implements, `skyve-web` delivers, `skyve-war` assembles)
- explicit contracts documented via Javadoc over implicit assumptions
- incremental delivery, one coherent milestone at a time
- clear evidence of validation for every change

Important constraints:
- Skyve is a framework consumed by downstream applications — API, metadata format, generated code conventions, and runtime behaviour changes can break consumers without any compile failure in this repository
- metadata is the primary source of truth for document shape, views, queries, privileges, and routing
- generated files under `src/generated/java/**` and `src/generatedTest/java/**` are never edited directly
- metadata changes may require domain regeneration before compile or test feedback is meaningful
- customer overrides, role-based security, and multi-tenancy are first-class design constraints — never bypass them
- every milestone must include AI validation and human validation suggestions
- the root Maven reactor (`pom.xml`) defines the authoritative build graph: `skyve-core`, `skyve-ext`, `skyve-war`, `skyve-web`, `skyve-content`, `skyve-tools`, `skyve-maven-plugin`, `skyve-coverage`

---

# Source-of-truth guidance

Use these sources when planning or executing milestones:
- `README.md` for user-facing framework capabilities and getting started
- `agents.md` for project-wide delivery rules, coding conventions, Javadoc standards, and cross-cutting invariants
- `docs/learnings.md` for durable engineering guidance and project learnings
- `docs/architecture.md` for framework architecture decisions and design attributes
- `docs/test-patterns.md` before creating or modifying tests
- `docs/coverage-plan.md` before any coverage-improvement work (defines target, skip list, tier ordering)
- module-local `pom.xml` files for dependency and build structure
- current module metadata, views, bizlets, services, jobs, and tests for the actual implemented behaviour

---

# Working files expected in this repo

These files should exist and evolve over time:
- `docs/plan.md` — framework roadmap and orchestration rules (this file)
- `docs/architecture.md` — durable architecture decisions and design attributes
- `docs/learnings.md` — durable engineering learnings and patterns
- `docs/coverage-plan.md` — coverage improvement plan, baselines, and tier ordering
- `docs/test-patterns.md` — reusable test pattern cookbook
- `docs/plan_m{n}.md` — one self-contained plan per milestone, for example `docs/plan_m1.md`

If guidance is milestone-specific, keep it in `docs/plan_m{n}.md`.
If guidance is durable and architecture-oriented, put it in `docs/architecture.md`.
If guidance is durable and broadly engineering-oriented, put it in `docs/learnings.md`.

---

# HOW TO PLAN A MILESTONE

If the user asks you to plan a milestone, these are the steps to take.

1. Read all of `docs/plan.md` to understand the framework roadmap, milestone boundaries, and planning rules.
2. Read any prior related `docs/plan_m{n}.md` files that this milestone depends on.
3. Read `agents.md`, `docs/architecture.md`, and `docs/learnings.md`.
4. If the milestone involves coverage work, also read `docs/coverage-plan.md` and `docs/test-patterns.md`.
5. Inspect the current repository and determine the real current state before planning.
6. Ask important clarifying questions only where needed.
   - Prefer discovering facts from the repo over asking the user.
   - Ask the user only for product intent, tradeoffs, or preferences that cannot be derived.
   - Questions must materially affect the plan.
   - Where possible, provide options and a recommendation.
7. Classify what the milestone changes:
   - public API surface (contracts, interfaces, abstract classes in `skyve-core`)
   - metadata format or interpretation
   - generated code templates or domain generation behaviour
   - runtime services (`skyve-ext`: persistence, reporting, backup, jobs, mail, etc.)
   - web delivery layer (`skyve-web`: JSF/Faces, filters, servlets)
   - assembled reference application or admin metadata (`skyve-war`)
   - Maven plugin behaviour (`skyve-maven-plugin`)
   - test infrastructure or coverage
   - documentation
8. Research the current codebase structures relevant to the milestone.
   - Identify existing classes, metadata, services, extension points, and tests to reuse.
   - Identify module ownership — which module should own the change.
   - Verify dependency direction is respected (core → ext → web → war).
9. Assess backward compatibility impact.
   - Will downstream applications break? If so, what migration path exists?
   - Does this change metadata format, generated code shape, or runtime behaviour consumers rely on?
   - If the change is breaking, state the impact explicitly and recommend a migration strategy.
10. Identify prerequisite milestones or hidden coupling.
    - If the work cannot be delivered safely in isolation, state the dependency explicitly.
    - If it can be delivered independently, keep the plan narrow.
11. Produce a self-contained milestone plan.
    - The plan must be specific enough that a fresh AI coding agent can execute it without replaying the entire repo history.
    - Include affected modules, metadata, handwritten Java code, generated code impact, tests, and validation steps.
12. Do a better-engineering pass on the plan.
    - Identify the smallest safe implementation.
    - Record deferred cleanup or follow-up work separately from the milestone scope.
13. Present the completed `docs/plan_m{n}.md` for user signoff.
14. Ensure the plan is a complete handoff document for implementation.

Use the following format for all `docs/plan_m{n}.md` files:

    # Plan: M{n} - <title>

    ## Summary
    <brief summary of deliverables, affected modules, dependencies, and validation expectations>

    ## Scope
    <list every work item covered by the milestone, including descriptions and any locked assumptions>

    ## Affected modules
    <which Maven modules are touched and why>

    ## Dependencies
    <prerequisite milestones, related work, or explicit statement that there are no blocking dependencies>

    ## Backward compatibility
    <impact on downstream applications, migration path if breaking, or explicit statement that the change is backward-compatible>

    ## Current state observed
    <what exists in the repo today that is relevant>

    ## Implementation plan
    <detailed implementation plan covering metadata, Java code, generated code impact, module placement, tests, and documentation as applicable>

    ## Acceptance checklist
    <item-by-item checklist stating what must be true for each deliverable to be considered complete>

    ## Better engineering notes
    <cleanup items, deferred improvements, backlog additions, or risks>

    ## AI validation plan
    <specific compile, generation, test, coverage, and manual verification steps the executor should perform>

    ## AI validation results
    <filled in during execution>

    ## User validation suggestions
    <specific manual checks the user can perform>

---

# HOW TO EXECUTE A MILESTONE

If the user asks you to execute a milestone plan, these are the steps to take.

1. Read the relevant `docs/plan_m{n}.md` in full.
2. Re-read any referenced parts of `docs/plan.md`, `agents.md`, `docs/learnings.md`, and area-specific guidance.
3. If the milestone involves tests or coverage, also read `docs/test-patterns.md` and `docs/coverage-plan.md`.
4. Implement the plan.
   - Prefer the smallest clean solution that satisfies the scope.
   - Reuse existing Skyve patterns, metadata mechanisms, and service abstractions.
   - Respect module ownership: `skyve-core` for contracts, `skyve-ext` for runtime services, `skyve-web` for web delivery, `skyve-war` for assembly.
   - Keep business logic out of views — use extensions, services, or jobs.
5. Regenerate domain when metadata changes make that necessary.
   - Use `mvn -B -DskipTests compile` and `mvn -B skyve:generateDomain` in the right order.
   - See `docs/learnings.md` for Eclipse class file conflicts that require `clean compile` first.
6. Validate continuously while implementing.
   - Run the smallest meaningful compile or test step first.
   - After editing any Java file, call `get_errors` and resolve all warnings before moving on.
   - Follow `docs/test-patterns.md` for test placement and base class selection.
   - For coverage work, use the build commands in `docs/coverage-plan.md`.
   - Broaden validation before handoff: `mvn -B package` is the baseline CI-aligned build.
7. Update the `Acceptance checklist` and `AI validation results` sections in the milestone plan.
8. Review your own work for behavioural regressions, backward compatibility, and scope completeness.
9. Do a better-engineering pass.
   - Add durable learnings to `docs/learnings.md` when warranted.
   - Record deferred follow-up work in the milestone plan instead of silently broadening scope.
10. Ensure the implementation is ready for human review.
11. Present:
    - what changed (files and modules)
    - how it was validated
    - what the user should test
    - any known limitations, assumptions, or follow-up items
    - backward compatibility impact (if any)

---

# Milestone roadmap

The roadmap below tracks planned framework milestones. Each milestone groups related work items into implementation-sized plans. Add new milestones here as they are identified and approved.

## Milestone categories

Framework milestones typically fall into one of these categories:

- **Feature**: New framework capability, API, metadata format, or runtime behaviour
- **Coverage**: Test coverage improvement following the tier ordering in `docs/coverage-plan.md`
- **Refactor**: Internal restructuring that preserves external behaviour
- **Fix**: Bug fix or correctness improvement
- **Docs**: Documentation, Javadoc, or developer guide improvements
- **Tooling**: Maven plugin, code generation, scaffolding, or build improvements
- **Upgrade**: Dependency upgrades (Hibernate, Jakarta EE, etc.)

## Active milestones

<!-- Add milestones here as they are planned. Use the format:

## Milestone M{n}: <title>

Category: <Feature|Coverage|Refactor|Fix|Docs|Tooling|Upgrade>

Covers:
- <work item description>

Goal:
<what this milestone delivers>

Affected modules:
- <module list>

Validate:
- <key validation criteria>

-->