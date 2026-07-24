# Reusable Deployed Integration Testing

Skyve 10 adds a feature-neutral harness for integration tests that must exercise a real deployed
application. It builds a classified test-overlay WAR, starts an isolated checksum-approved
WildFly runtime, runs one standard Failsafe `*IT` suite, and retains server, browser, and test
diagnostics.

The overlay is produced only by the `deployed-it` Maven profile. It is not included in the normal
Skyve WAR or its dependency surface.

## Shared Test Lifecycle

Deployed suites can reuse the harness for:

- isolated WildFly and file-backed H2 startup;
- browser startup, login, logout, and authenticated cookie transfer;
- correlation-scoped, authenticated probe traffic;
- JSF view-state, normal postback, and PrimeFaces Ajax postback support;
- redacted diagnostics, screenshots, Failsafe reports, and a result summary; and
- local execution and the reusable GitHub Actions workflow.

Feature-specific event names, fixtures, hooks, and assertions stay in each suite's test-overlay
adapter. New suites select their `*IT` class without copying the Maven profile, runtime scripts,
transport, or CI lifecycle.

## SAIL Reuse

SAIL is an intended consumer of this infrastructure. A SAIL integration suite can plug its JUnit
or Failsafe wrapper and approved repository or classpath SAIL definitions into the shared deployed
application and browser lifecycle.

The common infrastructure is ready for that reuse, but an actual SAIL suite is not included yet.
SAIL-specific test resources, an overlay adapter where server-side observation is required, and
the SAIL assertions still need to be implemented. Migrating existing SAIL suites is a separate
piece of work and does not require another deployment profile or runner.

## Running a Suite

The generic contract can be checked without starting WildFly:

```bash
SKYVE_DEPLOYED_IT_TEST=example.ReusableHarnessIT tools/deployed-it/run.sh --validate-only
```

Full runs use the same entry point with an approved runtime, isolated configuration, loopback base
URL, and protected credentials when required. See the
[deployed integration test guide](../tools/deployed-it/README.md) for the complete preparation and
execution contract.
