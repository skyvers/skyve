# SAIL Device Profiles

## Goal

A SAIL workflow can treat a device as a persistent browser profile. A leg on
"phone 1" stops, a desktop leg runs, and "phone 1" starts again with its own
IndexedDB, CacheStorage, service worker registration and persistent cookies.
Two profiles are two devices.

The harness has the mechanics already (`SailWorkflow` stops and starts browsers
per device, and `CrossBrowserSelenium` builds the Chrome arguments). What's
missing is a way to name the profile and pass extra browser arguments, and to
let a step restart the browser on another configuration without the workflow
losing track of it.

## Context (verified)

- Chrome 158 headless on the same `--user-data-dir` keeps IndexedDB,
  CacheStorage, the service worker registration and persistent cookies across
  `quit()` and a new `ChromeDriver`. Session cookies are lost, as on a real
  restart.
- A step that runs `selenide.stopBrowser(); selenide.startBrowser(configuration)`
  on the leg's Selenide works: the executor carries on with the new driver.
- `SailWorkflow` keeps a `Browser(selenide, configuration)` record and compares
  the next leg's configuration with the record's, so a step that restarted the
  browser on another configuration leaves a stale record behind.

## Approved new constructs

1. **`BrowserConfiguration.profile(@Nullable Path userDataDir)`** and
   `getProfile()`. The directory is made absolute when it's set. `null` (the
   default) means a fresh temporary profile, as today.
2. **`BrowserConfiguration.arguments(@Nonnull String... arguments)`**, which
   appends extra browser arguments (for example
   `--use-fake-device-for-media-stream`), and `getArguments()`, an unmodifiable
   list. Fit both into master's `fixed()`, `settings(...)` and `resolve()`
   semantics. If those copy or freeze a configuration, the new fields are
   copied or frozen too.
3. **`CrossBrowserSelenium`:**
   - Chrome and Edge get `--user-data-dir=<absolute path>` when a profile is
     set, then the extra arguments, after the built-in ones;
   - Firefox and Safari with a profile or extra arguments throw an
     `IllegalArgumentException` before any browser starts, saying they're
     supported on Chrome and Edge;
   - the argument list is built by a package-private static method, so it can
     be tested without a browser.
4. **`SailWorkflow`:**
   - `sameDevice` also compares the profile and the extra arguments;
   - after each leg, the workflow reads the live configuration from the leg's
     Selenide (`getConfiguration()`) into its `Browser` record. A step that
     restarted the browser on another configuration is then what the next leg
     compares against, reuses or stops;
   - the Javadoc says that a step may restart the leg's browser, and that a
     step which stops it without starting it again isn't supported.
5. **A run shared across calls, only if master lacks it.** Check whether
   master's `SailWorkflow`/`SailSuite` already share one `RunState` (the
   `{run}` token and the variables) across the files of a suite or across
   repeated `runResources`/`runFiles` calls. If they don't, add an opt-in
   `SailWorkflow.shareRunAcrossCalls()` that keeps one `RunState` for the
   runner's later calls. Record what you found either way.

## Out of scope

- A `device` or profile attribute on `<automation>` (deferred, as in
  `plan_sail_script_steps.md`).
- Touch emulation, network emulation, or any other DevTools work.
- Deleting profile directories. The caller owns them.
- SailSettings keys for the profile. Add one only if master's configuration
  flow can't work without it, and ask Mike first.

## Tests

- Unit tests, no browser:
  - the Chrome and Edge argument lists, with and without a profile and extra
    arguments;
  - the Firefox and Safari refusal;
  - `sameDevice` for a different profile, different arguments, and identical
    configurations;
  - the record following a restarted configuration (use a test seam on the
    package-private record logic rather than a browser);
  - the shared run, if added.
- One browser test (an `*IT`, or whatever master's harness tests use for a real
  headless Chrome). It serves a page from a JDK `HttpServer` on `localhost`,
  writes `localStorage` and IndexedDB on profile A, stops and starts the browser
  on A and reads them back. On profile B they're absent.
- Existing harness tests keep passing.

## Docs

- `docs/sail.md`: a short "Devices and browser profiles" section covering the
  new methods, what survives a restart (and that session cookies don't), and
  restarting the browser from a step.
- No Upgrading entry unless something breaks. Nothing should.

## AI validation results

- Master has no `Browser(selenide, configuration)` record or `sameDevice`.
  The leg's Selenide holds what its browser started with (`recordStart`) and
  `CrossBrowserSelenium.browserChange` is the comparison, so a step that
  restarts the browser leaves the right state without a separate record.
  Construct 4 became: `browserChange` compares the profile and extra
  arguments; `SailWorkflowTest` drives a restart inside a leg and checks the
  next legs reuse or replace it.
- Construct 5: master's `SailWorkflow.run` makes a `new RunState()` for every
  call and suite member, and `SailSuite` does not share one. Added
  `shareRunAcrossCalls()`.
- Gate 1: `mvn -o -B package` passed (one unrelated flake, `UniqueConstraintMultipleNavigableTest.testSaveVarArgCyclic`, passed alone).
- `DeviceProfileSailIT` passes in headless Chrome 158 (`-Dtest=`, surefire).
- Spotless excludes `skyve-war/src/test/java/util/**`, so it does not apply.

## Acceptance

- [x] Constructs 1–4 (and 5 if needed) are built and tested.
- [x] The browser test proves profile persistence and isolation.
- [x] Gates pass (see the prompt).
