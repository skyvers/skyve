# Plan: SAIL browser control and a standard call library

## Summary

Give SAIL a browser-agnostic way to control the browser beyond WebDriver, and a library of standard `<call>` steps built on it, taken from what basix-cloud built for its inspection UI suite (2026-11-18/19).

- **Browser control.** Interfaces grouped by what a test needs: request interception, a request log, network conditions, scripts injected before page load, dialogs, cookies, storage quota, page lifecycle, tabs, keys, window. One entry point picks an implementation per browser. A capability model says what the current browser supports.
- **Implementations.** Chromium uses the Chrome DevTools Protocol (CDP), plus plain WebDriver for what every browser has. Any other browser uses plain WebDriver only. WebDriver BiDi (Firefox, and Chrome) is Phase 2, a separate plan. Nothing in the interfaces may depend on CDP.
- **Unsupported means skipped.** A call that needs a capability the browser lacks stops the test as **skipped** (aborted), with a message naming the call, the capability and the browser. It is never failed.
- **Standard calls.** About 25 `SailCall`s on the interfaces only: offline and online, throttling, failing, holding and answering requests, request checks, dialogs, cookies, quota, a blocked IndexedDB, a shifted clock, keys, window size, tabs, and device start or restart. Each sits on a shared call base with typed, validated parameters.
- **Test files.** Generated upload files (photos of a given size or dimensions, an oversize one, a playable video of a target size made in the browser) in a new `SailTestFiles`. That is separate from `SailMedia`, which is run evidence.
- **Device profile housekeeping.** Device profiles are deleted when their run ends, and old ones are swept before a device starts. Chrome is stopped from filling every profile with components it downloads by itself. In basix-cloud, about 2,500 profiles from about 225 runs over one day took 89 GB, because nothing deleted them; about 70% of each was Chrome's own downloads.
- **No new SAIL op-codes.** A capability becomes an op-code only once it has implementations for at least Chromium and Firefox. That rule goes into `docs/sail.md`.

**Modules:** `skyve-war` test code only (`src/test/java/util/**` and its fixtures), plus docs. No `skyve-core` or `skyve-ext` change.

**Validation:**
- unit tests with fake browser controls;
- a self-contained CDP IT (a JDK `HttpServer` page with a service worker, no Skyve server needed);
- one SAIL IT running the calls through `<call>` in a script;
- Firefox runs proving unsupported calls skip, if Firefox is installed;
- a profile housekeeping test, and a Chrome profile measured before and after the launch flags;
- the usual gates.

## Scope

1. **Capability model and interfaces** (`util.sail.browser`):
   - `Capability` enum;
   - one interface per group (below);
   - `SailBrowserControl`, the entry point, with `of(WebDriver)` and `supports(Capability)`;
   - `SailCapabilityUnavailableException`.
2. **Implementations:**
   - `ChromiumBrowserControl`: CDP for interception, the request log, network, init scripts, dialogs, quota, lifecycle and touch; WebDriver for cookies, tabs, keys and window.
   - `WebDriverBrowserControl`: any browser, cookies, tabs, keys and window only.
3. **Call base** (`util.sail.call`):
   - `AbstractSailCall`, implementing `org.skyve.metadata.sail.execution.SailCall`, with a context giving the driver, the Selenide wrapper, the browser control and the base URL;
   - `SailCallParameters`, with declared names, unknown names rejected, and typed getters for strings, booleans, integers, required values, lists and choices.
4. **Standard calls** (`util.sail.call`), one class per step, on the interfaces only:
   - **Network:** `Offline`, `Online`, `Throttle`.
   - **Request rules:** `FailRequests`, `HoldRequests`, `AwaitHeldRequests`, `ReleaseRequests`, `RespondToRequests`, `LoseResponses`, `RewriteRequestHeaders`, `ClearRequestRules`.
   - **Request checks:** `CheckRequests` (merging basix's `CheckRequests`, `CountRequests` and `RequestLog`; see decision 8) and `RequestLog`.
   - **Browser:** `CheckDialog`, `DeleteCookies`, `LowerQuota`, `BlockIndexedDb`, `ShiftClock`, `PressKey`, `ResizeWindow`, `FreezeTab`.
   - **Tabs:** `SwitchTab`, `CheckOpenedTab`.
   - **Devices:** `StartDevice`, `RestartDevice` (decision 12).
5. **`SailTestFiles`** (`util.sail`): photo files of a given byte size or pixel dimensions, an oversize photo, and a playable WebM video of a target size, recorded in the browser (a canvas of noise, `captureStream`, `MediaRecorder`), so no ffmpeg is needed. Written under `target/sail-test-files/`.
6. **Device profile housekeeping** (decisions 13 and 14):
   - `SailProfiles` (`util.sail`): owns the profile root, gives a run's profile folders, deletes a run's folder when the run ends, and sweeps old runs;
   - `SailWorkflow` ends a run's profiles: each call's own run, or a shared run when the workflow is closed;
   - `CrossBrowserSelenium` starts Chrome and Edge without component and model downloads.
7. **Docs:**
   - `docs/sail.md`: a section "Browser control and standard calls", with the capability table, each call's parameters, how skipping works, and the op-code promotion rule; and a "Device profiles" subsection: where they go, when they are deleted, the settings that keep them;
   - `docs/skyve-10-changes.md`: a release-note line;
   - `docs/architecture-design.md`, if it lists test-harness components.

**Out of scope:**
- the WebDriver BiDi implementation (Phase 2);
- promoting calls to op-codes;
- `SailSuite` runner features (per-test isolation, filters): another plan;
- a "lanes" section in `docs/sail.md`;
- migrating basix-cloud onto these calls, which is downstream and done in basix after assembly;
- changing `SailMedia`'s screencast, which is Chromium-only today (recorded as a follow-up).

## Affected modules

- `skyve-war`: new test-scope code under `src/test/java/util/sail/browser/**`, `src/test/java/util/sail/call/**`, `util/sail/SailTestFiles.java` and `util/sail/SailProfiles.java`; changes to `util/sail/CrossBrowserSelenium.java`, `SailWorkflow.java`, `SailSettings.java` and `util/AbstractInterpretedSailWorkflow.java` (decisions 13 and 14); fixtures under `src/test/resources/util/sail/**`; docs. Assembly copies `util/**` into downstream apps, so everything must stay within `util/sail` (`HarnessSelfContainedTest` guards this).
- No production module changes.

## Dependencies

- Master at or after `afe0f69fb7` (per-device remember-me, password prompts off, opt-in browser per suite member).
- Branch `sign-out-everywhere` may land on master first. If master has moved when you start, merge master into this branch.
- No other blocking dependency.
- Selenium comes from Selenide 7.12.1 (Selenium about 4.38). It has `SeleniumCdpConnection` for raw CDP, and the WebDriver BiDi modules needed for Phase 2.

## Backward compatibility

- **Additive, apart from profile housekeeping.** New packages and classes; no SAIL XML changes. Two behaviour changes:
  - Chrome and Edge start with component and model downloads off (decision 14). Every SAIL browser gets this, with or without a profile. Tests don't depend on those downloads; W0 checks that the inspection-style behaviours survive.
  - A passed run's profile folders are deleted when the run ends, and runs older than the keep time are swept (decision 13). Anyone who opened a profile after a run passed must now set `sail.profiles.keep=all`. Record this in "Upgrading" in `docs/sail.md`.
- **Downstream apps** get the new packages on their next assembly.
- **basix-cloud** has its own copies under other packages (`au.com.emssolutions.basix.test.sail`, `sail.inspection`), so nothing clashes. Its migration (deleting the copies, changing `<call className>` from `sail.inspection.X` to `util.sail.call.X`) is a basix follow-up.
- **Skipped versus failed:** `SailCapabilityUnavailableException` makes a test **skipped**. If `SailWorkflow` or `SailSuite` today turns any exception from a call into a failure, W0 decides the smallest change to let this one through as aborted (decision 4). Record that change in "Upgrading" in `docs/sail.md`.

## Current state observed

**In Skyve (`skyve-war/src/test/java/util/sail`):**
- `SailCall` (skyve-core `org.skyve.metadata.sail.execution.SailCall`): `@Nullable String call(Map<String,String> parameters, Executor executor)`.
- `InterpretedWebDriverExecutor` has `getSelenide()`.
- `BrowserConfiguration` (`profile(Path)`, `arguments(...)`), `Devices`, `CrossBrowserSelenium` (chrome, firefox, safari, edge), `SailWorkflow`, `SailSuite`, `SailSettings` and `SailMedia` (step snapshots, plus a video from Chrome's DevTools screencast: already Chromium-only).
- No DevTools or network control exists apart from that screencast.
- Nothing deletes a profile folder. `BrowserConfiguration.profile(Path)` only names it. ChromeDriver deletes the temporary profile it makes when no profile is set, but not one passed with `--user-data-dir`.
- `CrossBrowserSelenium`'s Chrome arguments turn off occluded-window, renderer and timer backgrounding, but nothing stops Chrome's component updater or model downloads.
- The run token (`RunState`) is the run's start time in milliseconds, in base 36 (e.g. `mwmkb77r`).
- `SailWorkflow` has no close. A suite stops its kept browser with `closeBrowser()`. `AbstractInterpretedSailWorkflow`'s `@AfterAll` only releases the server lock.

**In basix-cloud (read-only source to port from, v6 at `8fa4e5675`, `/Users/mike/_/basix-cloud`):**
- `src/test/java/au/com/emssolutions/basix/test/sail/SailDevTools.java` and `FetchRule.java` (+ `FetchRuleTest`, `SailDevToolsIT`): the CDP engine.
  - Raw CDP via `SeleniumCdpConnection`, not the version-pinned `selenium-devtools-vNNN` modules, which lag Chrome. Chrome 158 against devtools v142 printed "Unable to find CDP implementation matching 158".
  - It auto-attaches at browser level (`Target.setAutoAttach` with `waitForDebuggerOnStart`, filters for tab, service_worker and shared_worker), then page under tab, and worker and iframe under page.
  - CDP events carry no session id, so a paused fetch is routed by trying each session.
  - A worker-forwarded request is intercepted only on the worker's session.
  - The quota override is enforced on writes in the page session, but `navigator.storage.estimate()` ignores it.
  - `onNewDocument` de-duplicates scripts.
  - It records dialogs, requests (with failure and from-service-worker flags), cookies and permissions.
- `src/test/java/sail/inspection/`: `InspectionCall` (a `Leg`: driver, Selenide, devTools, base URL) and `Parameters` (+ `ParametersTest`), plus these generic calls:
  - network: `Offline`, `Online`, `Throttle`;
  - request rules: `FailRequests`, `HoldRequests`, `AwaitHeldRequests`, `ReleaseRequests`, `RespondToRequests`, `LoseResponses`, `RewriteRequestHeaders`, `ClearRequestRules`, `Rules`;
  - request checks: `CheckRequests` (regex, `failed`, `exactly`/`atLeast`/`atMost`, `waitMillis`, `clear`), `CountRequests`, `RequestLog`;
  - browser: `CheckDialog`, `DeleteCookies`, `LowerQuota`, `BlockIndexedDb`, `ShiftClock`, `PressKey`, `ResizeWindow`;
  - tabs: `SwitchTab`, `CheckOpenedTab` (visits a new tab and returns within one step, because a SmartClient leg waits for SmartClient in the tab in front after every step);
  - devices: `DeviceStart`, `RestartBrowser`, `RestartDevice`;
  - files: `Media`, `PlayableVideo`;
  - `EnsureProbe`.
  - `DeviceStart` puts a profile at `<root>/<run>/<profile>`, with the root from `bhm.sail.profiles` (default `target/sail-profiles`), and nothing deletes it. On 2026-11-19, five basix worktrees held about 2,480 profiles in about 225 run folders: 89 GB, 7–26 GB per worktree. A typical profile was 35–70 MB, most of it Chrome's own downloads: `optimization_guide_model_store` (46 MB), `component_crx_cache` (47 MB), `WasmTtsEngine` (25 MB), `CertificateRevocation` (9 MB) and `OnDeviceHeadSuggestModel` (8 MB). The app's own data (`Default/Cache`, `Default/Service Worker`, IndexedDB) was 10–30 MB.
  - About 3,400 lines in 37 files. 22 of the calls use CDP; `SwitchTab`, `CheckOpenedTab`, `PressKey`, `ResizeWindow` and `EnsureProbe` don't.
- basix's `InspectionUiSailIT` and `docs/inspection-ui-sail-test-plan.md` (sections "Script steps and calls", "Devices are browser profiles", "Validation Results") show how they were used and what broke.

## Implementation plan

### Locked decisions

1. **Placement.** Everything goes in `skyve-war/src/test/java/util/sail/` (`browser`, `call` and `SailTestFiles`), so assembly delivers it. No `skyve-core` API is added: Selenium is test scope in skyve-war, and the existing `SailCall` contract is enough.
2. **Interfaces by need, not protocol:**

   | Interface | Operations | `Capability` |
   |---|---|---|
   | `RequestInterception` | add a rule (URL glob, method, request or response stage, action fail/hold/fulfil/rewrite-headers/lose-response, times), clear rules, held count, await held, release held, fail held | `REQUEST_INTERCEPTION` |
   | `RequestLog` | requests (method, URL, status, failure, from a service worker), clear | `REQUEST_LOG` |
   | `NetworkConditions` | offline, online, throttle (latency, down, up), applied to pages and service or shared workers | `NETWORK_CONDITIONS` |
   | `InitScripts` | add a script that runs before every new document (de-duplicated) | `INIT_SCRIPTS` |
   | `Dialogs` | dialogs seen, clear, answer | `DIALOGS` |
   | `Cookies` | list, delete by name (HttpOnly included) | `COOKIES` |
   | `StorageQuota` | override the quota for an origin, or clear the override | `STORAGE_QUOTA` |
   | `PageLifecycle` | freeze, resume | `PAGE_LIFECYCLE` |
   | `TouchEmulation` | max touch points | `TOUCH` |
   | `Tabs` | open, switch (waiting up to a timeout for a tab to appear), close, visit-a-new-tab-and-return | `TABS` |
   | `Keys` | press a key or chord | `KEYS` |
   | `Window` | resize | `WINDOW` |

   Method names and signatures are the implementer's, within these operations. No CDP type, JSON or method name may appear in an interface.
3. **Entry point.** `SailBrowserControl.of(WebDriver)` returns one control per driver (cached, and released once the driver has quit, at the next `of` call), chosen by browser: Chromium (Chrome, Edge) gets `ChromiumBrowserControl`, anything else `WebDriverBrowserControl`. `supports(Capability)` answers. `require(Capability)` returns the interface, or throws decision 4's exception.
4. **Unsupported means skipped.** `SailCapabilityUnavailableException extends org.opentest4j.TestAbortedException`, with the message "`<call>` needs `<capability>`, which `<browser>` doesn't support".
   - W0 checks it surfaces as skipped through `SailWorkflow`, `SailSuite` and `runStrings`. If it doesn't, make the smallest change in `SailWorkflow`/`SailSuite` to keep it as aborted (approved below).
   - A step after a skip doesn't run.
5. **Raw CDP** through `SeleniumCdpConnection`, as basix does. No `selenium-devtools-vNNN` dependency.
6. **Workers.** Network conditions and interception reach service and shared workers, using basix's attach-and-route rules (see "Current state observed"). Keep its behaviours and its comments explaining why.
7. **Calls** extend `AbstractSailCall`.
   - Each declares its parameter names; an unknown name is an error naming the call and the allowed names.
   - Each call's Javadoc lists its parameters and its variable (the return value), as basix's do.
   - A call gets interfaces only through `require`.
8. **Request checks.**
   - `CheckRequests`: `matching` (a regex over `METHOD url`), `failed`, `exactly`/`atLeast`/`atMost`, `waitMillis`, `clear`, `ignoreServiceWorkerCopies` (default true). Its variable is the count.
   - `RequestLog`: `matching`, `clear`. Its variable is the log as JSON.
   - basix's `CountRequests` (URL substring) is not ported: `CheckRequests` covers it.
   - **Log start (Mike, review round 1):** the control begins watching the first time a step asks for it, not when the browser starts. `CheckRequests` and `CheckDialog` (any check that reads the request log or the record of dialogs) fail with a clear message when watching only began in their own step, telling the author to put an earlier call (such as `RequestLog clear=true`) before the steps to watch.
9. **Test files** live in `SailTestFiles` (`util.sail`), not `SailMedia`. Each file is reused if it's already there with the right size. The playable video needs a browser (the leg's driver); the rest don't.
10. **No op-codes, and no `sail.xsd` change.** `docs/sail.md` records the promotion rule: a capability can become an op-code once it has Chromium and Firefox implementations. Scripts using op-codes then declare capabilities so unsupported browsers skip.
11. **`SailMedia` is unchanged.**
12. **Devices.**
    - `StartDevice` starts the leg's browser on a profile folder under a profile root (a new `SailSettings` key, `sail.profiles`, defaulting to `target/sail-profiles`), named by run and profile. It also accepts `offline` (`NETWORK_CONDITIONS`), `touch` (`TOUCH`) and `remembered` (keep or clear the profile). It returns the folder.
    - `RestartDevice` restarts the browser on the same profile.
    - Both use `BrowserConfiguration.profile` and `InterpretedWebDriverExecutor.getSelenide()`.
    - Read basix's `DeviceStart`, `RestartBrowser` and `RestartDevice`, and generalise away the basix-only property `bhm.sail.profiles`.
    - Both get their folder from `SailProfiles` (decision 13), not by building the path themselves.
13. **Profile housekeeping.** `SailProfiles` owns the root (`sail.profiles`). Every profile folder is `<root>/<run token>-<pid>/<profile>`, the pid being the JVM's (Mike, review round 1: two JVMs can start a run in the same millisecond). `SailProfiles` writes a marker file (`.sail-run`) in each run folder it creates (Mike, review round 1).
    - **When a run ends, after its browsers have quit:** delete `<root>/<run>` if the run passed. If it failed, keep it and print `SAIL device profiles kept for the failed run <run>: <folder>`.
      - A run of its own ends when its `runResources`, `runFiles` or `runStrings` call returns, or when its suite member does.
      - A run shared with `shareRunAcrossCalls()` ends when the workflow is closed. `SailWorkflow` has a plain `close()`; it does not implement `AutoCloseable` (Mike, review round 1, amending the W5 design). `close()` stops any kept browser, then ends the shared run's profiles. The run failed if any of its calls threw.
      - `AbstractInterpretedSailWorkflow`'s `@AfterAll` closes its workflow. A test that builds its own shared workflow closes it itself, in a `finally`.
    - **`sail.profiles.keep`**: `failed` (the default) keeps a failed run's folder, `all` keeps every run's, `none` deletes every run's.
    - **Sweep**, once per JVM, before the first profile folder is given out. It deletes each run folder under the root that is:
      - named like a run folder, `<run token>-<pid>` (base-36 letters and digits, a hyphen and digits), and holding the marker; anything else under the root is left alone and never followed through a link (Mike, review round 1: the marker too);
      - older than `sail.profiles.keepHours` (default 12) by both its token's start time and the newest last-modified time of any file inside it;
      - not in use: no profile in it has a Chrome `SingletonLock` naming a live process. On macOS and Linux the lock is a symbolic link to `host-pid`; check the pid with `ProcessHandle.of(pid)`. On Windows, a profile in use can't be deleted: a folder that fails to delete is skipped.
      - Before a run folder is deleted, it is renamed under the root to `<run token>-<pid>.deleting-<pid>` (the second pid the deleting JVM's), and its marker is deleted last. A folder that can't be renamed (on Windows, one in use) is left whole; a delete that stopped halfway is swept later.
      - This catches runs that were killed, stopped with Ctrl-C or ended in a JVM that never reached `close()`, and the failed runs that were kept.
    - Housekeeping never fails or skips a test. A folder that can't be deleted gets a one-line warning naming it and the reason.
    - Several JVMs (parallel lanes) may share a root. A sweep only deletes runs past the keep time, never the current JVM's runs, and skips profiles in use, so it is safe beside another lane's run.
14. **No Chrome background downloads.** `CrossBrowserSelenium` adds to the built-in Chrome and Edge arguments:
    - `--disable-component-update`, which stops `component_crx_cache`, `WasmTtsEngine`, `CertificateRevocation` and the other components;
    - `--disable-features=OptimizationGuideModelDownloading,OptimizationHintsFetching,OptimizationTargetPrediction,OptimizationHints`, which stops `optimization_guide_model_store` and `OnDeviceHeadSuggestModel`. W0 (d) confirms the feature names against the installed Chrome.
    - Chrome honours only the last `--disable-features`. If a configuration's extra arguments include one, merge its list with the built-in one into a single argument.
    - Not `--disable-background-networking`, unless W0 (d) shows it is needed and harmless: it is broader than these downloads.

### Approved new constructs

- the packages `util.sail.browser` and `util.sail.call`, and the classes and interfaces named in Scope and in decisions 2–12;
- `SailSettings` keys `sail.profiles`, `sail.profiles.keep` and `sail.profiles.keepHours`;
- `SailProfiles`, `SailWorkflow.close()` (a plain method, not `AutoCloseable`: amended by Mike in review round 1), the `@AfterAll` close in `AbstractInterpretedSailWorkflow`, and the extra Chrome and Edge arguments (decisions 13 and 14);
- the smallest `SailWorkflow`/`SailSuite` change for decision 4, if W0 shows it's needed;
- the decision-4 change in `InterpretedWebDriverExecutor` (approved by Mike on 2026-11-19): `executeCall` and `runSteps` rethrow a `TestAbortedException` as it is, under every strategy, so no later step runs; under Verify, `executeAutomation` lets failures recorded before the skip win, naming the skip in the message;
- test fixtures under `src/test/resources/util/sail/browser/**`.

Anything else needs Mike's approval: ask, with the reason and the smallest alternative.

### Work items

- **W0, spike (throwaway):**
  - (a) `SeleniumCdpConnection` raw commands work from skyve-war test code against the installed Chrome;
  - (b) a `TestAbortedException` thrown from a `<call>` reaches JUnit as skipped through `SailWorkflow.runStrings`, a `SailSuite` member and an IT. Record what happens, and the smallest change if not;
  - (c) `HarnessSelfContainedTest` still passes with new packages present;
  - (d) with decision 14's flags, a headless Chrome on a `--user-data-dir`, restarted on the same folder, still keeps its IndexedDB, CacheStorage, service worker registration and a persistent cookie. Record the profile's size and largest folders with and without the flags, after loading a page with a service worker, and which of the five download folders are still created.

  Delete the spike, and record the results in "AI validation results".
- **W1, interfaces and capability model:**
  - `Capability`, the interfaces, `SailBrowserControl` (`of`, `supports`, `require`), the exception;
  - `WebDriverBrowserControl`;
  - unit tests with a fake `WebDriver`, plus a fake control for later items.
- **W2, Chromium implementation:**
  - port basix's `SailDevTools` and `FetchRule` behind the interfaces;
  - port `FetchRuleTest`;
  - a self-contained IT, `ChromiumBrowserControlIT`: a JDK `HttpServer` serves a page and a service worker; it proves offline (page and worker), interception at both stages (fail, hold and release, fulfil, rewrite headers, lose response), the request log with worker flags, init scripts across navigation, dialogs, cookies, quota on writes, freeze and resume, and touch. Fixtures go under `src/test/resources/util/sail/browser/`.
- **W3, call base and standard calls:**
  - `AbstractSailCall`, `SailCallParameters` (port `ParametersTest`), every call in Scope 4;
  - unit tests for each call against the fake control: happy path, parameter validation, and the unsupported-means-skipped path;
  - `StandardCallsSailIT`: one SAIL script driving the calls with `<call className="util.sail.call.…">` against the W2 fixture server.
- **W4, `SailTestFiles`:** port basix's `Media` and `PlayableVideo`, with tests: sizes, dimensions, reuse, and a playable video, checked by loading it into a `<video>` and reading its duration.
- **W5, profile housekeeping and launch flags** (decisions 13 and 14):
  - `SailProfiles` and its settings; `StartDevice` and `RestartDevice` use it;
  - `SailWorkflow`: end each run's profiles; `close()` (`AutoCloseable` until review round 1); `AbstractInterpretedSailWorkflow` closes it in `@AfterAll`;
  - the Chrome and Edge arguments, with the `--disable-features` merge;
  - `SailProfilesTest`, on a temporary root with no browser: a passed run is deleted, a failed one is kept, `keep` all and none; the sweep deletes an old run, keeps a recent one (old token but a fresh file), keeps one with a `SingletonLock` naming this JVM's pid, deletes one whose lock names a dead pid, and leaves a non-token folder and a symbolic link alone;
  - `CrossBrowserSeleniumTest`: the new arguments are present, and an extra `--disable-features` is merged, not duplicated;
  - a `SailWorkflow` test: a shared run's folder survives across calls and is deleted on `close()` after passing calls, and kept after a failing one.
- **W6, docs:** `docs/sail.md`, the release notes, and `docs/architecture-design.md` if applicable. Check them against what was built.

## Acceptance checklist

- [ ] W0 results recorded. Decision 4 holds: an unsupported call reports skipped in a workflow, a suite and an IT.
- [ ] Every interface in decision 2 exists with no CDP leakage. `ChromiumBrowserControl` supports all of them. `WebDriverBrowserControl` supports cookies, tabs, keys and window, and reports the rest unsupported.
- [ ] `ChromiumBrowserControlIT` passes three runs in a row, headless, plus once headed.
- [ ] Every call in Scope 4 exists, with Javadoc listing its parameters and variable, and passes its unit tests, including the skip path.
- [ ] `StandardCallsSailIT` passes on Chrome three runs in a row. On Firefox, if installed, the Chromium-only calls skip with the message and the rest pass. If Firefox isn't installed, record that.
- [ ] `SailTestFiles` produces each file type, and the video plays.
- [ ] W0 (d) recorded: profiles still keep IndexedDB, CacheStorage, the service worker and cookies across a restart with the new flags, and a profile's size before and after.
- [ ] After `StandardCallsSailIT` passes, its run's folder under `target/sail-profiles` is gone. After a forced failure, it is kept and its path printed.
- [ ] `SailProfilesTest` covers every case in W5, and housekeeping never fails a test.
- [ ] `HarnessSelfContainedTest` and every existing `util.sail` test pass.
- [x] The docs describe the capabilities, the calls, skipping and the op-code rule.
- [ ] Gates pass.

## Better engineering notes

- **Phase 2:** `BiDiBrowserControl` (Firefox and Chrome) for request interception, the request log, init scripts and dialogs (`network.addIntercept`, `script.addPreloadScript`, `browsingContext` events). Then those capabilities meet the op-code rule. Network conditions, quota and lifecycle stay Chromium-only until BiDi settles.
- **`SailSuite` runner features** from basix's `InspectionUiSailIT`: per-test isolation split by interaction id, carrying on after a failure, file set-up and clean-up, and filters by member, test id and priority. That needs its own plan. With the password-prompt fix in, basix can then go back to `SailSuite`.
- **`SailMedia`'s screencast** could move under a `SCREENCAST` capability, so it degrades with a clear note on other browsers.
- **Docs:** a "parallel lanes" section in `docs/sail.md` (several servers, each with its own restored database), from basix's experience.
- **basix migration:** once assembled, basix deletes its copies, and its suite XML switches `className`s to `util.sail.call.*`. `InspectionUiSailIT` builds its own shared workflow, so it must close it (decision 13) in its `@AfterAll`, or its profiles wait for the sweep. Lanes passing `-Dbhm.sail.profiles` switch to `sail.profiles`.

## AI validation plan

- `mvn -o -B -pl skyve-war -am package`, with `util/sail/**` tests and `HarnessSelfContainedTest`.
- The ITs: `ChromiumBrowserControlIT` and `StandardCallsSailIT` (Chrome headless ×3, headed ×1), and Firefox if available.
- Red/green for each decision-level behaviour:
  - unsupported means skipped;
  - an unknown parameter is rejected;
  - offline reaches the worker;
  - a worker-forwarded request is intercepted;
  - `CheckRequests` `waitMillis`;
  - `SwitchTab` waits for a new tab;
- a passed run's profiles are deleted, a failed run's kept;
- the sweep skips a profile in use.
- JaCoCo on changed lines (test-harness code, so no 95% target, but list what's uncovered).
- JDT 3.48 warnings on changed lines: none.
- Sonar on the new files: none.
- Javadoc per `docs/ai/javadoc-standards.md` on every public type and method.
- Spotless: `util/**` is excluded in skyve-war, so format by hand to match.

## AI validation results

**Mike's decisions during the run (2026-11-19):** the decision-4 change may extend to `InterpretedWebDriverExecutor` (approved), and under Verify an earlier failure wins: if failures were recorded before a call skips, the test fails with them, the message mentions the skip, and no later step runs. Only with no recorded failures is the test skipped.

### W0

Spike on 2026-11-19, then deleted (nothing committed). Chrome 158.0.8127.37 (arm64), ChromeDriver 158.0.8127.36, Selenium 4.38.0 (from Selenide 7.12.1), JDK 21.0.2, headless, from skyve-war test code through surefire, with pages from a JDK `HttpServer` on a loopback port (no Skyve server).

**(a) Raw CDP works.** `SeleniumCdpConnection.create(driver)` and `Connection.sendAndWait(sessionId, new Command<>(method, params, Json.MAP_TYPE), timeout)`, as basix does, ran `Browser.getVersion`, `Target.getTargets`, `Target.attachToTarget` (flatten), `Runtime.evaluate`, `Network.enable` and `Network.emulateNetworkConditions` (offline: `navigator.onLine` was false through CDP and WebDriver). No `selenium-devtools-vNNN` module was needed. `Target.getTargets` also lists a `browser_ui` target (`chrome://omnibox-popup.top-chrome/`), which W2's attach filters should leave out.

**(b) A `TestAbortedException` from a `<call>` was reported as failed.** A control `@Test` that threw it directly was skipped, so JUnit and surefire are not the cause.
- Assert and None: failed, wrapped three times: `InterpretedWebDriverExecutor.executeCall`'s `catch (Exception e)` made it an `IllegalStateException`, `runSteps` wrapped that in a `SailFailure`, and `SailWorkflow.run` in an `AssertionError`.
- Verify: failed; the skip was recorded as an ordinary failure, and the next interaction still ran.
- Suite: the member counted as failed (`SailSuite.runMember` catches `RuntimeException`), and the suite failed.
- The smallest change, tried in the spike and reverted (it needs `InterpretedWebDriverExecutor` beyond the approved `SailWorkflow`/`SailSuite`): rethrow `TestAbortedException` in `executeCall` and `runSteps`; in `SailWorkflow.run`, rethrow it as `TestAbortedException("SAIL workflow: <leg> skipped: …")`; in `SailSuite`, count an aborted member as skipped, and when nothing else failed make `run()` throw `TestAbortedException` with the summary.
- With the change: Assert, Verify and None each reported skipped, with no later step (or, under Verify, interaction) run. A suite of an aborting and a passing member reported skipped ("1 passed, 0 failed, 1 skipped"); an aborting and a failing member reported failed. `SailWorkflowTest` (18), `SailSuiteTest` (14), `InterpretedWebDriverExecutorTest` (66) and `HarnessSelfContainedTest` (1) still passed.
- Open point, settled by Mike above: under Verify, the spike's change lost a failure recorded before the skip.

**(c)** `HarnessSelfContainedTest` passed with throwaway classes in `util.sail.browser` (importing `org.openqa.selenium.devtools.*`) and `util.sail.call`.

**(d) Profiles and the decision-14 flags.** Each profile was started through `CrossBrowserSelenium` on a `--user-data-dir`, loaded a page that registered a service worker, wrote IndexedDB and CacheStorage, and got a persistent cookie; the browser was kept open for a measured window (240 s, or 60 s for the single-flag runs) while the profile was polled for the five download folders, then restarted on the same folder.
- Persistence: in all four configurations the restarted browser kept IndexedDB, the CacheStorage entry, the service worker registration (controlling the page) and the cookie. The flags break none of them.

| Configuration | Profile total | `optimization_guide_model_store` | `component_crx_cache` | `WasmTtsEngine` | `CertificateRevocation` | `OnDeviceHeadSuggestModel` |
|---|---|---|---|---|---|---|
| No flags (240 s) | 151.2 MB | 46.7 MB, at about 14 s | 47.3 MB | 25.0 MB | 9.3 MB | 7.7 MB |
| Both flags (240 s) | 12.3 MB | absent | 4 KB (`metadata.json`) | absent | absent | absent |
| `--disable-component-update` only (60 s) | 54.1 MB | 46.7 MB, at about 12 s | 4 KB | absent | absent | absent |
| `--disable-features=...` only (60 s) | 8.5 MB | absent | 140 KB | empty | empty | empty |

- Both flags are needed: `--disable-component-update` alone leaves the model store, and `--disable-features` alone leaves the component updater on.
- Chrome doesn't reject unknown `--disable-features` names, so the names are shown to work only by behaviour: with the list, the model store never appeared. Which name does the work was not isolated.
- ChromeDriver adds `--disable-features=IgnoreDuplicateNavs,Prewarm` and merged ours into it (chrome://version). Whether it merges two user-supplied `--disable-features` was not tested; W5 merges them itself.
- `--disable-background-networking` is not needed: ChromeDriver already adds it, and the downloads happened anyway.

Not verified in W0: headed Chrome, Edge, Firefox, runs longer than 240 s, and which feature name stops the model store.

### W1

- Built `util.sail.browser`: `Capability` (each constant names its interface), the twelve interfaces of decision 2, the value types `RequestRule` (with its `Stage`, `ErrorReason` and actions `Fail`, `Fulfil`, `Hold` and `Headers`; a lost response is a `Fail` at the response stage, made by `RequestRule.loseResponse`), `LoggedRequest`, `Dialog` and `BrowserCookie`, `SailBrowserControl`, `SailCapabilityUnavailableException` and `WebDriverBrowserControl`; and the test helper `FakeBrowserControl` for W3.
- `require` takes the interface and the step's name, `require(NetworkConditions.class, "Offline")`, so it returns the interface typed; the capability comes from the interface (`Capability.of`).
- A control implements the interfaces of the capabilities it supports, and `supports` defaults to that, so `ChromiumBrowserControl` can extend `WebDriverBrowserControl` (cookies, tabs, keys and window by WebDriver, as the plan says) and add the rest.
- `of` keeps one control per driver and drops those of drivers that have quit (a `RemoteWebDriver` with no session id) each time it is called. It picks by browser at one seam, `SailBrowserControl.create`: a `ChromiumDriver` gets `WebDriverBrowserControl` until W2.
- The tab waits use a `WebDriverWait` polling every 100 ms, as `Selenide.waitFor` does, bounded by the caller's timeout; a tab that doesn't appear fails with an `AssertionError`.
- `RequestRuleTest` is basix's `FetchRuleTest` for the value type (globs, matching, narrowing, checks, copying). Its `mergeHeaders` and DevTools-name cases belong with W2's port.
- Decision 4 as approved: W0's change plus "an earlier failure wins" under Verify.
- Gate: `mvn -o -pl skyve-war -am test -Dtest='util.sail.**.*Test,sail.HarnessSelfContainedTest' -DskipUnitTests=false -DskipIntegrationTests=true -Dsurefire.failIfNoSpecifiedTests=false`: 262 tests in 18 classes, 0 failures, 0 errors, 0 skipped, no flakes. New: `SailBrowserControlTest` (8), `WebDriverBrowserControlTest` (13), `RequestRuleTest` (6), and 2 in `InterpretedWebDriverExecutorTest`, 1 in `SailWorkflowTest` and 2 in `SailSuiteTest`.
- Red/green on "unsupported means skipped": with the rethrow in `executeCall` removed, `InterpretedWebDriverExecutorTest` failed 2 of 68 (the skip became a `SailFailure` under Assert, and under Verify the skip was not attached); restored, 68 of 68 passed.
- ECJ 3.41 with skyve-war's JDT settings on the changed files: no warnings.

### W2

- Built `ChromiumBrowserControl`, ported from basix's `SailDevTools` with its attach and routing rules and the comments explaining them. It extends `WebDriverBrowserControl`, so cookies, tabs, keys and window stay WebDriver, and adds the eight CDP-backed interfaces.
  - It auto-attaches at browser level with `waitForDebuggerOnStart`, filtered to tab, service_worker and shared_worker, so `browser_ui` is left out. Pages attach under tabs, and workers and iframes under pages.
  - A paused request is answered by trying each session.
  - Rules and network conditions go to every page and worker session.
  - The quota is set in a page session.
  - Init scripts are de-duplicated.
  - CDP stays private to the class. `SailBrowserControl.create` returns this control for any `ChromiumDriver` (Chrome and Edge).
- Choices made within the plan:
  - basix's two sleep-polls (waiting for the current targets to attach, and `awaitHeld`) became condition waits, woken by the attach and hold events and bounded as before.
  - The control connects to DevTools when `of` makes it. With a lazy connection, the first IT run showed that a dialog already open when the control attaches is never reported. If it can't connect then, it tries again at the first DevTools use.
  - `of` now calls a package-private `release()` on the control of a driver that has quit. This closes the connection and its thread.
  - `Dialogs` and `RequestLog` now say they record from when the control began watching, not from the browser's start.
  - A failed response paused at the response stage carries on through `Fetch.continueRequest`, because it has no status for `continueResponse`.
- `ChromiumBrowserControlTest` (5 tests) is the rest of basix's `FetchRuleTest`: the DevTools names of stages, of errors (every constant) and of dialogs; `mergeHeaders`; and the command each rule answers a paused request with. `SailBrowserControlTest` now expects the Chromium control, with every capability, for a Chrome driver.
- `ChromiumBrowserControlIT` (17 tests, tagged `sail`).
  - Setup: a JDK `HttpServer` on an ephemeral loopback port serves `page.html` and `sw.js` (fixtures under `src/test/resources/util/sail/browser/`) and counts requests by path. The browser starts through `PrimeFacesSelenide` and `BrowserConfiguration` (headless unless `-Dsail.headless=false`), and is quit after each test.
  - Network: offline for the page and the worker, and a worker that Chrome restarts stays offline; throttling; a new tab gets the settings.
  - Interception: fail once, and fail by method; hold, then release or fail; fulfil at both stages; change request and response headers; a lost response that the server still handled; a worker-forwarded request fulfilled and held.
  - Request log: the page's service-worker copy, the worker's own request, and a failed request.
  - Also: init scripts across navigation and reload (added once); confirm and prompt answered; beforeunload recorded; cookies, HttpOnly included; quota on writes; freeze and resume (seen through the page's `freeze` and `resume` events); and touch.
  - Waits are on named events (the page's promise, the held count, DevTools events reaching the control), bounded by the SAIL timeout.
- Chrome 158.0.8127.37, ChromeDriver 158.0.8127.36, Selenium 4.38.0. Command: `mvn -o -pl skyve-war -am test -Dtest=ChromiumBrowserControlIT -DskipUnitTests=false -DskipIntegrationTests=true -Dsurefire.failIfNoSpecifiedTests=false -Dsurefire.rerunFailingTestsCount=0 -DforkCount=1`. Three headless runs in a row passed 17/17 each (about 13 s). One headed run (`-Dsail.headless=false`, under the headed lock) passed 17/17.
- Red/green:
  - With network conditions sent to page sessions only, `offlineReachesThePageAndTheServiceWorker` and `aServiceWorkerChromeStartsAgainStaysOffline` failed ("the worker's request: expected ERR but was 200 real").
  - With `Fetch.enable` sent to page sessions only, `aWorkerForwardedRequestIsIntercepted` failed ("expected 503 busy but was 200 real"), while the page-level `fulfilAtTheRequestAndTheResponse` still passed.
  - Restored, both passed.
- Gate: `-Dtest='util.sail.**.*Test,sail.HarnessSelfContainedTest'` ran 267 tests: 0 failures, errors or skips, and no flakes. ECJ 3.41 with skyve-war's JDT settings on the changed files: no warnings.
- Not verified: Edge, and a remote (grid) Chromium without a DevTools endpoint.

### W3

- Built `util.sail.call`:
  - `AbstractSailCall`: checks the step's parameters against the names the call takes, then runs it with a `Context` (the driver and control of the browser running now, so both follow a restart; the leg's Selenide; the base URL; the run token; the leg's timeout). A call gets interfaces only through `require(context, type)`, which is `SailBrowserControl.require` with the call's simple name.
  - `SailCallParameters`, ported with `ParametersTest` (as `SailCallParametersTest`); "needs a n parameter" became "needs the parameter n".
  - The 25 calls of Scope 4, each with Javadoc listing its parameters, its variable and what it needs.
- `SailSettings`: the key `sail.profiles` (`PROFILES`, `DEFAULT_PROFILES_DIR` = `target/sail-profiles`, `Code.profiles(Path)`, `profiles()`), with a `SailSettingsTest` case. `StartDevice` builds `<root>/<run token>/<profile>` in one private method, `folder`, for W5's `SailProfiles` to replace.
- Choices made within the plan:
  - A call reads all its parameters before it asks for an interface, so a script's mistake fails in every browser, and only a valid step skips.
  - Waits default to the leg's timeout (`sail.timeoutSeconds`): `AwaitHeldRequests` and `CheckOpenedTab` (both keep `timeoutMillis`), and `SwitchTab`'s wait for a tab. basix used 20 s, 30 s and 10 s.
  - basix's sleep loops became condition waits. `CheckRequests` `waitMillis` is a `FluentWait` over the log every 100 ms, because the log raises no event to wait on. `CheckOpenedTab` waits for its script with a `WebDriverWait`, and a lost window or session fails at once.
  - `RequestLog`'s variable is a JSON array of objects (`method`, `url`, `status`, `failure`, `fromServiceWorker`), escaped with `OWASP.escapeJsonString`. basix gave `[method, url, status]` arrays.
  - `FreezeTab`'s actions are `freeze` and `resume`, as the interface names them. basix used `thaw`.
  - `RewriteRequestHeaders` refuses a step with neither `set` nor `remove`.
  - The `method` and `times` narrowing shared by the rule calls is a package-private `FailRequests.narrow`. basix's `Rules` class was not ported.
  - Devices (decision 12):
    - `remembered` keeps (`true`, the default) or clears the profile before the browser starts on it. basix's `DeviceStart` used `remembered` to check for a `remember` cookie; that check is not ported.
    - `offline` and `touch` default to false (basix defaulted `touch` to true). The capability is required only to change the browser: `offline=true` needs `NETWORK_CONDITIONS` and `touch=true` needs `TOUCH`. Otherwise `online` and touch off are applied where the browser supports them, as a restarted browser has no settings.
    - `RestartDevice` took in all that basix's `RestartBrowser` did (restart on the same profile, then apply the network and touch). It returns the folder.
    - In a browser that can't take a profile (anything but Chrome or Edge), both skip with a plain `TestAbortedException`: "`StartDevice` needs a device profile, which firefox doesn't support". There is no `Capability` for profiles, so `SailCapabilityUnavailableException` can't be used. The message has decision 4's shape.
- **A W2 fix: a tab the page opens itself never ran.** A `window.open` (or `target="_blank"`) page under `ChromiumBrowserControl` hung every driver command ("Timed out receiving message from renderer"). The page attaches with `waitingForDebugger=false`, but Chrome 158 holds it anyway. `attached` now sends `Runtime.runIfWaitingForDebugger` to every target, which does nothing to one that isn't waiting. `ChromiumBrowserControlIT.aTabThePageOpensRunsWithTheSettings` covers it. With the old condition it failed after 30 s with that timeout; restored, `ChromiumBrowserControlIT` passed 18/18 twice.
- Unit tests, against `FakeBrowserControl` (35 tests in 6 classes, no `@TestFactory`):
  - `AbstractSailCallTest`: a table of all 25 calls (`assertAll`, the call's name in each message). Each refuses an unknown parameter, naming itself and the names it takes. Each skips in a browser without what it needs, naming itself, the capability and the browser, and does nothing first. Also: a call needs a browser; the context follows a restart.
  - `RequestCallsTest`, `BrowserCallsTest`, `TabCallsTest` and `DeviceCallsTest`: each call's happy path, defaults, parameter checks and variable.
  - `TabCallsTest.switchTabWaitsForANewTab` runs `SwitchTab` on plain WebDriver's control over a mock driver whose new tab appears at the third look.
- `StandardCallsSailIT` (6 tests, tagged `sail`; `AbstractInterpretedSailWorkflow`, a JDK `HttpServer` serving the W2 fixtures, no Skyve server, no sign-in). Each test is one `<automation>` of `<call className="util.sail.call.…">`, script and `navigateLink` steps.
  - `theCallsEveryBrowserHasRun`: `DeleteCookies` (HttpOnly included), `ResizeWindow`, `PressKey` (trusted keys), and `SwitchTab` open, switch, close and wait for a tab the page opens 500 ms later. Then `CheckOpenedTab`.
  - The other five each start with a call that needs a Chromium capability or a profile:
    - network: `Online`, `Offline` (page and worker), `Throttle`;
    - request rules: fail once, hold then release or fail, respond, lose, rewrite, clear, with the server's hit counts checked in Java;
    - request checks: `RequestLog` clear and JSON, `CheckRequests` with and without service-worker copies, `waitMillis` for a request sent 500 ms later, `failed`;
    - browser: `LowerQuota` and restore, `BlockIndexedDb` and `ShiftClock` from the next page, a `beforeunload` dialog after `PressKey` gave the page user activation (`CheckDialog`), `FreezeTab` freeze and resume;
    - devices: `StartDevice` with touch, `RestartDevice` keeping localStorage and then offline, another device, back, and `remembered=false`. Profiles go under a temporary `sail.profiles`, deleted in `@AfterAll`; the run's folder held `phone1` and `phone2`.
- Runs, with Chrome 158.0.8127.37, Firefox 159.0 and geckodriver 0.36.0. Command: `mvn -o -pl skyve-war -am test -Dtest=StandardCallsSailIT -DskipUnitTests=false -DskipIntegrationTests=true -Dsurefire.failIfNoSpecifiedTests=false -Dsurefire.rerunFailingTestsCount=0 -DforkCount=1`.
  - Chrome headless: three runs in a row passed 6/6 each (about 17 s).
  - Firefox headless (`-Dsail.browser=firefox -DargLine=-Dwebdriver.gecko.driver=~/.cache/selenium/geckodriver/mac-arm64/0.36.0/geckodriver`): 1 passed, 5 skipped. The skip messages: "SAIL workflow: script 1 skipped: …"
    - "Online needs NETWORK_CONDITIONS, which firefox doesn't support";
    - "FailRequests needs REQUEST_INTERCEPTION, which firefox doesn't support";
    - "RequestLog needs REQUEST_LOG, which firefox doesn't support";
    - "LowerQuota needs STORAGE_QUOTA, which firefox doesn't support";
    - "StartDevice needs a device profile, which firefox doesn't support".
- Red/green, each restored afterwards:
  - An unknown parameter is rejected: with `SailCallParameters`' check disabled, `SailCallParametersTest.unknownParametersAreRefusedByName` and `AbstractSailCallTest.everyCallRefusesAParameterItDoesNotTake` (25 failures) failed, and so did `aCallNeedsABrowser`.
  - `CheckRequests` `waitMillis`: with the wait never entered, `checkRequestsWaitsForARequestTheStepBeforeStarted` and `checkRequestsStopsWaitingAtTheTime` failed.
  - `SwitchTab` waits for a new tab: with `WebDriverBrowserControl`'s wait given a zero timeout, `TabCallsTest.switchTabWaitsForANewTab` failed ("Tab 1 (0 being the first) did not open within PT7S: 1 tab(s) are open").
- Gate: `-Dtest='util.sail.**.*Test,sail.HarnessSelfContainedTest'` ran 303 tests: 0 failures, errors or skips, and no flakes.
- ECJ 3.41 (`--release 17`, skyve-war's JDT settings, `-d` a scratch folder) on the new and changed files gives one warning: "Unnecessary @SuppressWarnings("resource")" at `ChromiumBrowserControl` line 116. It was already there at `c7791dd042`, and no W3 line is involved.
- Not verified: a headed run, Edge, and Safari.

### W4

- Built `util.sail.SailTestFiles`, ported from basix's `Media` and `PlayableVideo`, with their comments. Files go under `target/sail-test-files/`, written under a unique temporary name and then moved into place, so a partly written file is never reused and JVMs sharing the folder don't collide.
  - `photo(long bytes)`: exactly that many bytes. Seeded noise up to 12 megapixels fills most of it, and JPEG comment segments of seeded random bytes, inserted after the JFIF segment, pad it to the exact size. Reused if the size matches exactly.
  - `photo(int width, int height)`: a labelled gradient (basix's `photo-small.jpg`). Reused if the dimensions in its header match.
  - `oversizePhoto()`: basix's 9000 x 7000 gradient with a block of noise, over 10 MB (Skyve's default image upload limit), about 14 MB. Reused if over 10 MB.
  - `video(WebDriver, long bytes)` and `video(WebDriver, long bytes, Duration timeout)`: a WebM of at least that many bytes, recorded in the page in front (canvas noise, `captureStream`, `MediaRecorder` VP8 at 24 Mbit/s, as basix), then read back in 1 MB pieces. Reused if at least that size. The two-argument form uses the SAIL timeout.
- Choices within the plan:
  - basix paced frames with a 66 ms `setTimeout`. Now frames are drawn on `requestAnimationFrame`. The recording stops on the `dataavailable` event that reaches the size, and the promise settles on the recorder's `stop` event. The bound is a page deadline of the timeout, with WebDriver's script timeout set a little past it and put back afterwards.
  - Not ported: basix's `tooLarge(bytes)` file of zeros (`photo(bytes)` covers upload limits) and the non-playable `clip-20mb.mp4`.
- `SailTestFilesTest` (7 tests, no browser): exact sizes (2,000 B to 20 MB + 3, either side of 4 and 10 MB) that decode with ImageIO; padding edge cases (a full 64 KB segment with 1 to 3 bytes left over); dimensions read back with ImageIO (800 x 600, 1 x 1, 3 x 2000, 4000 x 3000); the oversize photo's header dimensions and size; reuse, checked by an old modification time and the same file key being kept, including the video's, with no calls on the driver; a wrong-size file made again, and a short video sent back to the browser; refused sizes and dimensions. One bug found and fixed on the first run: the shrink loop never ended for a size under the smallest JPEG.
- `SailTestFilesIT` (1 test, tagged `sail`, headless Chrome, JDK `HttpServer` on an ephemeral loopback port): records a 6 MB video (6,457,484 bytes) and checks the EBML header. It chooses the video in a file input, plays it in a `<video>`, and finds a duration of 3.7 s, 640 x 360, and a frame shown after seeking to the middle. The page then no longer holds the video. A second call reuses the file without writing it. The duration was `Infinity` at `loadedmetadata`, as expected for `MediaRecorder` output. Seeking past the end gave `durationchange` with the real duration.
- Gate: `mvn -o -pl skyve-war -am test -Dtest='SailTestFilesTest,SailTestFilesIT,sail.HarnessSelfContainedTest' -DskipUnitTests=false -DskipIntegrationTests=true -Dsurefire.failIfNoSpecifiedTests=false -Dsurefire.rerunFailingTestsCount=0 -DforkCount=1` ran 9 tests: 0 failures, errors or skips, and no flakes. The IT also passed on its own once before that. ECJ 3.41 with skyve-war's JDT settings on the three files: no warnings.
- Not verified: Firefox, Safari and Edge recording (`MediaRecorder` WebM support differs), and headed runs.

### W5

- Built `SailProfiles` (`util.sail`), with the keys `sail.profiles.keep` (`SailSettings.KeepProfiles`: `failed`, the default, `all`, `none`) and `sail.profiles.keepHours` (default 12, a whole number above 0) beside `sail.profiles`, and a `SailSettingsTest` case.
  - `SailProfiles.folder(settings, run, profile)` gives `<root>/<run token>/<profile>` and records, for the run, what it keeps when it ends (the last folder given out says). `end(run, passed)` (package-private) deletes `<root>/<run>` if the run passed, or keeps it and prints `SAIL device profiles kept for the failed run <run>: <folder>`; `all` keeps every run's (printing only a failed one's), `none` deletes every run's. A run with no folder does nothing, and a run ends once.
  - The sweep runs before the first folder under a root is given out. It deletes a folder under the root only if it is a real folder (not a link) named `[0-9a-z]{8,13}` (a run token; `RunState`'s are 8 characters since 1973, and a name too big for a long is not one), isn't one of this JVM's runs, is older than `keepHours` by its token and by the newest last-modified time of anything in it (links included, not followed), and has no profile whose `SingletonLock` link names a live process (`ProcessHandle.of(pid)`). A folder that fails to delete, or to be checked, gets one line, `SAIL device profiles: could not delete <folder>: <exception>`, and is left; nothing is thrown.
  - Root, clock and printer are injectable through a package-private constructor; the public entry point keeps one instance per root.
  - `delete(Path)` is public, because `StartDevice` uses it for `remembered=false`, in place of its own copy.
- `StartDevice` gets its folder from `SailProfiles.folder`, and its private `folder` and `delete` are gone. `RestartDevice` now asks `SailProfiles` for the run's folder of the profile's name and refuses a profile that isn't it (another run's, or one set outside `StartDevice`) with its existing message; `DeviceCallsTest` covers that.
- `SailWorkflow implements AutoCloseable`.
  - A run of its own (`runResources`, `runFiles`, `runStrings`, `runWorkflow`, a suite member) ends its profiles in the run's `finally`, after its browser has stopped.
  - A suite member that passes and keeps its browser for the next member leaves its run waiting for that browser. The waiting runs move with the browser into the next run, and end, as passed, once it stops: at the end of a run that doesn't keep it, or in `closeBrowser()`, which the suite calls in its `finally`. A browser-per-member member has no kept browser, so it ends its run when it ends.
  - `close()` calls `closeBrowser()`, then ends the shared run's profiles: failed if any of its calls threw (a skip counts as thrown). `shareRunAcrossCalls()` resets that flag with its new run.
  - `AbstractInterpretedSailWorkflow` keeps its tests' workflows in a static queue. Its `@AfterAll`, `closeWorkflowsAndReleaseServer()` (was `releaseServer()`), closes them all and then releases the server lock. It needs a queue because JUnit makes an instance, and so a workflow, for each test unless a class asks for one instance.
- `CrossBrowserSelenium`: Chrome (after the three backgrounding flags) and Edge (first) start with `--disable-component-update` and `--disable-features=OptimizationGuideModelDownloading,OptimizationHintsFetching,OptimizationTargetPrediction,OptimizationHints`. The features of any `--disable-features=` among a configuration's extra arguments are merged into that one argument (in order, each once, blanks dropped), and the extra itself is left out. `DeviceProfileTest`'s expected built-in arguments now include both.
- **Fallout from `AutoCloseable`: ECJ resource warnings.** With skyve-war's annotation-based resource analysis, making `SailWorkflow` closeable gave 23 ECJ warnings at existing sites. They came from fluent setters returning the workflow, workflow fields ("mark as `@Owning`"), and engines handed to other objects. I fixed them the way the repo already does:
  - `SailSuiteTest` (3) and `SailLoadTest` (2) now close their engines with try-with-resources;
  - `@SuppressWarnings("resource") // <why>` went on `SailLoad.VirtualUser`, `runEntries`, `runUser`, `runIteration` and `engine`, on `SailSuite`'s `engine` field and `settings(Code)`, on `AbstractInterpretedSailWorkflow`, and on `SailWorkflowTest`.
  - ECJ now reports none in any file that uses `SailWorkflow`. Mike may prefer another approach (see "Open").
- `StandardCallsSailIT` now uses the default root (`skyve-war/target/sail-profiles`), not a temporary one. Its device test checks that no run folder newer than the test's start is left after the passed run. Before, the test checked the folder was still there: it is now deleted.
- Tests (no `@TestFactory`):
  - `SailProfilesTest` (6), on a `@TempDir` root with no browser:
    - a passed run deleted and a failed one kept and printed once;
    - `keep` all and none;
    - a run without a folder;
    - one sweep test: an old run deleted; old token with a fresh file kept; recent token with old files kept; `SingletonLock` naming this JVM's pid kept; naming a dead pid (a finished `true` process) deleted; a non-token name and a too-short name left; a link named like an old token left, and what it points to untouched; an old run with a link inside deleted without following it; this JVM's run never swept; the sweep runs once;
    - a root that doesn't exist;
    - unwritable folders (POSIX `r-x`) in an old run and in a passed run: two warnings and both left, with nothing thrown.
    - The JDK can't set a dangling link's own time on macOS ("Bad file descriptor"), so the test ages links with `touch -h -t`.
  - `CrossBrowserSeleniumTest` (2, new: there was no such class): the two arguments are in Chrome's and Edge's arguments and options, headed and headless, and `--disable-background-networking` isn't; two extra `--disable-features` merge into the one built-in argument without duplicates, and the other extra is kept.
  - `SailWorkflowTest` (+4): a run of its own (passed deleted, failed kept); a shared run's folder survives two calls and is deleted on `close()`; a shared run with a failed call keeps it on `close()`; a passed member's folder waits for its kept browser through the next member and is deleted at `close()`, while a browser-per-member member's is deleted when it ends.
- Gate:
  - `-Dtest='util.sail.**.*Test,sail.HarnessSelfContainedTest' … -Dsurefire.rerunFailingTestsCount=0 -DforkCount=1`: 316 tests, 0 failures, errors or skips, no flakes.
  - `StandardCallsSailIT`, Chrome headless: 6/6 passed. `skyve-war/target/sail-profiles` didn't exist before; afterwards it existed and was empty.
  - Forced failure (a temporary `return false` test script at the end of the device test, then restored): it printed `SAIL device profiles kept for the failed run mwn3547s: …/skyve-war/target/sail-profiles/mwn3547s`. The folder held `phone1` (8.6 MB) and `phone2` (8.4 MB), with no `SingletonLock` left after the browser quit. I deleted it afterwards.
  - `ChromiumBrowserControlIT`, headless: 18/18.
- Red/green, each restored:
  - With the delete in `endRun` removed, `SailProfilesTest` failed 3 (passed/failed, keep all/none, the unwritable-folder warnings) and `SailWorkflowTest` failed 3 (own run, shared run on close, kept browser).
  - With a live lock's process treated as not alive, `theSweepDeletesOnlyOldUnusedRunFolders` failed: "a run whose lock names a live process is kept".
  - Restored, both classes passed: 29/29.
- ECJ 3.41 (`--release 17`, skyve-war's JDT settings, `-d` a scratch folder) on every changed file and every file that uses `SailWorkflow` (the SAIL ITs and `SailLoad` included): no warnings.
- Choices made within the plan:
  - The sweep runs once for each root in a JVM, not once per JVM, so a test on a temporary root doesn't use up the default root's sweep.
  - `end` doesn't sweep, so the unit tests that run workflows without a profile never touch `target/sail-profiles`.
  - A skipped run counts as not passed: its folder is kept, and the message says "failed run". A skip on a device only happens after `StartDevice`, and the sweep removes the folder later.
  - A run folder in use is skipped silently: no warning, because nothing failed.
- Open:
  - **Mike:** keep the `@SuppressWarnings("resource")` fallout above, or another approach (for example `close()` without `AutoCloseable`, which decision 13 rules out)?
  - The single-renderer test bases (`AbstractPrimeFacesInterpretedSail`, `AbstractSmartClientInterpretedSail`) don't run through `SailWorkflow`, so a `StartDevice` in one of their tests leaves its folder to the sweep.
  - Not verified: Windows (the undeletable-folder path was exercised with POSIX permissions), Edge, a headed run.


### W6

- Wrote the docs from the draft in `~/skyve-sail-work/browser-control/w6-draft/`, checking each statement against the code at `106fef4005`, and resolved every `TODO(W3/W4/W5)`:
  - `docs/sail.md`: new "Browser control and standard calls" (capabilities, skipping, the 25 calls, test files, op-code rule, "Device profiles") before "The Suites"; a cross-link in "Use them sparingly"; a new "Upgrading" group at the end. `sail.md` has no table of contents of `##` sections, so none changed.
  - `docs/skyve-10-changes.md`: one SAIL bullet with three behaviour-change sub-bullets, before "Number converters, ...".
  - `docs/architecture-design.md`: not changed. Its one SAIL subsection names no test-harness classes beyond `SailLoad` in passing, so there is no list to add to.
- Checked against the code:
  - the call table against each call's Javadoc and `super(...)` parameter names, its defaults in `run`, and its variable and capability (25 classes in `util/sail/call`, none added or renamed since the draft; the draft table was right, and gained the `CheckRequests` count default, `RequestLog`'s `matching` default and `FreezeTab`'s index base);
  - the skip message in `SailCapabilityUnavailableException` and `StartDevice.requireProfiles`, the Verify wording in `InterpretedWebDriverExecutor` (lines 296 to 330), and `SailWorkflow`'s "SAIL workflow: <name> skipped: ...";
  - `SailTestFiles`' four methods, their reuse rules and `FOLDER`; `SailSettings` (`sail.profiles`, `sail.profiles.keep` = `failed`, `all`, `none`, `sail.profiles.keepHours` = 12); `SailProfiles` (the kept-run message, the sweep's checks, the warning line); `CrossBrowserSelenium` (flags for Chrome and Edge, the `--disable-features` merge); `SailWorkflow implements AutoCloseable`; `AbstractInterpretedSailWorkflow.closeWorkflowsAndReleaseServer()`;
  - the example XML is the request-rules case of `StandardCallsSailIT`, shortened, in the attribute style of `sail.md`'s examples.
- Not run: no tests (docs only). Anchors in the new text point at headings that exist.

### Review round 1

Sources: three reviews of `d5e7d995c4` (PR, rubric, slop) and the final gates, listed in `~/skyve-sail-work/browser-control/fix-list.md` (items 1 to 28). Mike decided four points (below). Fixed in one commit, "SAIL browser control: review and gate fixes (round 1)".

**Mike's decisions (2026-11-19), which amend the plan above:**
- **Log start** (decision 8): `CheckRequests` and `CheckDialog` fail when the control began watching in their own step, telling the author to start watching with an earlier call. The control's Javadoc no longer claims it connects before a step asks.
- **Plain `close()`** (decision 13, "Approved new constructs"): `SailWorkflow` is not `AutoCloseable`. The `@SuppressWarnings("resource")` and try-with-resources fallout is gone; `SailLoad` and `SailLoadTest` are back to their base form.
- **Pid in the run folder** (decision 13): `<root>/<run token>-<pid>/<profile>`.
- **Marker** (decision 13): each run folder gets `.sail-run`; the sweep deletes only marked folders.

**Fixed:**
- `ChromiumBrowserControl`:
  - Selenium 4.38's `Connection` does hand each event to a cached thread pool (`Connection.java` lines 66 and 236), so events can be handled out of order. Now a session whose detach comes before its attach isn't added; a setting sent to a session Chrome says has gone ("Session with given id not found") drops that session instead of failing the step; and the threading comment says so.
  - Held requests: `clearRules()` lets them go and clears the rules in one turn of the DevTools thread. A held request is dropped when its frame navigates (`Page.frameNavigated`) or its page detaches, but only if it was held before that event: each event's Selenium sequence number is kept, so pool reordering can't drop a newer one. CDP reports nothing else: after a reload, no `Network.loadingFailed` came for the paused request, and its interception id was invalid in every session (checked with a throwaway probe and the IT). The `release()` Javadoc is corrected.
  - `toOwner`: Chrome's errors were checked with a throwaway probe. A request that isn't the session's gives "Invalid InterceptionId." (or "Fetch domain is not enabled", or "Session with given id not found"); a bad status gives "Invalid http status code or phrase"; a bad header gives "Invalid header: X Bad". Any error other than the first three now fails the paused request (`Fetch.failRequest`) and prints why, rather than leaving it paused.
  - `watchingSince()` was added to `RequestLog` and `Dialogs` (the time the record began) for the log-start rule, plus constants for the repeated CDP strings, and the unneeded `@SuppressWarnings("resource")`.
- `SailBrowserControl`: `of` makes a control outside the lock, then `putIfAbsent`, releasing the loser. Its Javadoc says a quit driver's control is released at the next `of` call. A shared `POLLING` (100 ms) constant replaces the four copies.
- Calls:
  - `RespondToRequests` takes a `status` of 100 to 599, and `RewriteRequestHeaders` takes only header names that are RFC 7230 tokens.
  - Each `ShiftClock` script is numbered, so a repeated shift adds.
  - `SwitchTab` and `FreezeTab` read `action` as private enums through `choice`, so the message is now "must be one of [OPEN, SWITCH, CLOSE]". `SwitchTab` calls `require` once.
  - `CheckRequests.pattern` uses `required`. `SailCallParameters.list` no longer wraps its result. The repeated parameter names are constants.
- `CheckRequests` (PR11): a request a service worker forwards that fails in the network is counted twice. The page's copy gets only `loadingFailed`, with no `fromServiceWorker` mark, and CDP has no other marker. This is documented in the call's Javadoc and `sail.md`, and pinned in `ChromiumBrowserControlIT` (two unmarked failed entries).
- `SailProfiles`:
  - pid and marker;
  - a rename before deleting, to `<token>-<pid>.deleting-<pid>`, with the marker deleted last, and a renamed half-deleted folder still swept;
  - a run folder holding only its marker is deleted quietly even when the run failed;
  - `runFolder(settings, run)` is a query that registers nothing, and `RestartDevice` uses it.
- `SailWorkflow`: calling `shareRunAcrossCalls()` again ends the run shared so far, and `close()` clears `sharedRun`. `AbstractInterpretedSailWorkflow`'s `@AfterAll` closes each workflow in its own try, rethrows the first failure with the rest suppressed, and releases the lock in `finally`. Its queue now holds `Runnable` closers, package-private for its test.
- Tests:
  - `SailProfilesTest` disables the link, `touch -h` and `true` cases on Windows, and its permission tests assume the folder isn't writable (not root). The 26-assert sweep test is split in two.
  - `StandardCallsSailIT` checks only its own run's folder: the script reports `StartDevice`'s folder to the test server. It asserts on server hits in the three tests Sonar flagged, and checks the folder name ends in `-<pid>`.
  - Sonar's test-code issues are fixed: S5778 setup moved out of `assertThrows`, S1130, S6213 (`record` became `note` in the fake, and `record` became `seen` in `CheckDialog`), S7158 and S2147.
- Stricter scan: no raw tab in the `ShiftClock` script. `RequestRule.globMatches` uses a helper, `matchedWidth`, with no `continue`.
- Docs (`sail.md`, `skyve-10-changes.md`):
  - when watching begins;
  - the measured saving (151 MB to 12 MB, Chrome 158, W0), with Edge described as not measured and 8.5 MB as the fixture page's profile;
  - the three profile keys in the "Keys" table;
  - the new folder layout, the marker, the rename and plain `close()`.
- New and changed tests:
  - `RequestCallsTest.checkRequestsFailsWhenTheLogBeganInItsStep` and `BrowserCallsTest.checkDialogFailsWhenTheRecordBeganInItsStep`;
  - status and header checks;
  - two `ShiftClock`s give two scripts, and in `StandardCallsSailIT` two one-day shifts give two days;
  - `SailProfilesTest`: `runsOfTwoJvmsWithOneTokenHaveFoldersOfTheirOwn`, `theSweepLeavesAloneWhatIsNotARunFolder` (unmarked, token-only and half-deleted folders), `aRunFolderThatCannotBeRenamedIsLeftWhole`, `theRunFolderQueryMakesNothing` and `aRunWithoutAProfileEndsQuietly`;
  - `SailWorkflowTest.sharingARunAgainEndsTheRunSharedSoFar`;
  - `util.AbstractInterpretedSailWorkflowTest` (new);
  - `ChromiumBrowserControlIT.awaitingHeldRequestsWakesForOneHeldLater` (the page holds its fetch 500 ms after the wait begins, plus the timeout message) and `heldRequestsAreForgottenWhenThePageGivesUpOrTheRulesAreCleared`.

**Not done:**
- Item 27, a skip with a media recorder attached (`InterpretedWebDriverExecutor` lines about 430): `InterpretedWebDriverExecutorTest` doesn't mock `SailMedia`, which the fix list made the condition.
- Item 28, Javadoc leads: `javadoc-standards.md` asks for a verb lead on "the element", with only method examples. It says nothing different for types or constants, and the neighbouring harness types and `SailSettings` keys use noun leads. So, as the fix list asked, they are unchanged.

**Waived (from the fix list):**
- R2, test seams: package-private constructors and helpers are the repo's usual test access, not public API.
- R12, test class naming: the call tests are grouped by family, as the W3 brief allowed.
- S1, `answerDialog` unused by a call: decision 2's `Dialogs` row includes "answer", and the IT proves it.
- The kept-folder message says "failed run" for a skipped run: decision 13 fixes the text, and the docs say a skipped run's folder is kept.
- Stricter scan only:
  - S106 (`System.out`): the harness prints its run messages that way.
  - S1845 (`profiles()` and `PROFILES`): the `SailSettings` convention.
  - S1452 (wildcards in `Selenide<?, …>`): the existing types use them.
  - S6539 (21 dependencies): one class over one CDP connection, as decisions 2 and 3 lay out.

No finding was shown to be wrong. PR2 was confirmed in the Selenium source. PR3's reload case was confirmed by the IT: the old code kept the stale id.

**Re-gate (JDK 21, Chrome 158.0.8127.37, `-DforkCount=1 -Dsurefire.rerunFailingTestsCount=0`):**
- Unit: `-Dtest='util.sail.**.*Test,sail.HarnessSelfContainedTest,util.AbstractInterpretedSailWorkflowTest'` ran 331 tests, with 0 failures, errors or skips and no flakes. The extra class is the new `util` test, outside `util.sail.**`.
- ITs: `ChromiumBrowserControlIT` (20), `StandardCallsSailIT` (6) and `SailTestFilesIT` (1), in one JVM, headless, three runs in a row: 27/27 each. `skyve-war/target/sail-profiles` was empty after every run.
- Red/green, each restored byte for byte:
  - Log start (`requireWatchingBefore` never throwing): `checkRequestsFailsWhenTheLogBeganInItsStep` and `checkDialogFailsWhenTheRecordBeganInItsStep` failed.
  - Unmarked folder (the marker check removed): `theSweepLeavesAloneWhatIsNotARunFolder` failed.
  - Pid (the name without it): 7 of 10 in `SailProfilesTest` failed, including `runsOfTwoJvmsWithOneTokenHaveFoldersOfTheirOwn` ("each JVM's own folder ==> expected: not equal").
  - Rename (no rename): `aRunFolderThatCannotBeRenamedIsLeftWhole` ("nothing in it is deleted") and `aFolderThatCannotBeDeletedGetsAWarningNotAFailure` failed.
- ECJ 3.46 (`--release 17`, skyve-war prefs, core, ext, web and war compiled together): 0 warnings on changed lines, and 0 on unchanged lines of changed files.
- Sonar (`skyve-local-browser-control`, test-code rules): the first re-scan found 2 new issues on changed lines (S6213, a parameter named `record`, and S5778 in the new IT case), both fixed. The second found 0 issues on changed lines against base `8e9d436801`, and the gate's 2 issues on unchanged lines of changed files remain.
- After the Sonar fixes (a parameter rename and a hoisted `Duration`), the unit set plus `ChromiumBrowserControlIT` ran 351 tests, all passing, and ECJ again found 0 warnings on changed lines. `StandardCallsSailIT` and `SailTestFilesIT` weren't rerun, as those fixes don't touch them.
- Not verified: Windows (the rename-before-delete and the `@DisabledOnOs` guards), Edge, a headed run and Firefox in this round.

## User validation suggestions

- Run `StandardCallsSailIT` headed and watch the offline, hold and release, and dialog steps.
- On a Mac with Firefox, run it with the Firefox browser setting and check the Chromium-only steps report skipped with a readable reason.
- Run a SAIL test that starts a device, then check `target/sail-profiles` is empty after it passes. Make it fail and check the folder is kept and named in the output. Leave a profile open in a headed Chrome and confirm a later run's sweep doesn't touch it.
- After assembling into basix-cloud, check `du -sh target/sail-profiles` on a lane after a full inspection suite run. A failed run's folder should be about 20 MB per profile, not 35–70 MB.
- After assembling into basix-cloud, swap one basix call (for example `sail.inspection.Offline`) for `util.sail.call.Offline` in a suite file, and run that file on a lane.
