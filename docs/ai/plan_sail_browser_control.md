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
3. **Entry point.** `SailBrowserControl.of(WebDriver)` returns one control per driver (cached, released when the driver quits), chosen by browser: Chromium (Chrome, Edge) gets `ChromiumBrowserControl`, anything else `WebDriverBrowserControl`. `supports(Capability)` answers. `require(Capability)` returns the interface, or throws decision 4's exception.
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
9. **Test files** live in `SailTestFiles` (`util.sail`), not `SailMedia`. Each file is reused if it's already there with the right size. The playable video needs a browser (the leg's driver); the rest don't.
10. **No op-codes, and no `sail.xsd` change.** `docs/sail.md` records the promotion rule: a capability can become an op-code once it has Chromium and Firefox implementations. Scripts using op-codes then declare capabilities so unsupported browsers skip.
11. **`SailMedia` is unchanged.**
12. **Devices.**
    - `StartDevice` starts the leg's browser on a profile folder under a profile root (a new `SailSettings` key, `sail.profiles`, defaulting to `target/sail-profiles`), named by run and profile. It also accepts `offline` (`NETWORK_CONDITIONS`), `touch` (`TOUCH`) and `remembered` (keep or clear the profile). It returns the folder.
    - `RestartDevice` restarts the browser on the same profile.
    - Both use `BrowserConfiguration.profile` and `InterpretedWebDriverExecutor.getSelenide()`.
    - Read basix's `DeviceStart`, `RestartBrowser` and `RestartDevice`, and generalise away the basix-only property `bhm.sail.profiles`.
    - Both get their folder from `SailProfiles` (decision 13), not by building the path themselves.
13. **Profile housekeeping.** `SailProfiles` owns the root (`sail.profiles`). Every profile folder is `<root>/<run token>/<profile>`.
    - **When a run ends, after its browsers have quit:** delete `<root>/<run>` if the run passed. If it failed, keep it and print `SAIL device profiles kept for the failed run <run>: <folder>`.
      - A run of its own ends when its `runResources`, `runFiles` or `runStrings` call returns, or when its suite member does.
      - A run shared with `shareRunAcrossCalls()` ends when the workflow is closed. `SailWorkflow` implements `AutoCloseable`. `close()` stops any kept browser, then ends the shared run's profiles. The run failed if any of its calls threw.
      - `AbstractInterpretedSailWorkflow`'s `@AfterAll` closes its workflow. A test that builds its own shared workflow closes it itself, e.g. with try-with-resources.
    - **`sail.profiles.keep`**: `failed` (the default) keeps a failed run's folder, `all` keeps every run's, `none` deletes every run's.
    - **Sweep**, once per JVM, before the first profile folder is given out. It deletes each run folder under the root that is:
      - named like a run token (base-36 letters and digits only; anything else under the root is left alone and never followed through a link);
      - older than `sail.profiles.keepHours` (default 12) by both its token's start time and the newest last-modified time of any file inside it;
      - not in use: no profile in it has a Chrome `SingletonLock` naming a live process. On macOS and Linux the lock is a symbolic link to `host-pid`; check the pid with `ProcessHandle.of(pid)`. On Windows, a profile in use can't be deleted: a folder that fails to delete is skipped.
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
- `SailProfiles`, `SailWorkflow implements AutoCloseable` with `close()`, the `@AfterAll` close in `AbstractInterpretedSailWorkflow`, and the extra Chrome and Edge arguments (decisions 13 and 14);
- the smallest `SailWorkflow`/`SailSuite` change for decision 4, if W0 shows it's needed;
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
  - `SailWorkflow`: end each run's profiles; `AutoCloseable`; `AbstractInterpretedSailWorkflow` closes it in `@AfterAll`;
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
- [ ] The docs describe the capabilities, the calls, skipping and the op-code rule.
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

(filled in during execution)

## User validation suggestions

- Run `StandardCallsSailIT` headed and watch the offline, hold and release, and dialog steps.
- On a Mac with Firefox, run it with the Firefox browser setting and check the Chromium-only steps report skipped with a readable reason.
- Run a SAIL test that starts a device, then check `target/sail-profiles` is empty after it passes. Make it fail and check the folder is kept and named in the output. Leave a profile open in a headed Chrome and confirm a later run's sweep doesn't touch it.
- After assembling into basix-cloud, check `du -sh target/sail-profiles` on a lane after a full inspection suite run. A failed run's folder should be about 20 MB per profile, not 35–70 MB.
- After assembling into basix-cloud, swap one basix call (for example `sail.inspection.Offline`) for `util.sail.call.Offline` in a suite file, and run that file on a lane.
