# SAIL: Opt-in Browser per Suite Member

## Goal

A `SailSuite` keeps handing its browser from member to member by default, as it always did. A suite can opt in to a new browser for each member.

## Context (verified)

- On master, a suite member that passes keeps its browser for the next member when the configuration matches; a member that fails stops it; the suite stops the last one when it ends (`SailWorkflow.kept`, `closeBrowser()`). `docs/sail.md` documents this under Suites, Independence.
- A downstream app (basix) had a later member's `<login>` fail with "The sign in fields did not take the typed values". The cause was Chrome's password leak prompt, a tab-modal prompt that the shared browser carried from one member to the next. `plan_sail_chrome_password_prompts.md` turns it off, so that case is fixed at its source.
- The first version of this branch (`sail-suite-login`, commits 23a3a73d3a, 10f9ac4270, 600e466624) made every member run in its own browser, and so changed documented behaviour for every suite. Mike decided the default stays.
- Suite files have attributes (`testStrategy`, `evidence`), but they are part of the SAIL language in skyve-core: the `Suite` class, two `sail.xsd` copies and their tests.
- The other code-level knobs of a suite are on `SailSuite`: `settings(...)`, `suiteOnly`.

## Decisions

1. **`SailSuite.browserPerMember()`**, a builder method, off by default. A suite loaded with an engine (`resource`, `file`) passes it to `SailWorkflow.runMember(name, workflow, member, ownBrowser)`; with it, the member starts in a browser of its own (leaving alone any kept one) and stops it when it ends, passed or failed. The three-argument `runMember` is unchanged.
   - Why a builder method: it is a property of how a test runs a suite, like `settings(...)`; it needs no change to the SAIL language, the XSDs or skyve-core's `Suite`; and the harness is test code that a downstream app copies, so the smallest surface wins.
   - Not done: a suite file attribute (e.g. `browserPerMember="true"`). It would let a BA opt in without Java (a `sail.run` suite). It needs a `Suite` property, both XSDs and tests in skyve-core, so it is left as the follow-up if a non-Java author needs it (Mike's decision: keep the builder method).
   - Not done: a `sail.browserPerMember` setting in `SailSettings`; it would apply to every suite in a JVM, which is a coarser choice than the one suite that needs it.
2. The test base gets `sailSuite(resourceName, boolean browserPerMember)`.
3. The default is exactly master: this branch's earlier changes to `SailSuite`, `SailWorkflow`, `SailLoad` and their tests were taken back to master's, then the opt-in added.
4. `docs/sail.md` says when to opt in, with Chrome's password prompt as an example of carried-over state, and that the password prompt change fixes that case.

## Tests

- Unit (`SailWorkflowTest`, no browser): an opted-in member stops its browser when it ends; an opted-in failed member stops its browser; an opted-in member leaves a kept browser alone; a real `SailSuite` over the login fixture hands its browser on by default (one start, one stop) and starts one per member with `browserPerMember()`.
- Browser (`SailSuiteLoginSailIT`): the two-member login suite, opted in; the second member finds nothing in local storage that the first left, and signs in first.

## AI validation results

- Red: with the suite's flag not passed to the engine, `aSuiteHandsItsBrowserOnUnlessItOptsIn` failed; with the engine ignoring `ownBrowser`, 7 of the tests failed. Green restored.
- `SailWorkflowTest` (15), `SailSuiteTest` (14) and `SailLoadTest` (41) pass.
- Build: `mvn -o -B -pl skyve-war -am package` with the `util/sail/*Test` tests and `sail.HarnessSelfContainedTest` (220 tests): passed.
- Eclipse (JDT 3.48) on the changed files: one warning on a changed line (a `@Nullable` member passed as `@Nonnull`), fixed.
- Sonar (local, project `skyve-local-suite`): one issue on changed lines, `java:S5778` in `SailWorkflowTest`, fixed. The `TODO` (`java:S1135`) in `AbstractInterpretedSailWorkflow` is older than this change.
- Spotless: `skyve-war/pom.xml` excludes `src/test/java/util/**`, so it does not apply; formatting by hand.
- JaCoCo over `SailWorkflowTest` and `SailSuiteTest`: every changed line of `SailSuite` and `SailWorkflow` is covered.
- Not run: `SailSuiteLoginSailIT` against a disposable server (the machine was loaded and a server was not started); its only change since the earlier run is the opt-in call, which the unit test covers. The earlier commits of this branch record no result for it either.

## Acceptance

- [x] The default behaviour is as on master (its tests are master's, unchanged).
- [x] The opt-in is tested, in units and in the browser IT.
- [x] `docs/sail.md` says when to opt in.

## Notes after the rebase

- The branch is rebased onto `sail-chrome-leak-detection`, so the docs' statement that SAIL's Chrome has the password prompts off holds.
- Edge gets the same preferences in that change; it has not been run (no Edge here).
