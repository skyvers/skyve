# Execute the SAIL browser control plan

Carry out `docs/ai/plan_sail_browser_control.md` in Skyve: W0 to W5 in order, then the gates once at the end.

You can do the work yourself, or hand a work item to one subagent at a time. Never run two implementers or two builds at once, because the items share files. If you use subagents, give each one this prompt's hard rules and its work item, and keep only their summaries in your context.

## Hard rules

1. **Work in the existing worktree, not the shared checkout.**
   - The worktree is `/Users/mike/skyve-sail-browser-control`, on branch `sail-browser-control`, which already holds this plan and prompt.
   - Never edit, build in, or run git writes against `/Users/mike/_/skyve`. Other sessions use it, and VS Code compiles into its `target/` directories.
   - Before W0, if `master` has moved past the branch's base (`git -C /Users/mike/skyve-sail-browser-control merge-base --is-ancestor master HEAD` fails), merge `master` into the branch.
2. **Git.** Commit on `sail-browser-control` once per work item, with a message naming it. Never push, rebase, reset, stash, or touch other branches or worktrees. End commit messages with the co-author line your session's instructions give.
3. **Maven.** Builds run in the worktree, always offline (`-o`), and never `install` or `deploy`: downstream projects (basix-cloud) build against the Skyve jars in `~/.m2`. Use `-pl skyve-war -am` and `-Dtest=… -Dsurefire.failIfNoSpecifiedTests=false`. For the test environment, see "Environment" in `docs/ai/plan_m1_prompt.md` in the main checkout, which you may read but not change. JDK 21 is at `/Users/mike/jdk-21.0.2.jdk/Contents/Home`.
4. **Repo rules.** Read `AGENTS.md` and the documents it lists, and `docs/sail.md`, especially "Running the Tests", "A disposable server", "Synchronisation Rules", "The run token and variables" and "Upgrading". Change Restraint and Test Restraint apply. The plan's "Approved new constructs" is the complete list. Anything else needs Mike's approval: ask, with the reason and the smallest alternative.
5. **The locked decisions in the plan are decided.** Don't re-open them unless W0 or the code proves one impossible. If that happens, stop and tell Mike what failed and what you propose.
6. **Porting from basix-cloud.** The source to port is in `/Users/mike/_/basix-cloud` (v6), listed in the plan's "Current state observed". Read it, and never edit, build in, or run git writes against that repo. Keep the hard-won behaviour and the comments explaining why, such as how workers are attached, how a paused request finds its session, and the quota versus `estimate()` note. Rename and restructure to fit the plan's interfaces.
7. **Browsers and servers.** The ITs use a JDK `HttpServer` you start in the test. They need no WildFly. If you do need a disposable Skyve server, follow `docs/sail.md`. Use a port that isn't 8080, 8380, 8480 or 8580 (basix's servers), run one browser at a time, and stop anything you start. Never run Docker lifecycle commands, except `docker start skyve-sonar` for the Sonar gate. If it was stopped when you began, stop it again at the end, unless another session is using it (check `docker ps` and recent activity).
8. **Spotless.** `util/**` is excluded in skyve-war, so format by hand. Never run a bare `spotless:apply`.
9. **No `@TestFactory` for gating tests.** Use one `@Test` with `assertAll(Stream<Executable>)`, and put the case name in each message.
10. **Report only what you verified:** what ran, its result, and what was skipped.

## Work items

Record the base commit first: `git -C /Users/mike/skyve-sail-browser-control rev-parse HEAD`.

For each item: implement it, run its tests (from the plan's "AI validation plan"), and commit once they pass.
- **W0** is a throwaway spike. Delete its code before W1, and commit its results in the plan's "AI validation results" with W1. If W0(b) shows a `TestAbortedException` from a call is turned into a failure, include the smallest `SailWorkflow`/`SailSuite` change to keep it as skipped in W1, with a test.
- **W1** must leave `skyve-war` compiling (`test-compile`), with every existing `util.sail` test and `HarnessSelfContainedTest` passing.
- **W2:** `ChromiumBrowserControlIT` passes three runs in a row headless, and once headed (`-Dsail.headless=false`).
- **W3:** `StandardCallsSailIT` passes three runs in a row on Chrome. If Firefox is installed, also run it with the Firefox browser setting (see `CrossBrowserSelenium` and `SailSettings`). Chromium-only calls must report skipped with the plan's message, and the rest must pass. If Firefox isn't installed, record that.
- **W4:** `SailTestFiles` tests, including a video that plays.
- **W5** is documentation. Check it against what was built.

## Gates (once, after W5)

Run them on the lines changed since the base commit (`git diff -U0 <base>`), counting new files as wholly changed. The recipes are in "Gates" and "Environment" of `docs/ai/plan_m1_prompt.md`; run them against this worktree.

1. **Build and tests.** `mvn -o -B -pl skyve-war -am package`. Read the results from `*/target/surefire-reports/TEST-*.xml`. Every test passes, with no flakes. Then the ITs, as in W2 and W3.
2. **Red/green.** For each behaviour in the plan's "AI validation plan" red/green list, break the code temporarily, show a test fails, then restore it.
3. **Coverage.** JaCoCo over the new code. List each uncovered changed line with its reason. There's no fixed percentage, because this is test-harness code, but untested public behaviour must be justified.
4. **Eclipse.** Zero JDT 3.48 warnings on changed lines, using the IDE's batch compiler with skyve-war's `.settings/org.eclipse.jdt.core.prefs` and `skyve-core/target/classes` first on the classpath. See `docs/ai/plan_m1_prompt.md` for the recipe.
5. **Sonar.** Zero issues on changed lines.
   - Start it with `docker start skyve-sonar`, then wait for `/api/system/status` to report `UP`.
   - Source `~/.config/skyve-sonar/env` without printing it.
   - Use project key `skyve-local-browser-control`.
   - If Sonar won't start, record that.
6. **Javadoc.** Every public type and method meets `docs/ai/javadoc-standards.md`. Each call lists its parameters and its variable.
7. **Review.** Run three read-only reviewer subagents in parallel over `git diff <base>...HEAD`, with the plan as the governing document:
   - **PR review:** `/Users/mike/_/review-miner/skills/pr-review/SKILL.md`;
   - **Rubric:** `/Users/mike/_/review-miner/skills/rubric-review/SKILL.md`, with the rubric at `/Users/mike/_/review-miner/rubric-draft.md`. The skill's `~/workspace/review-miner` path is `/Users/mike/_/review-miner` on this machine.
   - **Code slop:** `/Users/mike/_/review-miner/skills/code-slop-allergist/SKILL.md`, with the rules at `/Users/mike/_/review-miner/ai-slop-rules.md`.

   Findings come back as `file:line | rule | problem | failure scenario | fix`. Fix each one, or argue against it with evidence. Re-run the gates the fixes affect, then re-review once.

Commit the gate fixes. Fill in the plan's "AI validation results" and acceptance checklist, and commit that. Add to `docs/ai/learnings.md` only what meets that file's bar.

## Report to Mike

- the commits;
- the files changed, by work item;
- each gate's result, with any suppressions and uncovered lines;
- the review findings waived, and why;
- what wasn't verified (Firefox if not installed, headed runs if not done, anything a disposable server would have needed);
- the downstream impact: applications that copy the `util/sail` harness get the new packages on their next assembly, and basix-cloud then migrates off its own copies (the plan's "Better engineering notes").
