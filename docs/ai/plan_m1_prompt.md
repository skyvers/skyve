# Execute M1 with subagents

You are the orchestrator for milestone M1 in the Skyve repository at `/Users/mike/_/skyve`. The plan is `docs/ai/plan_m1.md`.

Carry out every work item in the plan (W1 to W9). Each item is done by subagents and must pass gates before you move to the next. The gates cover:

- Mike's review rules (review-miner);
- Eclipse and SonarQube warnings on the lines this milestone changed;
- Javadoc;
- test coverage.

Leave every change uncommitted in the working tree.

You coordinate; subagents do the work. Keep your own context small: hold the state files and each subagent's summary, not file contents or build logs.

## Hard rules

1. **Local only.** Edit files in `/Users/mike/_/skyve` on the current branch (`master`).
   - Do not create branches or worktrees.
   - Do not run `git commit`, `stash`, `checkout`, `switch`, `reset`, `restore`, `clean`, `rebase` or `push`.
   - Read-only git commands are fine.
   - This overrides any skill or plan text that says otherwise.
2. **Other sessions edit this checkout at the same time.**
   - Touch only the files the plan needs.
   - Never revert, reformat or "fix" a change you did not make.
   - If a file in the manifest changes in a way no subagent reported, stop and ask Mike.
3. **Never run Maven in the checkout.** VS Code's compiler rewrites `target/classes` there and breaks javac.
   - Build and test only in the rsync'd mirror (see [Environment](#environment)).
   - Edit only in the checkout.
4. **Follow the repo's own rules.** Read `AGENTS.md` and the documents it lists: `docs/ai/project-agent-guide.md`, `learnings.md`, `plan.md`, `javadoc-standards.md`, `test-patterns.md`, `testing-and-coverage.md` and `performance-and-coding-conventions.md`.
   - The Change Restraint and Test Restraint sections apply. The plan has approved exactly two new constructs:
     - `ActionAvailability` and `ActionUnavailableException`;
     - the prepared-download field on `AbstractWebContext`.
   - Anything else new needs Mike's approval first.
5. **Never use `@TestFactory` for tests that must gate the build.** Surefire reruns failures and reports a consistently failing dynamic test as a flake, so the build still passes. Use one `@Test` with `assertAll(Stream<Executable>)`, with the case name in each message.
6. **Don't claim what you didn't verify.** Report what ran, its result, and what was skipped.

## Before W1 (you do this yourself)

1. Read `docs/ai/plan_m1.md` in full, and the repo documents in hard rule 4.
2. The plan lists decisions 6–9 under "confirm at sign-off". Unless the plan records Mike's approval, ask Mike once to confirm them, and record the answer in the plan. Don't start W1 without it.
3. Create the state directory `$S = <your scratchpad>/m1` with:
   - **`baseline/`**: a snapshot of the checkout now, made with `rsync -a` using the mirror excludes below. Changed lines are measured against this snapshot, not against `HEAD`, so uncommitted work that predates the milestone (for example the `FacesAction` fix) is not counted.
   - **`manifest.txt`**: one repo-relative path per line, for every file the milestone creates or modifies. Append each subagent's reported files. Mark generated files (`src/generated*`) with a trailing ` generated`.
   - **`changed_lines.py`**: reads the manifest and prints JSON `{path: [[start, end], ...]}` of changed line ranges on the new side.
     - A file missing from `baseline/` counts as wholly new.
     - Otherwise parse `diff -U0 baseline/<path> <path>` hunks (`@@ -a,b +c,d @@`, where `d=0` means a pure deletion and adds no range).
     - Skip files marked generated.
   - **`ledger.md`**: one section per work item, with the gate results, review findings and how each was resolved, waived findings with reasons, and files touched.
4. Start SonarQube: `docker start skyve-sonar`, then poll `http://127.0.0.1:9000/api/system/status` until it reports `UP` (about 90 seconds). If it won't start, record that in the ledger and carry on, using VS Code's SonarLint diagnostics only.

## Per work item: the loop

Run the work items in plan order, one at a time. They share files and the single mirror, so never run two implementers or two builds at once. The review passes in step 3 are read-only, so run those in parallel.

For each item `Wn`:

### 1. Implement

Launch a `general-purpose` subagent in the foreground. Give it:

- the [Subagent briefing](#subagent-briefing);
- the instruction "Implement `Wn` of `docs/ai/plan_m1.md`", plus any decisions or findings from earlier items that it needs.

It must return:

- the files it created or modified;
- the commands it ran and their results;
- coverage, Eclipse and Sonar results for its changed lines (see [Gates](#gates));
- anything it could not do.

Add its files to the manifest.

### 2. Gate check

Read the results yourself. If any gate failed, send the implementer back to fix it before review, with `SendMessage` so it keeps its context.

### 3. Review

Launch three read-only `general-purpose` subagents in parallel. Each gets the briefing, the item name, the manifest paths for this item, and the output of `changed_lines.py`. Tell each to replace its skill's "Establish the diff under review" step with: review exactly these files, focusing on these line ranges, read against `$S/baseline/`. A merge-base diff on this shared `master` would include other sessions' work.

- **Rubric review.** Follow `/Users/mike/_/review-miner/skills/rubric-review/SKILL.md`, with the rubric at `/Users/mike/_/review-miner/rubric-draft.md`. The skill names `~/workspace/review-miner`, which does not exist on this machine.
  - The requirements context is `Wn` in `docs/ai/plan_m1.md`.
  - Run both directed passes, simplification and security.
- **Code slop.** Follow `/Users/mike/_/review-miner/skills/code-slop-allergist/SKILL.md`, with the rules at `/Users/mike/_/review-miner/ai-slop-rules.md`, overlaid with this repo's `AGENTS.md` and conventions.
- **Plan, Javadoc and tests.**
  - Does the change do exactly what `Wn` specifies, no more and no less?
  - Does the Javadoc meet `docs/ai/javadoc-standards.md` for every new or changed class, constructor, method and field? That includes side effects, threading, nullness, `@param`, `@return` and `@throws`.
  - Do the tests assert behaviour, rather than just executing lines?
  - Do they follow `docs/ai/test-patterns.md` naming and placement?

Each reviewer returns findings as `file:line | rule | problem | failure scenario | fix`, most severe first, or "no findings". They must not edit anything.

### 4. Fix

Send all findings to the implementer with `SendMessage`. It fixes each one, or argues against it with evidence, and re-runs the gates on the lines it changed.

Then re-run only the reviewers that had findings. Stop when no blocking finding remains, or after three rounds. Record any finding left open, with its reason, in the ledger and in the plan's "Better engineering notes".

### 5. Record

Update the ledger, and tick `Wn` in the plan's acceptance checklist only when every part of it is true.

### Item-specific notes

- **W4** adds a new document, which needs domain generation: see [Environment](#environment). Copy only the new generated files back to the checkout and add them to the manifest as generated.
- **W5** starts with the client-refresh spike in the plan. If the spike fails, use the plan's fallback (a warning message only) and record why.
- **W8**: do the automated parts:
  - the renderer comparison;
  - the PrimeFaces builder audit;
  - the Flutter reading;
  - `node --check` on the fixture's SmartClient script.

  Then update the W2 rules if a finding requires it. That re-opens W2's gates and review, and W5–W7's tests. Leave the browser checks to Mike: they are the plan's "User validation suggestions".
- **W9** is documentation. Review it as hard as code. Its gate is accuracy against what was built.

## Gates

A work item passes only when all of these hold for its changed lines (from `changed_lines.py`):

- **Build and tests.** Every test the item adds passes, and so do the related existing tests the plan lists. There must be no "Flakes" line, and no `@TestFactory` used for gating tests.
- **Red/green.** For each new behaviour, temporarily break the production code in the mirror and show the test fails. Then restore it and `touch` the file, because rsync keeps modification times and Maven would otherwise skip recompiling it.
- **Coverage.** For the item's changed production lines (Java under `src/main/java`; tests, generated sources and `skyve-war` fixture code are exempt):
  - at least **95% of lines** and **90% of branches** are covered;
  - every new class is at least 90% line-covered;
  - each uncovered changed line is listed with a reason. "Needs a browser" is acceptable only for client JavaScript.

  Measure it from JaCoCo XML: `<sourcefile>` → `<line nr mi ci mb cb>`, intersected with the changed lines. See [Environment](#environment) for the command.
- **Eclipse warnings.** Zero JDT errors or warnings on changed lines, using the batch ECJ recipe ([Environment](#environment)). Also check the VS Code diagnostics (`mcp__claude-vscode__getDiagnostics`) for each touched file, where the tool is available.
- **SonarQube warnings.** Zero issues on changed lines, from both the local SonarQube scan and VS Code's `sonarqube` diagnostics.
  - Fix the root cause.
  - Suppress only a confirmed false positive, at the narrowest scope (declaration first), with `@SuppressWarnings("java:Sxxxx")` and a comment explaining why. That follows the project-agent-guide rules. List every suppression in the ledger.
- **Pre-existing warnings.** Leave warnings on unchanged lines alone. Note any on a changed line's own statement that you can't fix without widening the change.
- **Javadoc.** Present, and conforming to `docs/ai/javadoc-standards.md`, on everything new or changed in `skyve-core`, and on everything new or changed elsewhere that other modules call.
- **Formatting.** Tab indentation, matching the surrounding file. For `skyve-war` sources, run Spotless from the module in the mirror (`mvn -o -Pspotless-with-download -DskipTests spotless:apply`). Carry back only the files in the manifest.

## Final milestone gate (after W9)

1. Re-sync the mirror, then run `mvn -o -B package` in it. It runs about 21,000 tests; read the results from `*/target/surefire-reports/TEST-*.xml`.
2. Run the coverage gate on **all** milestone changed lines, using an aggregate JaCoCo run over the milestone's tests.
3. Run the Eclipse and SonarQube gates on all milestone changed lines.
4. Run a whole-milestone review in parallel: rubric review (as above), plus a PR review following `/Users/mike/_/review-miner/skills/pr-review/SKILL.md`, with the plan as the governing document. Use the same diff override, with all manifest files. Fix, then re-review, as in the per-item loop.
5. Fill in the plan's "AI validation results" and "Acceptance checklist" with what actually ran and the results.
6. Add durable learnings to `docs/ai/learnings.md` only if they meet that file's own bar.
7. Stop. Don't commit.

Report to Mike:

- the files changed, grouped by work item;
- the gate results and any suppressions;
- review findings that were waived, with their reasons;
- what was not verified (at least the browser checks);
- the backward-compatibility notes from the plan.

## Subagent briefing

Paste this block into every subagent prompt, followed by that subagent's task.

```text
You are working on Skyve milestone M1 (docs/ai/plan_m1.md) in /Users/mike/_/skyve, a shared
checkout that other agent sessions edit at the same time.

Rules:
- Edit only in /Users/mike/_/skyve, and only files your task needs. Never revert or reformat
  changes you did not make. No git writes of any kind (no commit, stash, checkout, reset,
  restore, clean, branch, worktree). Read-only git is fine.
- Never run Maven in the checkout. Build and test only in the mirror at <MIRROR>.
- Read AGENTS.md and the docs it lists before writing code; follow Change Restraint, Test
  Restraint, javadoc-standards.md and test-patterns.md. Match the surrounding code's style:
  tabs, comment density, naming.
- No @TestFactory for gating tests: use one @Test with assertAll and the case name in each message.
- Report honestly: the commands you ran, their results, and anything skipped or unverified.
- Return: files created or modified (repo-relative), gate results for your changed lines
  (tests, red/green, coverage %, Eclipse, Sonar, Javadoc), and open issues.

State: baseline snapshot <S>/baseline, manifest <S>/manifest.txt, changed-line ranges via
`python3 <S>/changed_lines.py`. Environment recipes: the "Environment" section of
docs/ai/plan_m1_prompt.md.
```

## Environment

These recipes have been verified in this repo.

**Mirror.** Sync with:

```
rsync -a --delete --exclude target --exclude .git --exclude node_modules --exclude deployments --exclude .settings --exclude .classpath --exclude .project --exclude skyve_flutter --exclude skyve-vue /Users/mike/_/skyve/ <MIRROR>/
```

- Re-sync before every build.
- `--delete` and rsync both overwrite generated domain classes in the mirror, so regenerate after each sync until those classes have been copied back to the checkout.
- Always pass `-o` (offline) to Maven.
- A downstream module needs `-am`. Without it, Maven compiles against stale jars in `~/.m2`, and tests fail with `NoClassDefFoundError` for classes added upstream.
- Add `-Dtest=<pattern> -Dsurefire.failIfNoSpecifiedTests=false` so upstream suites don't run.
- `-DskipTests` does not skip `skyve-war` tests.

**Test commands.**

- One core class:

  ```
  mvn -o -q -pl skyve-core test -Dtest=<Class>
  ```

- One skyve-web class:

  ```
  mvn -o -q -pl skyve-web -am test -Dtest=<Class> -Dsurefire.failIfNoSpecifiedTests=false
  ```

- One skyve-war class (about 15 seconds):

  ```
  mvn -o -pl skyve-war -am test -Dtest=<Class> -DskipUnitTests=false -DskipIntegrationTests=true -Dsurefire.failIfNoSpecifiedTests=false
  ```

**Test environment limits.**

- `skyve-web` tests use Mockito's subclass mock maker, so there is no `mockStatic`.
- Constructing Skyve's `SecurityException` against mocked persistence throws `IllegalArgumentException`. Mock the exception instead, as `FacesActionTest` and `ContentServletTest` do.
- `skyve-web` has no EL implementation on its test classpath. Use `{role:...}` conditions there, or put `{el:...}` tests in `skyve-war`.

**Coverage.** Run the aggregate profile, restricted to the tests that matter:

```
mvn -o -Pcoverage -pl skyve-coverage -am verify -DskipUnitTests=false -DskipIntegrationTests=true -Dtest='<pattern>' -Dsurefire.failIfNoSpecifiedTests=false
```

Then read `skyve-coverage/target/site/jacoco-aggregate/jacoco.xml`. Code in `skyve-core` and `skyve-web` that only `skyve-war` H2 tests exercise is credited there. If the aggregate build fails, record why and fall back to the per-module command, `mvn -o -Pcoverage -pl <module> -am verify ...` (report: `<module>/target/site/jacoco/jacoco.xml`).

**Eclipse warnings (batch ECJ).** Compile all source roots together, with nothing written out:

```
java -Xmx8g -jar ~/.m2/repository/org/eclipse/jdt/ecj/3.41.0/ecj-3.41.0.jar -21 -proc:none -d none \
  -properties /Users/mike/_/skyve/skyve-core/.settings/org.eclipse.jdt.core.prefs -cp "<union classpath>" <source roots>
```

- **Union classpath:** run `mvn -o -q -pl <m> dependency:build-classpath -Dmdep.outputFile=<file>` in the mirror for `skyve-core`, `skyve-ext`, `skyve-web` and `skyve-war`. Concatenate the results and remove every jar under `/org/skyve/`.
- **Source roots:** `src/main/java`, `src/generated/java`, `src/test/java` and `src/generatedTest/java` of core, ext, web and war, in the mirror.
- It takes about 10 seconds. Ignore the duplicate `package-info` errors, which come from compiling everything together.
- Map each `WARNING/ERROR in <file> (at line N)` to repo paths, and keep those on changed lines.

**SonarQube.**

- Source `~/.config/skyve-sonar/env` and never print it.
- Scan in the mirror:

  ```
  mvn -o -B test-compile org.sonarsource.scanner.maven:sonar-maven-plugin:sonar -Dsonar.host.url=http://127.0.0.1:9000 -Dsonar.projectKey=skyve-local -Dsonar.scm.disabled=true
  ```

  It takes about 3 minutes. The explicit host URL matters, because the pom defaults to sonarcloud.io.
- Read issues from `GET /api/issues/search?componentKeys=skyve-local&issueStatuses=OPEN,CONFIRMED&ps=500` (paginate), with the header `Authorization: Bearer $SONAR_TOKEN`.
- Map each issue's `component` to a repo path, and keep the issues on changed lines.
- The local server uses the "Sonar way" profile, not SonarCloud's, so VS Code's `sonarqube` diagnostics are a second source.

**Domain generation (W4).**

- In the mirror, from `skyve-war/`, run `DomainGenerator` directly: `mvn skyve:generateDomain` uses stale jars from `~/.m2`.
- Build the classpath once:

  ```
  mvn -o -q dependency:build-classpath -Dmdep.outputFile=../cp.txt -Dmdep.includeScope=test
  ```

- Before each run, copy the metadata into `target/classes`, because the generator reads metadata from there:

  ```
  rsync -a --exclude '*.java' src/main/java/ target/classes/
  ```

- Generate:

  ```
  java -cp "target/test-classes:target/classes:../skyve-core/target/classes:../skyve-ext/target/classes:../skyve-web/target/classes:$(cat ../cp.txt)" \
    org.skyve.impl.generate.DomainGenerator true src/main/java/ src/generated/java/ src/test/java/ src/generatedTest/java/ H2_NO_INDEXES false false test,whosin,whosinintegrate
  ```

- **A brand new document:**
  1. Generate once with the new document's `views/` moved away, and with any association to it removed.
  2. Compile `skyve-war`.
  3. Restore the metadata.
  4. Generate again, with validation.
- Add the document's action privileges to the module's roles only after the action classes exist.

**Checking the fixture's SmartClient script.**

- From a throwaway H2 test, write `SmartClientGeneratorServlet.newRenderer(user, module, document, view, uxui, true).getCode()` to a file, wrapped as `function create(){var view={};` … `return view;}`.
- Run `node --check` on the file. The renderer tests don't catch invalid JavaScript.

**i18n.** `skyve-war/src/main/java/resources/i18n_de.properties` is ISO-8859-1. Search it with `grep -a` and edit it as bytes.
