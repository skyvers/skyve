# Plan: SAIL script and call steps

## Summary

Split SAIL's single `<execute>` step into steps that each do one job. A mostly
plain SAIL script can then test pages SAIL has no gestures for (custom
JavaScript widgets, client-side storage, asynchronous browser APIs) and hand off
to handwritten Java for what a script cannot do in the page, such as browser
DevTools, extra windows or test-JVM fixtures:

| Step | Job |
|---|---|
| `<execute variable="…">` | Synchronous JavaScript, as now; optionally keeps the result in a variable |
| `<executeAsync timeoutMillis="…" variable="…">` | An `async` function body: `await` works; the step waits for it to settle |
| `<waitFor timeoutMillis="…">` | Polls a script until it returns a truthy value. A wait, not a test |
| `<testScript message="…" timeoutMillis="…">` | An assertion written in JavaScript, following the test strategy |
| `<call className="…" variable="…">` + `<parameter name value/>` | Runs a handwritten Java step (`SailCall`) in the test JVM |

Affected modules: `skyve-core` (language, `Executor`, `SailCall`), `skyve-ext`
(`WebDriverExecutor` comments), `skyve-war` (schema, interpreted harness,
kitchen sinks, tests), and `docs/sail.md`. No recorder change. Validation is
unit and XML round-trip tests in core, harness unit tests in war, and both
kitchen sinks against a disposable server.

## Scope

- **W0 Spike.** Prove the two runtime assumptions before writing the language.
- **W1 Language.** Step classes, `Procedure`/`Interaction` refs, both `sail.xsd`
  copies, `Executor` methods, `SailCall`, generator comments, harness stubs.
- **W2 Harness.** Interpreted behaviour for all five steps, shared in
  `InterpretedWebDriverExecutor`, with PF and SC settling.
- **W3 Conformance.** Kitchen sink usage and inline failure tests in both renderers.
- **W4 Documentation.** `docs/sail.md`.

### Locked decisions

1. **Separate elements, not attribute modes on `<execute>`.** Each step has its
   own failure rule and strategy behaviour, and the XSD can then say which
   attributes each one takes.
2. **Script bodies are never token-substituted.** `docs/sail.md` promises this
   (`{` is normal JavaScript, and `{run}` is valid destructuring). Instead every
   script step passes one argument, the read-only object
   `sail = {run: "<token>", vars: {name: "value", …}}`. The executor runs each
   body as
   `return (function (sail) { BODY }).apply(this, arguments);`, or with
   `async function` for `executeAsync`, `waitFor` and `testScript`. So `sail`
   and `arguments[0]` both work, and `return` gives the result.
   Existing `<execute>` scripts get the same wrapper; W2 must show the
   kitchen sink `execute` scripts still pass.
3. **Async by returned promise.** W3C Execute Script awaits a returned promise,
   bounded by the script timeout, so `executeAsync`, `waitFor` and `testScript`
   use `executeScript` with the async wrapper, not `executeAsyncScript`. W0
   proves this on Chrome and Firefox. Fallback if it fails: `executeAsyncScript`
   with the callback `arguments[arguments.length - 1]` wrapping the body's
   promise as `{ok, value | error}`.
4. **`call` names a class, as actions and Bizlets do.** The class implements
   `org.skyve.metadata.sail.execution.SailCall` and has a public no-argument
   constructor. A new instance is made for each step from the thread context
   class loader. There is no registry.
5. **`SailCall` takes the executor, not a new context type:**
   `@Nullable String call(@Nonnull Map<String, String> parameters, @Nonnull Executor executor) throws Exception`.
   A handler that needs the browser casts to the harness class. If the harness
   has no public accessor for the `WebDriver`, add one (`getDriver()`), and
   nothing else.
6. **Values that are substituted:** `call`'s parameter `value`s and `testScript@message`.
   Not substituted: `className`, parameter `name`, `variable`, numbers, script bodies.
7. **`variable`** follows `CaptureStep`'s naming rule
   (`CaptureStep.VARIABLE_NAME_PATTERN`) and `ContextualExecutor.putVariable`.
   The value is `String.valueOf(result)` for a JavaScript result, and the
   returned string for a call. A `null`/`undefined` result with `variable` set
   fails the step, as a blank capture does.
8. **`Executor` gets abstract methods**, following this repository's
   convention: `executeExecuteAsync`, `executeWaitFor`, `executeTestScript`,
   `executeCall`. List them under Upgrading in `docs/sail.md`.
9. **Generators** (`WebDriverExecutor`, so `PrimeFacesInlineWebDriverExecutor`
   too) emit the existing "not supported by this generator" comment for the four
   new steps. `<execute>` still becomes `execute("…")`; with `variable` it also
   emits the comment for the `variable`.
10. **Contexts.** None of these steps changes the contexts. A script or call that
    navigates must be followed by a navigate step, as `navigateLink` to a
    non-Skyve page is today. Document it.
11. **Out of scope:** the recorder; a `device` or browser-profile attribute on
    `<automation>`; any SmartClient- or PrimeFaces-specific script helpers;
    a call registry; generated code for `call`.

### Step semantics

Defaults: `timeoutMillis` defaults to `sail.timeoutSeconds`. `waitFor` and
`testScript` poll every 250 ms (fixed, with no attribute). The JavaScript is
the element's text, usually in CDATA. An empty body fails with a `MetaDataException`.

| Step | Passes when | Fails when | Under `None` | Under `Verify` |
|---|---|---|---|---|
| `execute` | the script returns or throws nothing | it throws; or `variable` is set and the result is null/undefined | runs | failure recorded as now |
| `executeAsync` | the promise resolves | it rejects (message is the error's `message`, and the stack when there is one), or it is not settled within `timeoutMillis` | runs | recorded |
| `waitFor` | a poll returns truthy | `timeoutMillis` passes. The message gives the timeout, the script's first line and the last error a poll threw. A poll that throws counts as false | **runs** (like a timed `testGridRows`) | recorded |
| `testScript` | a poll returns truthy | still falsy at `timeoutMillis` (or at the first poll if `timeoutMillis` is absent). Message is `message` or the script's first line | skipped | recorded |
| `call` | `call` returns | the class is missing, not a `SailCall`, or not constructible (`MetaDataException` naming the class); the call throws; or `variable` is set and it returned null | runs | an `AssertionError` from the call is recorded and the run goes on; any other exception halts, as a broken gesture does |

Renderer settling: after `execute`, `executeAsync` and `call`, PF waits as
`PrimeFacesSelenide.runScript` does today (requests and page loads the step
started), and SC calls `prepareForInteraction()` before and `selenide.settle()`
after, as `SmartClientInterpretedWebDriverExecutor.executeExecute` does.
`waitFor` and `testScript` do not settle: they only read.

### Approved new constructs

Per Change Restraint in `docs/ai/project-agent-guide.md`, only these are approved:

- the step classes `ExecuteAsync`, `WaitFor`, `TestScript`, `Call`, and the JAXB
  child `CallParameter`, in `org.skyve.metadata.sail.language.step`;
- `Execute.variable`;
- the interface `org.skyve.metadata.sail.execution.SailCall`;
- the four `Executor` methods;
- a public `WebDriver` accessor on the harness, only if none exists;
- one shared private or package-private helper in `InterpretedWebDriverExecutor`
  that wraps and runs a script body with the `sail` argument;
- one test `SailCall` fixture class in `skyve-war` tests.

Anything else (a shared step base class, an interface over the script steps, new
renderer hooks, new settings) needs Mike's approval first, with the reason.

## Affected modules

- `skyve-core`: language classes, `Procedure`, `Interaction`, `Executor`,
  `SailCall`, `package-info.java`, the test copy of `sail.xsd`, and tests.
- `skyve-ext`: `WebDriverExecutor`, and `SailExecutorTest`.
- `skyve-web`: only if `PrimeFacesInlineWebDriverExecutor` overrides something
  affected; `PrimeFacesInlineWebDriverExecutorTest` if it emits code for these steps.
- `skyve-war`: `src/main/java/schemas/sail.xsd`; the `util/sail` harness
  (`InterpretedWebDriverExecutor`, PF/SC executors, and `PrimeFacesSelenide.runScript`
  if it needs to take arguments and return the result); the kitchen sink
  scripts and ITs; `SailLanguageCoverageTest` and its allow-list if needed.
- `docs/sail.md`.

## Dependencies

None blocking. Branch from local `master` at `1c80d14c45` or later.

Downstream: applications that copy the `util/sail` harness into their own tests
(see "Adding SAIL to an Application" in `docs/sail.md`) need the W2 harness
changes copied, and they fail to compile against the new `Executor` until then.

## Backward compatibility

- Script authors: additive. Existing `<execute>` scripts behave the same; the
  only change is the transparent function wrapper and the extra argument.
- `Executor` implementers outside Skyve: four new abstract methods (breaking at
  compile time, as previous op-codes were). Classes extending `WebDriverExecutor`
  inherit the comments. Record this under Upgrading.
- XSD: `execute` changes from a `simpleType` restricting `xs:string` to a
  `complexType` with `simpleContent` extending `xs:string`, plus an optional
  `variable`. Documents that validated before still validate.

## Current state observed

- `Execute` (`skyve-core/.../language/step/Execute.java`) has only an
  `@XmlValue` CDATA script. `Executor.executeExecute(Execute)` is the only hook.
- `Procedure` and `Interaction` list steps with `@XmlElementRef`.
- Both `sail.xsd` copies declare `<xs:element name="execute" type="tns:execute"/>`
  with a string `simpleType`. They are hand-maintained and must be identical.
- `WebDriverExecutor.executeExecute` emits `execute(<literal>);`.
- `InterpretedWebDriverExecutor.executeExecute` calls `executeScript` directly.
  PF overrides it with `selenide.runScript`, which waits for requests and page
  loads. SC wraps it with `prepareForInteraction()` and `selenide.settle()`.
- Kitchen sinks use `execute` once each: PF line ~775, SC line ~315.
- `ContextualExecutor.putVariable` and `RunState` hold variables;
  `CaptureStep` holds the variable-name pattern.
- The test strategy is applied in `InterpretedWebDriverExecutor`, around lines 390–430:
  under Verify, failures are recorded unless they halt.
- `docs/sail.md` "Adding an Op-code" is the checklist for this work; follow it in order.

## Implementation plan

### W0 Spike (throwaway, not committed)

In a scratch test under `skyve-war/src/test/java` (deleted afterwards), with
the harness's browser setup:

1. Show that `executeScript("return (async function (sail) { await new Promise(r => setTimeout(r, 500)); return sail.vars.x + sail.run; }).apply(this, arguments);", Map.of("run", "r1", "vars", Map.of("x", "y")))`
   returns `"yr1"` on Chrome. Also test Firefox if a driver is available.
2. Show that a rejected promise surfaces as a `JavascriptException` carrying the
   rejection message, and that a script longer than the script timeout throws
   `ScriptTimeoutException`. Also show that setting and restoring
   `timeouts().scriptTimeout` per step works.
3. Show that the existing kitchen sink `execute` bodies run unchanged inside the sync wrapper.

Record the results in "AI validation results". If step 1 or 2 fails, switch to
the fallback in locked decision 3, and record why.

### W1 Language (core, plus the minimum elsewhere to compile)

1. `Execute`: add `variable` (`@XmlAttribute`, nullable), with Javadoc
   specifying the step as in the semantics table.
2. New `ExecuteAsync`, `WaitFor` and `TestScript`, each with an `@XmlValue` CDATA
   script (`CDATAAdapter`, as `Execute` has). Their attributes:
   - `ExecuteAsync`: `timeoutMillis` (`Integer`) and `variable`;
   - `WaitFor`: `timeoutMillis`;
   - `TestScript`: `timeoutMillis` and `message`.

   `getIdentifier` returns the element name plus the script's first line,
   truncated to 60 characters.
3. New `Call` with `className` (required), `variable`, and
   `List<CallParameter> parameters`, from `<parameter name="…" value="…"/>`
   children. Both attributes of `CallParameter` are required, and a duplicate
   name fails when the step runs.
4. Each class gets `@XmlType` and `@XmlRootElement` with
   `namespace = XMLMetaData.SAIL_NAMESPACE`, implements `Step`, and is added to
   `package-info.java`. `execute(Executor)` calls the new `Executor` method.
   Validate the rules the XSD cannot express (empty body, bad variable name,
   negative timeout) when the step runs, failing with `MetaDataException`, as
   other steps do.
5. Add an `@XmlElementRef` for each class to `Procedure` and `Interaction`.
6. Add the four abstract methods to `Executor`, with Javadoc.
7. Add `SailCall` to `org.skyve.metadata.sail.execution`, with Javadoc as its
   contract. The contract covers the threading (the automation's thread), the
   instance lifecycle (one per step), how `AssertionError` and other exceptions
   are treated, and the return value.
8. Edit both `sail.xsd` copies by hand. Add the four elements and their types
   (`waitFor`, `testScript`, `executeAsync` and `call`), each with
   `xs:documentation`, and convert `execute` to `simpleContent`. Add the new
   elements wherever `execute` appears in step choices. Compare with schemagen
   output (`XMLMetaData.main`), but merge by hand.
9. `WebDriverExecutor`: comments, as in locked decision 9.
10. `skyve-war` harness: stub each new method to throw
    `UnsupportedOperationException("… is not implemented yet")`, so everything
    compiles. W2 replaces the stubs. Do the same in every test `Executor` in
    the reactor that must compile (`SailExecutorTest`'s `TestWebDriverExecutor`,
    `ContextualExecutorTest` helpers, …).
11. Tests:
    - step-class tests beside `ExecuteTest`;
    - `SailLanguageXmlTest` round trips for each element and attribute,
      including `<parameter>` children, CDATA bodies and an `execute` with no
      attributes;
    - `SuiteXmlTest` still passing;
    - `SailExecutorTest` for the emitted comments.

### W2 Harness (skyve-war tests)

1. In `InterpretedWebDriverExecutor`, one helper builds the `sail` argument
   (`run` and a copy of the variables, from `RunState`), wraps the body as
   locked decision 2 says, sets and restores the script timeout, and runs it.
   All four script steps use it.
2. Implement `executeExecute` (the wrapper, `variable`), `executeExecuteAsync`,
   `executeWaitFor` (polls with the existing wait helpers, and runs under None),
   `executeTestScript` (polls, follows the strategy, skipped under None, and
   substitutes `message`) and `executeCall` (loading, instance, substituted
   parameters, the failure split in the semantics table, `variable`).
   Trace each step, as `comment` does: the identifier, and for `call` the class
   and the parameter names.
3. PF: route `execute`, `executeAsync` and `call` through the settle that
   `runScript` does. Prefer giving `PrimeFacesSelenide.runScript` arguments and
   a return value over adding a hook. SC: wrap the same three as
   `executeExecute` does today.
4. Tests in `InterpretedWebDriverExecutorTest` with the mocked driver:
   - the wrapper and argument shape;
   - variables set, and a null result failing;
   - strategy behaviour for each step under Assert, Verify and None;
   - the `waitFor` timeout message with the last error;
   - the call failures (missing class, wrong type, constructor throws,
     `AssertionError` versus `RuntimeException`, duplicate parameter);
   - parameter substitution;
   - the script timeout restored after a timeout.

### W3 Conformance

1. Add a `SailCall` fixture, `EchoCall`, in `skyve-war/src/test/java/sail/kitchenSink/`. It
   echoes its `text` parameter back as its result, and with `fail="assert"` or
   `fail="error"` it throws the matching exception.
2. Both kitchen sinks, in "Contexts, links, maps and scripts":
   - `execute variable`;
   - `executeAsync` with an `await`, and `variable`;
   - `waitFor` on something the page sets after a `setTimeout`;
   - `testScript` that reads `sail.vars`;
   - a `call` with substituted `{run}` and `{var:…}` parameters and `variable`,
     then a `testScript` on its result.

   Use every attribute, so `SailLanguageCoverageTest` passes without allow-list
   entries.
3. Inline failure tests in each kitchen sink IT, run under Verify, one `@Test`
   with `assertAll` per step. Cover each failure in the semantics table.
   Never use `@TestFactory`.
4. Run `SailScriptsUnmarshalTest`, `SailLanguageCoverageTest`, and both kitchen
   sinks (`KitchenSinkSuiteSailIT`) headless against a disposable server, as
   "Running the Tests" in `docs/sail.md` describes. Run headed too if a display
   is available.

### W4 Documentation (`docs/sail.md`)

Update:

- the "Other steps" op-code rows, with PF and SC support;
- "The run token and variables": the substitution list, the four steps that set
  a variable, and the `sail` argument;
- a short "Scripts and calls" subsection with the XML example below and the
  contexts caution;
- "Other Executors", adding the four steps to the comment list;
- Known Limitations;
- Upgrading.

```xml
<call className="sail.kitchenSink.EchoCall" variable="echo">
	<parameter name="text" value="made {run}" />
</call>
<executeAsync variable="status"><![CDATA[
	const response = await fetch(location.href, {method: 'HEAD'});
	return response.status;
]]></executeAsync>
<waitFor timeoutMillis="20000"><![CDATA[
	return document.readyState === 'complete' && ! document.querySelector('.ui-blockui');
]]></waitFor>
<testScript message="The page should answer 200 and the call should echo"><![CDATA[
	return sail.vars.status === '200' && sail.vars.echo === 'made ' + sail.run;
]]></testScript>
```

## Acceptance checklist

- [ ] W0: promise awaiting, rejection and timeout behaviour recorded; approach chosen
- [ ] W1: four new elements and `execute@variable` unmarshal and round-trip; both XSDs identical and validating; `Executor` has four new methods; generators emit comments; reactor compiles
- [ ] W2: each step behaves as in the semantics table under Assert, Verify and None, proven by unit tests; existing `execute` scripts unchanged
- [ ] W3: both kitchen sinks pass with every new element and attribute; inline failure tests pass; `SailLanguageCoverageTest` and `SailScriptsUnmarshalTest` pass
- [ ] W4: `docs/sail.md` updated in every section listed
- [ ] Gates (tests, red/green, coverage, Eclipse, Sonar, Javadoc, formatting) pass on changed lines
- [ ] `mvn -o -B package` passes on the branch

## Better engineering notes

- Deferred: a `device` (browser profile) attribute on `<automation>`, so a
  workflow can switch persistent browser profiles declaratively; generated code
  for `call` in `WebDriverExecutor`; recorder support (nothing to record).
- Risk: scripts and calls bypass what SAIL tests. The docs should say to use
  them for what SAIL cannot express, and keep assertions on the UI in SAIL steps.
- Risk: wrapping existing `execute` bodies. W0 and W3 cover it. If any shipped
  script breaks, keep the raw path for `execute` without `variable`, and record why.

## AI validation plan

- W0 spike results recorded.
- Per work item: the item's tests below. The gates in
  `docs/ai/plan_sail_script_steps_prompt.md` run once, after W4.
- Core: `ExecuteTest`, the new step tests, `SailLanguageXmlTest`,
  `SuiteXmlTest`, `ContextualExecutorTest`.
- Ext: `SailExecutorTest`; web: `PrimeFacesInlineWebDriverExecutorTest`.
- War: `InterpretedWebDriverExecutorTest`, `SailScriptsUnmarshalTest`,
  `SailLanguageCoverageTest`, and both kitchen sinks through
  `KitchenSinkSuiteSailIT` headless.
- Final: `mvn -o -B package` on the branch, and aggregate coverage on the changed lines.

## AI validation results

Branch `sail-script-steps`, worktree `/Users/mike/skyve-sail-steps`, base `1c80d14c45`.

### W0 spike (2026-11-18)

A throwaway `util.sail.ScriptStepsSpikeTest` (deleted before W1) drove headless Chrome 158 (chromedriver
158.0.8127.36) and headless Firefox 159 (geckodriver 0.36.0, from `~/.cache/selenium`: the 0.20.1 in
`/usr/local/bin` cannot start Firefox 159), Selenium 4.38.0, on a `data:` page. The results were the same in both browsers:

1. **Awaiting.** `executeScript("return (async function (sail) { await new Promise(r => setTimeout(r, 500)); return sail.vars.x + sail.run; }).apply(this, arguments);", Map.of("run", "r1", "vars", Map.of("x", "y")))`
   returned `"yr1"` after about 510 ms. `arguments[0] === sail` is true. Results come back as
   `String`, `Long` (42), `Boolean`, `LinkedHashMap` (an object), and `null` for `undefined`. A sync wrapper whose body
   returns a promise is awaited too.
2. **Rejection and timeout.** A rejected promise, a rejected plain string and a synchronous throw each surface as
   `org.openqa.selenium.JavascriptException`. The first line of its message is the error: Chrome gives `javascript error: boom r1`,
   and Firefox gives `Error: boom r1`. The rest is Selenium's build, driver and command information. **The JavaScript stack does not reach
   Java.** No browser includes it, so the step's message gives the error's message only. With
   `timeouts().scriptTimeout(300 ms)`, a 3 s promise throws `ScriptTimeoutException` after about 305 ms (Chrome) or
   327 ms (Firefox). Restoring the earlier timeout (PT30S, the drivers' default) works, and the next 400 ms script
   passes.
3. **Shipped `execute` bodies.** The kitchen sinks' bodies and those in `PrimeFacesActionUploadSailIT`,
   `PrimeFacesSelectMenuSailIT`, `SmartClientListGridSelectedEventSailIT` and `SmartClientActionUploadSailIT` (11
   bodies) ran unchanged inside `return (function (sail) { BODY\n}).apply(this, arguments);` against stubs of `PF`,
   `SKYVE`, `jQuery` and `isc`, with the same effects as without the wrapper. `var` declarations stay function-local,
   as they already were in WebDriver's own function.

**Approach:** locked decision 3 holds, so `executeScript` with the async wrapper is used. The fallback is not needed.

## User validation suggestions

- Run both kitchen sinks headed and watch the new section.
- Write a five-line SAIL in skyve-war that waits on something in the browser and calls a
  trivial `SailCall`.
- Check that a `testScript` failure under Verify appears in the failure report with its `message`.
- Open a generated Control Panel test for a script containing the new steps, and check the comments.
