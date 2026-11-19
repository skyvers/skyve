# SAIL: Remember Me and Signed-Out Detection

## Goal

1. A SAIL script can sign in with Remember me ticked: `<login remember="true" …/>`.
2. SAIL notices when the browser is signed out, instead of passing on the login page.

## Context (verified)

- `login.jsp` has `<input type="checkbox" class="hidden" id="remember" name="remember">` with a styled `<label for="remember">`. `<login>` has only `customer`, `user` and `password`, and `Selenide.login` never touches the checkbox. An app that tests "a restarted browser stays signed in" has to write its own sign-in step. A downstream app did exactly that: it set `#remember` checked by script, then clicked submit.
- With Chrome on a persistent profile (`BrowserConfiguration.profile`), a restart keeps the `remember` cookie and loses the session cookies (`JSESSIONID`, `touch`). Opening a page then signs the browser in again by remember-me.
- **The false pass.** In a spike, a browser restarted on its profile without remember-me was signed out. `<navigateList module="admin" document="User"/>` and then `<testSuccess/>` both passed on the login page. `executeTestSuccess` only checks for error messages, and the login page has none. No navigate step notices a redirect to login. Only an explicit script check caught it.

## Locked decisions

1. **`remember` on `<login>`.** It's an optional boolean.
   - Absent: today's behaviour. The checkbox is left as the page has it.
   - `true`: the interpreted executors tick it, and `false` unticks it, before submitting.
   - Set it as a user's click on the label would. The checkbox itself is hidden, so set `checked` and fire `change`, or click the label. Choose whichever works in both renderers' login, and say why.
   - The WebDriver generator emits the same.
2. **One login-page test** on `Selenide`: `isLoginPage()`, true when the page shows Skyve's login form (`form[name=loginForm]`). Check whether the URL path is needed as well, and use it only if the form alone isn't reliable. Both renderers use it.
3. **Navigate steps refuse the login page.**
   - Which steps: `navigateList`, `navigateEdit`, `navigateTree`, `navigateMap`, `navigateCalendar`, `navigateMenu` and `navigateLink`.
   - The failure: if the page they land on is the login page, they fail as a step that can't do what it says. That stops the interaction under every test strategy, as a missing element does. The message is: `<step> landed on the login page: the browser is signed out (session expired, or remember-me didn't sign it in)`.
   - The exception: a `navigateLink` whose `href` is itself the login page (`login…`) may land there.
4. **`testSuccess` fails on the login page** as an assertion, so it follows the test strategy, with the message "expected success, but the browser is on the login page (signed out)".
   - The exceptions: straight after a `<logout/>`, and after a `<login>`, whose own check (`loginError`) is unchanged.
5. **Backward compatibility.** Scripts that passed while signed out now fail. That's the point, but it is a behaviour change.
   - Search every SAIL script and test in the repo (kitchen sinks, admin suites, harness tests) for one that relies on landing on the login page, and fix or exempt each one with a reason.
   - Record the change in `docs/sail.md` Upgrading and in `docs/skyve-10-changes.md`.
6. **SmartClient and PrimeFaces both.** The login and navigate code is in `InterpretedWebDriverExecutor` and the renderer Selenides. Put the check in one place that both use.

## Approved new constructs

- `Login.remember` (`Boolean`, `@XmlAttribute`), in both `sail.xsd` copies (`skyve-war/src/main/java/schemas/sail.xsd`, `skyve-core/src/test/resources/schemas/sail.xsd`). Follow "Adding an Op-code" in `docs/sail.md` for an attribute on an existing step.
- `Selenide.login(customer, user, password, @Nullable Boolean remember)`, or a change to the existing overloads, whichever is the smaller change. Keep the existing signatures working.
- `Selenide.isLoginPage()`.
- The navigate and `testSuccess` checks in decisions 3 and 4, and the WebDriver generator's output for `remember`.
- Harness unit tests, and kitchen-sink coverage of `remember` (and `SailLanguageCoverageTest` if attributes are covered there).

Anything else needs Mike's approval: ask, with the reason and the smallest alternative.

## Out of scope

- Checking the `remember` cookie itself, which belongs to device setup and stays in the app.
- Re-signing in automatically when signed out.
- A `<testSignedIn/>` step. Decisions 3 and 4 cover the need. Propose one only if they can't.

## Tests

- **Unit:**
  - `Login` unmarshals with and without `remember`;
  - the XSD accepts it;
  - each navigate step fails on a fake login page with the message;
  - `navigateLink href="login"` doesn't fail;
  - `testSuccess` fails on the login page, but not after `<logout/>` or `<login>`;
  - the existing harness tests keep passing.
- **Browser** (a headless-Chrome `*IT`, or the harness's existing real-browser test style; a disposable server where one is needed, as "A disposable server" in `docs/sail.md`):
  - `<login remember="true">` on a profile, then a restart on that profile, is still signed in on the next navigate;
  - without `remember`, the restart is signed out, and the next navigate fails with the message.

  If a server can't be run, record exactly what wasn't run.

## Docs

`docs/sail.md`: the `remember` attribute; what signed-out detection does and its `navigateLink` exception; the Upgrading entry. `docs/skyve-10-changes.md`: one release-note line.

## AI validation results

- Build and tests: `mvn -o -B package` passed in all modules; after the review fixes the affected unit tests (util.sail, HarnessSelfContainedTest, language, generator) passed again.
- Browser: `LoginRememberSailIT` passed against a disposable server (private WildFly 35 base, exploded war, H2): remember across a restart, and the signed-out failure and `testSuccess` failure without it. `PrimeFacesConvertersSailIT` and `SmartClientConvertersSailIT` passed with `remember="true"` in their dates scripts. Not run in a browser: SmartClient navigate steps on the login page, and `navigateMenu`'s signed-out check in either renderer.
- Red/green: each new behaviour was broken in turn and a test failed (executor drops remember, navigateList, navigateEdit, navigateLink login exemption, testSuccess check, testSuccess logout exemption, setRememberMe, generator, `Login.remember`).
- Coverage: JaCoCo agent over the util.sail tests. Uncovered changed lines are the browser-only ones: `Selenide.login`/`isLoginPage`, `SmartClientSelenide.login`, `PrimeFacesSelenium.login`, both `navigateMenu` checks (covered by the ITs, not measured). `Login.java` fully covered.
- Eclipse (JDT 3.48): two warnings on changed lines, fixed. Sonar (`skyve-local-remember`): two issues on changed lines, fixed.
- Spotless check on the changed skyve-war files: clean.
- Assembly safety: `util.sail` tests pass with `src/test/java/{modules,org,resources,router,sail}` and `src/test/resources/sail` removed; `HarnessSelfContainedTest` guards it.

## Acceptance

- [x] Decisions 1–6 built and tested.
- [x] The browser test proves remember-me across a restart, and the signed-out failure.
- [x] No repo script relies on passing while signed out (or each exception is recorded).
- [x] Gates pass.
