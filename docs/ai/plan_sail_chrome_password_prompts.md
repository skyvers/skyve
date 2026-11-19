# SAIL: Chrome Password Prompts Off

## Goal

SAIL's Chrome (and Edge) starts with the password manager's prompts off, so a sign in with a leaked password cannot block all input to the tab.

## Context (verified)

- A downstream app (basix) has a user `setup` with password `setup`. About a second after signing in as it in SAIL's Chrome, every real input stops reaching pages of that origin: Selenium `sendKeys` and clicks, and raw CDP `Input.dispatchKeyEvent` and `dispatchMouseEvent`. Script-set values and `executeScript` still work, and `document.hasFocus()` stays true. `data:` and `about:blank` pages in the same tab still take keys.
- The cause is Chrome's password leak detection. Its "Change your password" prompt is tab-modal browser UI, not in the DOM. Other users with the same password but a different name are unaffected. Setting only `profile.password_manager_leak_detection=false` fixes it.
- In a `SailSuite` the browser is handed from member to member, so the prompt carries over and the next member's `<login>` fails with "The sign in fields did not take the typed values".
- The harness builds `ChromeOptions` in one place, `CrossBrowserSelenium.setupChrome`, and `EdgeOptions` in `setupEdge`. The profile (`BrowserConfiguration.profile`) and extra arguments are only command-line arguments. A mobile device is a user agent string and a window size (`Devices`), also arguments; there is no `mobileEmulation` option. The generated WebDriver tests and interpreted executors start browsers through `CrossBrowserSelenium` too. Nothing else in the repository starts ChromeDriver.
- ChromeDriver already sets `credentials_enable_service=false` and `profile.password_manager_enabled=false` on its own profiles (checked in a profile's `Preferences` file), and turns Safe Browsing off, but does not set `profile.password_manager_leak_detection`.
- Reproduction in Skyve's own harness: a JDK `HttpServer` with a plain sign in form, headed Chrome 158, user `setup` / `setup`, wait 3 s, then type. Without the preferences the typed value stays empty. `admin` / `admin` (the bootstrap user) did not trigger the prompt in the same test, so a Skyve disposable server's test data does not reproduce it on its own.

## Decisions

1. `CrossBrowserSelenium.passwordPromptPreferences()` is the three preferences, all false: `profile.password_manager_leak_detection`, `credentials_enable_service` and `profile.password_manager_enabled`. The first is the fix; the other two stop the "Save password?" bubble and match what ChromeDriver does already, so a profile that was made by another tool gets them too.
2. `chromeOptions(config, headless)` and `edgeOptions(config, headless)` build the options (arguments, then preferences), so they can be tested without a browser. Edge is Chromium and has the same preference names.
3. The preferences are merged into any `prefs` already on the options; the existing value wins for the same name, so a caller can turn a prompt back on. Selenium has no getter for an experimental option, so the existing ones are read from `asMap()`. Today no caller sets any (`BrowserConfiguration` has no preference setting); the merge is for the next one.
4. No new setting on `BrowserConfiguration`. Tests that need the prompts on are rare; they can build their own options.
5. Firefox and Safari are unchanged: no equivalent prompt seen.

## Tests

- Unit, `PasswordPromptPreferencesTest`: plain Chrome; headless Chrome with a profile and extra arguments (prefs and arguments both present); a mobile device (user agent and size); Edge; the map is new each time; existing preferences kept, and the caller's value wins.
- Browser, `PasswordPromptSailIT` (needs Chrome and the network, not a Skyve server): signs in as `setup` / `setup` to a JDK `HttpServer`, waits 3 s, checks `sendKeys` reaches a field.

## AI validation results

- Red: with the `turnOffPasswordPrompts` calls commented out, `PasswordPromptSailIT` failed: `real keys reach the page ==> expected: <typed> but was: <>`. Green with them restored. (The same test with `admin` / `admin` passed with the preferences off: Chrome does not flag that pair here.)
- Unit tests: `PasswordPromptPreferencesTest` and `DeviceProfileTest` pass.
- Build: `mvn -o -B -pl skyve-war -am package` with the `util/sail/*Test` tests and `sail.HarnessSelfContainedTest` (221 tests): passed.
- Eclipse (JDT 3.48, the IDE's batch compiler, with skyve-war's prefs) on the three changed files: two warnings on changed lines (a misplaced `@SuppressWarnings`, an unchecked cast), fixed; clean after.
- Sonar (local, project `skyve-local-chrome`): one issue on changed lines, `java:S2925` (`Thread.sleep`) in `PasswordPromptSailIT`; the wait is the test (the prompt is browser UI the page cannot see), so it is suppressed with a reason. Nothing in `CrossBrowserSelenium` or the unit test.
- Spotless: `skyve-war/pom.xml` excludes `src/test/java/util/**`, so it does not apply to these files; the formatting was done by hand (tabs).
- JaCoCo (agent over the unit tests): every changed line in `CrossBrowserSelenium` is covered except the two `new ChromeDriver(...)` / `new EdgeDriver(...)` start lines and the `setupBrowser` calls after them, which only a browser start runs. `PasswordPromptSailIT` ran through the Chrome one. The Edge start was not run: no Edge here.
- Not run: Firefox and Safari (unchanged), a Skyve disposable server (the IT uses a stand-in server, as above).

## Acceptance

- [x] Chrome and Edge options carry the preferences, with and without a profile, and for a mobile device.
- [x] Existing preferences are merged, not overwritten.
- [x] The IT shows the failure without them and the pass with them.
- [x] `docs/sail.md` and `docs/skyve-10-changes.md` updated.
- [x] Gates (above).
