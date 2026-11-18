# Two Factor Sign In Issues The Remember Me Cookie

## Goal

A successful two-factor sign in with Remember me ticked leaves the browser with the `remember` cookie, as a sign in without a second factor does. After a restart the browser is signed in without a new code, so a phone app (such as basix's offline inspection app) can sync without a fresh sign in, with a code, after every browser restart.

## Context (verified)

- **The premise was wrong for the ordinary path, and the filter was misread.** The plan `plan_remember_me_per_device.md` said that "`TwoFactorAuthPushEmailFilter` is built without `RememberMeServices` (a `NullRememberMeServices`), so a two-factor sign in does not issue a Remember me cookie." That is true of the filter's own `UsernamePasswordAuthenticationFilter` machinery, but the filter never completes a sign in itself. At a good code `TwoFactorAuthPushFilter.doTFACodeCheckProcess` calls `attemptAuthentication` only to check the code, returns `false`, and `doFilter` continues the chain. The next filter is the form login filter that `SpringSecurityConfig` configures (`.formLogin(...)`), which authenticates the same request again (the code is the password, see `SkyveSpringSecurity.useTFAPushCodeAsPassword`), runs the session strategy and `SkyveAuthenticationSuccessHandler` (which clears the code), and calls `rememberMeServices.loginSuccess`. That filter has the shared `SkyveRememberMeServices` (`SpringSecurityConfigTest` already asserts it is the same instance as the remember-me filter's). So the cookie is issued from the form login filter, by the same per-device services as a normal sign in, when the request carries the `remember` parameter.
- **Verified before any change, against a running server** (a disposable WildFly 35 with H2 and email two-factor on for `demo`, mail logged by `smtp.testBogusSend`; curl and Chrome):
  - `POST /loginAttempt` with `remember=on` and the password sends the code and shows the code page with the Remember me box ticked (`login.jsp` sets `checked` from the `remember` request attribute that the filter sets).
  - The code step, with the box still ticked, answers `302 /skyve/` with one `Set-Cookie: remember=...; Max-Age=1209600`. Presenting only that cookie to `/` shows the signed-in home page, with no login form and no code. With the box unticked there is no `remember` cookie.
  - **The real gap.** After a wrong code the filter forwarded to the code page with `forwardToTwoFactorPage(request, response, user)`, which hard-coded `rememberMe = false`. The page came back with Remember me unticked, so the right code that followed signed in with no cookie unless the user noticed and re-ticked the box. Reproduced with curl (box `checked` absent after a wrong code) and in Chrome (`TwoFactorRememberSailIT`, below).
- **Every two-factor path, and what it does now** (and after this change):

  | Path | Where | Now | After |
  | --- | --- | --- | --- |
  | Password step (a customer in `account.tfaCustomers` whose Configuration has two-factor type email) | `TwoFactorAuthPushFilter.doPushNotificationProcess`, `TwoFactorAuthPushEmailFilter.pushNotification` | emails the code, shows the code page, carries `remember` (the request attribute `REMEMBER_ATTRIBUTE`) into the page's box | unchanged |
  | Resend | `doResendProcess` | resends, carries `remember` from the request | unchanged (it already read the parameter; now through one helper) |
  | Code step, good code | `doTFACodeCheckProcess`, then the form login filter | the cookie is issued by the form login filter's remember-me services if `remember` is posted | unchanged, now locked by tests |
  | Code step, wrong code | `doTFACodeCheckProcess` | code page again with Remember me **unticked** | code page again with Remember me as the user left it |
  | Code step, expired, locked, or other failure | the same | back to the login page with an error, no cookie | unchanged |
  | Sign in by the `remember` cookie | `RememberMeAuthenticationFilter` | signed in, no second factor | unchanged (a locked decision, below) |
  | `admin.UserAccount.secondFactorPreferredMethod` (Authenticator, Email, SMS), `account.secondFactor` and `account.authenticationMethods` in `skyve.json` | `UserAccount.xml`, `skyve.json` | declared and shown on the User Account view, but **nothing reads them**: there is no TOTP or SMS filter or controller. `TfaType` is `OFF`, `EMAIL` or `UNSUPPORTED`, and an `UNSUPPORTED` type refuses the sign in (`doUnsupportedPushConfigurationProcess`) | unchanged; a future filter must carry `remember` the same way |

  The email filter is the only two-factor sign in. There are no other filters or controllers.
- **How the choice survives from the password step to the code step.** There is no session attribute and none is needed. The filter sets the request attribute `remember` from the posted parameter, `login.jsp` renders the code page's Remember me checkbox `checked` from it, and the code step posts the checkbox (`remember=on`) with the code and `tfaToken`. So the user's choice is the box on the code page, which starts as the box on the password page and which the user may change. The SAIL `<login remember="true"/>` ticks `#remember` on the password page, as before.
- **Security note.** The cookie (and the series it names in `ADM_UserToken`) now stands in for both factors for as long as it lives. That is how Spring treats any remembered sign in, and it is the point of the change. It ends when the cookie expires (`account.rememberMeTokenTimeoutHours`, 336 by default), when the user signs out in that browser (`SkyveRememberMeServices.logout` removes that series), when a password change removes every series of the user (`UserBizlet.postSave`), or when Spring detects a stolen cookie. Per-device sign out is on master. "Sign out everywhere" is being built on another branch (`sign-out-everywhere`); it is the control for a lost phone.

## Locked decisions

1. **Use the same `SkyveRememberMeServices` (per-device series) as the normal sign in.** No second cookie mechanism. The two-factor filter does not issue the cookie itself and is not given remember-me services: the form login filter that follows it already does, once, with the shared services. Making the two-factor filter complete the sign in itself (and so replace the second authentication) was considered and rejected: it changes lockout, audit and success-handler behaviour for no gain, and the cookie already issues.
2. **With Remember me unticked, nothing changes.**
3. **A sign in by the remember-me cookie does not ask for a second factor again.** That is Spring's behaviour for a remembered sign in and the point of the change. The cookie stands in for both factors until it expires or is signed out (see the security note). Per-device sign out and, when it lands, "sign out everywhere" are the controls.
4. **The user's choice is the Remember me box on the code page,** carried from the password page by the request attribute and re-posted with the code. A mistyped code keeps it as the user left it (the fix), as the resend already did.
5. **One helper reads the choice.** `TwoFactorAuthPushFilter.isRememberMeRequested(request)` replaces the three copies of `request.getParameter(REMEMBER_PARAMETER) != null`; the three-argument `forwardToTwoFactorPage` that dropped the choice is removed.
6. **No schema change, no new configuration, no new class.**
7. **Backward compatible.** An app with its own security configuration is unaffected. For one that uses the two-factor filter with its own form login and remember-me services, the sign in behaves as before.

## Out of scope

- TOTP (Authenticator) and SMS second factors: there is no code for them (see the table).
- Asking for a second factor again when a remember-me cookie signs a user in, or a shorter validity for a two-factor Remember me. Both would be new decisions.
- "Sign out everywhere" (another branch).

## Tests

- **Unit, `TwoFactorAuthPushFilterTest`** (`skyve-web`): a wrong code forwards with `REMEMBER_ATTRIBUTE` true when the box was ticked and false when not; a good code continues the chain with the `remember` parameter intact and does not set the attribute. The existing tests cover the lockout, expiry, resend and invalid-code paths, unchanged.
- **Unit, `TwoFactorRememberMeSignInTest`** (`skyve-war`): the two-factor filter and the form login filter that `SpringSecurityConfig.filterChain` builds, in sequence, with a real `SkyveRememberMeServices` and an in-memory token repository: a good code with Remember me ticked redirects home and issues the `remember` cookie (HttpOnly, max age set) whose series is stored for the user; unticked issues none; a bad code issues none and shows the code page again.
- **Browser, `TwoFactorRememberSailIT`** (`util.sail`, tag `sail`; a disposable server as in "A disposable server" in `docs/sail.md`, set up for email two-factor with bogus mail, with `-Dsail.serverLog` naming its log, from which the test reads the code): (1) sign in as admin with Remember me, enter the code, restart the browser on the same profile: signed in, no code asked, no code emailed; (2) the same after a mistyped code first: the box is still ticked and the right code is remembered; (3) without Remember me: signed out after a restart.
- **Red/green** (see the results): each behaviour was broken in turn and a test failed.

## Docs

`docs/skyve-10-changes.md` (the behaviour, the fix and the security note); `docs/sail.md` (the new IT and what it needs). The code-page behaviour is described there for people who run the SAIL tests; there is no other user documentation of two-factor sign in to change.

## AI validation results

- **Browser, green.** `TwoFactorRememberSailIT` passed, 3 of 3 (18 s), against a disposable server (private WildFly 35 base, exploded war built from this branch, H2, port 9280) with email two-factor on for `demo` and `testBogusSend`. The server was stopped afterwards.
- **Browser, red.** Against a war built without the fix to `TwoFactorAuthPushFilter` the mistyped code test failed ("with Remember me still ticked ==> expected: true but was: false"); the other two passed, as the ordinary path was already right.
- **Unit red/green.** Without the fix `testBadTwoFactorCodeKeepsRememberMeTicked` fails (the others pass). In `TwoFactorRememberMeSignInTest`: the chain not using the remember-me services fails the "issues the cookie" test, and `alwaysRemember` fails the "unticked issues none" test. The "bad code issues nothing" test guards the lockout and failure paths, which the unchanged existing tests also cover; it has no mutation of its own.
- **Gate 1, `mvn -o -B package`** over `skyve-web` and `skyve-war`: see Gates below.
- **Eclipse JDT (IDE compiler 3.48, each module's prefs)** over the changed files: no warnings on changed lines in `TwoFactorAuthPushFilter` or its test; three in `TwoFactorRememberMeSignInTest` (an unused import, two resource warnings for the application context), fixed or suppressed with a reason; none remain. The remaining warnings in those files are older lines.
- **Spotless check** (`skyve-war`; scratch git copy because the worktree's `.git` file is not found by the plugin, with `-DspotlessFiles`): `TwoFactorRememberMeSignInTest` clean (a deliberate violation was caught, so the check does look). `TwoFactorRememberSailIT` is under `util/`, which Spotless excludes; `skyve-web` has no Spotless.
- **Not run:** SmartClient (desktop) sign in (it posts the same `loginAttempt` form), a customer's own login page, a database other than H2, an SMS or authenticator factor (there is none), and an MFA sign in on a phone browser (the IT uses Chrome with the tablet user agent as the other remember-me ITs do).

## Acceptance

- [x] A good two-factor code with Remember me ticked issues the `remember` cookie (unit and browser).
- [x] A restarted browser is signed in with no code asked (browser).
- [x] Unticked: nothing changes; signed out after a restart (unit and browser).
- [x] A failed code issues nothing; lockout behaviour unchanged (existing tests and the new bad-code test).
- [x] A mistyped code keeps the Remember me choice (fixed, unit and browser).
- [x] Every two-factor path is listed with what it does.
- [x] The security note and the controls are recorded.
- [ ] Gates pass (see the report).
