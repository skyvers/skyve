# Remember Me Per Device: Sign Out Ends It In One Browser Only

## Goal

Signing out ends Remember me in the browser that signed out, and in no other browser or device of the user. After a restart, the user's phone and tablet are still signed in by their own Remember me cookies, however the user signed out on their office PC.

## Context (verified)

- **The cause.** Skyve's `SpringSecurityConfig` (`skyve-war/src/main/java/org/skyve/impl/web/spring/SpringSecurityConfig.java`) configured `.rememberMe(c -> c.key(...).tokenRepository(tokenRepository())...)`, so Spring built a stock `PersistentTokenBasedRememberMeServices`. Its `logout` (Spring Security 6.5.11 source, the version in the root `pom.xml`) is:

  ```java
  super.logout(request, response, authentication);   // cancels this browser's cookie
  if (authentication != null) {
      this.tokenRepository.removeUserTokens(authentication.getName());
  }
  ```

  `SkyveRememberMeTokenRepository.removeUserTokens(String)` runs `delete from ADM_UserToken where userName = ?`: every series of the user, so every device. Verified in the Spring sources jar and the repository. The browser run in the AI validation results shows it: before the change, signing out on one profile signed out the other after a restart.
- **How Skyve signs out.** All the sign out links go to `/loggedOut`: the SmartClient desktop header (`skyve-web/src/js/desktop/header.js`), the PrimeFaces templates' `loggedOut` menu items (`diamond`, `editorial`, `ecuador`, `ultima`), the expired and reset-password page timers, and SAIL's `<logout/>`. `LoginServlet` forwards `/loggedOut` to `pages/loggedOut.jsp` (or the customer's own page), which calls `WebUtil.logout(request, response)`. That calls `request.logout()`, which runs Spring's logout handlers, among them the remember-me services' `logout(request, response, authentication)`, with the request still carrying its cookies. Then the session is invalidated and `deleteCookies` deletes every cookie but a few. There is no `SkyveLogout` handler and no `LogoutFilter` path of its own in use: the `.logout(...)` configuration only sets the success URL and deletes `JSESSIONID`. The only other `WebUtil.logout` call is commented out (a CSRF check in `SmartClientListServlet`). So `RememberMeServices.logout` is the one place a sign out reaches the tokens.
- **The cookie.** Name `remember` (`SpringSecurityConfig.REMEMBER_ME_PARAMETER`), value Base64 of `series:token`. The series identifies this browser's row in `ADM_UserToken`; Spring issues a new series at each Remember me sign in and rotates the token at each use. `ADM_UserToken` has `userName` (`customer/userName`, as `Authentication.getName()` gives it), `series`, `token` and `lastUsed` (`modules/admin/UserToken/UserToken.xml`). Nothing needs adding: a delete by `userName` and `series` is enough.
- **Every path that removes tokens, and what each does now** (and after this change):

  | Path | Where | Removes | After |
  | --- | --- | --- | --- |
  | Sign out | `PersistentTokenBasedRememberMeServices.logout` | all of the user's series | only the series in this browser's cookie; nothing if there is no cookie, an invalid cookie or no signed-in user |
  | Password change (any route that saves the User with a changed password: Profile, Account, reset by email, admin) | `UserBizlet.postSave`, `removeUserTokens(Persistence, String)` | all of the user's series, with the user's sessions | unchanged: all devices |
  | Stolen cookie (a presented token does not match its series) | `PersistentTokenBasedRememberMeServices.processAutoLoginCookie` | all of the user's series, and the cookie throws `CookieTheftException` (see `web.xml`) | unchanged: all devices |
  | User disabled (`inactive`, not `activated`), or deleted | nothing removes rows | no rows; the sign in is refused instead: `jdbcUserDetailsManager` reads `enabled` as `not inactive and activated`, and Spring checks the loaded user before it signs in a cookie (deleted users are not found) | unchanged: the user is on no device; stale rows stay, as before |
  | Admin "sign out everywhere" | none exists | | not added: needs Mike's decision |
  | Expired tokens | `ADM_UserToken` rows are not purged; an old cookie is just refused | | unchanged |

- **Apps with their own remember-me** build their own `RememberMeServices` or copy `SpringSecurityConfig`; they are untouched by a change to Skyve's configuration and keep signing out everywhere until they opt in.
- **Two factor sign in.** `TwoFactorAuthPushEmailFilter` is a `UsernamePasswordAuthenticationFilter` that is built without `RememberMeServices` (a `NullRememberMeServices`), so a two-factor sign in does not issue a Remember me cookie. That is as before and out of scope.

## Locked decisions

1. **Sign out removes only the series in this browser's `remember` cookie.** With no cookie, an undecodable cookie, a cookie that is not `series:token`, or no signed-in user, nothing is removed. The cookie itself is still cancelled in every case, as before.
2. **The delete is by user and series** (`delete from ADM_UserToken where userName = ? and series = ?`), so a cookie can only remove a series issued to the signed-in user.
3. **A password change, a stolen cookie and any all-devices removal keep removing all of a user's tokens.** `removeUserTokens` is unchanged.
4. **No schema change.**
5. **A new class, `org.skyve.impl.web.spring.SkyveRememberMeServices`**, in `skyve-web`: a `PersistentTokenBasedRememberMeServices` that overrides `logout`. It does not call `super.logout`, which would remove all, and so cancels the cookie itself. If its repository is not a `SkyveRememberMeTokenRepository` (the only one that can remove a series), it signs out as Spring does.
6. **`SkyveRememberMeTokenRepository.removeUserToken(userName, series)`** and its configurable SQL (`getRemoveUserTokenSql`/`setRemoveUserTokenSql`), beside the existing `removeUserTokensSql`.
7. **`SpringSecurityConfig` builds the services** in `rememberMeServices()` and passes them with `c.rememberMeServices(...)`. Spring refuses a cookie name set on the configurer beside custom services, so the token validity, parameter, cookie name and secure flag move onto the services, with the same values.
8. **Backward compatible.** An app with its own remember-me configuration is unchanged. The release note (`docs/skyve-10-changes.md`) records the behaviour change and how to opt in.

## Out of scope

- An admin "sign out everywhere" action, or deleting a user's token rows when the user is disabled or deleted. Neither exists now, and a disabled or deleted user cannot sign in by cookie anyway.
- Purging expired token rows.
- Issuing a Remember me cookie from a two-factor sign in.

## Tests

- **Unit, `SkyveRememberMeServicesTest`** (`skyve-web`): a logout with a cookie removes only that series and never `removeUserTokens`; it cancels the cookie; with no cookie, other cookies only, an invalid cookie, a cookie of the wrong shape, or no signed-in user it removes nothing; with a repository that is not Skyve's it removes all as Spring does.
- **Unit, `SkyveRememberMeTokenRepositoryTest`**: the new SQL and its setters; `removeUserToken` passes the user then the series; it needs a `JdbcTemplate`; and against a real H2 table, only that series of that user goes, another user's series of any name stays, and `removeUserTokens` still removes all of one user's.
- **`UserBizletH2Test`**: a password change (`postSave` with the password-changed stash) removes every series of the user, on all devices, and no other user's.
- **`SpringSecurityConfigTest`**: the filter chain's remember-me filter, the form login filter and the logout handlers all use the same `SkyveRememberMeServices`, with the `remember` cookie name.
- **Browser, `RememberMePerDeviceSailIT`** (`util.sail`, tag `sail`; a disposable server as in "A disposable server" in `docs/sail.md`): Chrome profiles for a phone and a PC both sign in as `admin` with `<login remember="true"/>`; `<logout/>` on the PC; after a restart the phone is still signed in and the PC is signed out.
- Red/green: break each behaviour in turn and a test fails (see the results).

## Docs

`docs/skyve-10-changes.md`: the behaviour change and the upgrade note. `docs/architecture-design.md`: the new class in the security table.

## AI validation results

- **Verified cause.** Spring Security 6.5.11's `PersistentTokenBasedRememberMeServices.logout` calls `tokenRepository.removeUserTokens(authentication.getName())`, which in `SkyveRememberMeTokenRepository` deletes every `ADM_UserToken` row of the user. Confirmed in a browser: on a server running the build from before this change (the older `run-pf-base` war), `RememberMePerDeviceSailIT` fails at its first check, "`<navigateList />` landed on the login page: the browser is signed out", because the phone's Remember me was gone after the PC signed out.
- **Unit tests.** `SkyveRememberMeServicesTest` (9), `SkyveRememberMeTokenRepositoryTest` (22, incl. a real H2 delete), `UserBizletH2Test` password change removes all (18 in the class), `SpringSecurityConfigTest` (5): all pass.
- **Browser.** `RememberMePerDeviceSailIT` passed against a disposable server (private WildFly 35 base, exploded war built from this branch, H2, port 9080): phone still signed in after the PC signed out and both restarted; PC signed out. It failed against the pre-change build, as above. The first attempt failed because the test ran both devices on the phone's configuration (the harness restarts the browser on the configuration's profile each script); the test now changes the profile on the one configuration, which the harness treats as a browser-level change. The server was stopped afterwards. Not run: SmartClient (desktop) sign out, the PrimeFaces templates' own sign out menu items and a customer-defined logged out page. All reach the same `/loggedOut` page, which calls `WebUtil.logout`; the IT's `<logout/>` goes through it.
- **Red/green.** Each behaviour was broken in turn and a test failed: logout always removing all (5 of 9 `SkyveRememberMeServicesTest` fail), no-cookie logout removing all (2 fail), the single-series SQL not matching the series (3 repository tests fail), `UserBizlet.postSave` not removing tokens (the new H2 test fails), the config using the stock services (`SpringSecurityConfigTest` fails). The browser test is the red/green for the whole path.
- **Gate 1.** `mvn -o -B package` in `skyve-ext`, `skyve-web` and `skyve-war`: BUILD SUCCESS (3477, 3422 and 5098 tests, 0 failures; the war's ITs are not part of `package`).
- **Eclipse JDT (IDE compiler 3.48, repo prefs)** over the changed files: three warnings on changed lines (an unnecessary `static-method` suppression, a redundant `instanceof` assertion, a null-safety warning in `UserBizletH2Test`), all fixed; none remain.
- **Spotless check** (`skyve-war` is the only module that has it; run in a scratch git copy because the worktree's `.git` file is not found by the plugin), with `-DspotlessFiles` for the three changed war files: two test files needed formatting, applied, now clean. `RememberMePerDeviceSailIT` is under `util/`, which Spotless excludes.
- **Local Sonar** (`skyve-sonar`, project `skyve-local-remember-per-device`, started and stopped again): no issues on any changed file.
- **JaCoCo** (`-Pcoverage`, focused tests): `SkyveRememberMeServices` fully covered (lines and branches); the new lines of `SkyveRememberMeTokenRepository` fully covered; `SpringSecurityConfig.rememberMeServices()` is exercised by `SpringSecurityConfigTest`.
- **Not run:** the full SAIL IT suites, and a test on a database other than H2 (the SQL is standard `delete ... where userName = ? and series = ?`).

## Acceptance

- [x] Sign out removes only this browser's series; no cookie removes nothing.
- [x] A password change still removes all of a user's tokens.
- [x] The browser test proves the phone stays signed in after the PC signs out.
- [x] Every token-removal path is listed with what it does.
- [x] Backward compatibility is noted in the release notes.
- [x] Gates pass.
