# Plan: Security and Identity Workflow Tests

## Summary

Part 1 adds the hand-written `SignOutOtherSessionsTest` so `generateDomain` stops generating the failing test for `admin.UserAccount`'s `SignOutOtherSessions` action. Part 2 audits every security and identity workflow, rates how well tests cover it, and adds unit and H2 tests for the highest-risk gaps (lockout, expiry, forced password change, reset tokens, role and group changes, the access matrix of the Spring chain, session fixation, security headers, activation expiry, the admin password paths and who holds the sensitive privileges). Product changes are limited to the BUG-1 and BUG-2 fixes and the BUG-4 and BUG-3 warnings. Four real defects were found. **BUG-1 and BUG-2 are fixed** (Mike's decision, 2026-11-19; the only product changes on this branch, plus warnings). **BUG-3 is known and deferred; BUG-4 is known and not fixed** (option (a)), with documentation warnings. BUG-3 and BUG-4 keep `@Disabled` tests that name them. Modules: `skyve-war` (tests only). Validation: `mvn -o -B package` over the reactor up to `skyve-war`, JDT 3.48, Spotless in a scratch copy, local Sonar, and red/green runs for each new guard.

## Scope

1. **Part 1.** `skyve-war/src/test/java/modules/admin/UserAccount/actions/SignOutOtherSessionsTest.java`, an H2 test with the same simple name as the generated one. `DomainGenerator` does not generate an action test when a test of that name exists under `src/test/java` (verified: with the file in place `generateDomain` no longer writes `UserAccount/actions/SignOutOtherSessionsTest.java`). Also commits the generated `SignOutEverywhereTest`, which master lacked.
2. **Part 2 audit** of the workflows in the inventory below.
3. **Part 2 tests**, in the order of risk: account takeover and lockout bypass, reset tokens, session fixation, privilege changes, then the rest.

Locked assumptions: no product change except what Mike decided for the defects below (an unfixed bug keeps its test, `@Disabled`, with the reason); no browser (SAIL) test was added, because every gap found can be proved below the browser and the browser-level behaviour (the generic "invalid" page, sign in and out in both renderers, change of password) is already in `sail/admin/{pf,sc}/01-session.xml` and `03-security-admin.xml`; one disposable server and one browser were not needed.

## Affected modules

`skyve-war` test sources, plus the fixes and warnings: `skyve-core` (`AppConstants.PASSWORD_RESET_STASH_KEY`), `skyve-web` (`WebUtil`, `RestService` Javadoc), `skyve-war` (`MakePasswordChange`, `ChangePassword.xml`, `web.xml` comment), and docs. `skyve-war/src/generatedTest/java/modules/admin/User/actions/SignOutEverywhereTest.java` is generator output, committed as the repository does for the other generated tests.

## Dependencies

Master `9d5df115a6` (per-device Remember me, two-factor Remember me, sign out everywhere). No blocking dependencies.

## Backward compatibility

Test-only. Downstream applications are unaffected. The new hand-written `SignOutOtherSessionsTest` is the same name basix-cloud already uses, so a downstream project that assembles Skyve gets no clash.

## Current state observed

### Workflow inventory

Rating: **covered**, **partly** (what is missing in the last column), **uncovered**. "Before" is the state at master; "After" is this branch.

| Workflow | Code | Tests before | Tests after |
| --- | --- | --- | --- |
| Sign in (form; both login pages) | `SpringSecurityConfig` (war), `SkyveSpringSecurity.jdbcUserDetailsManager`, `LoginServlet`, `pages/login.jsp`, `SkyveAuthenticationSuccessHandler`, `WebUtil.processUserPrincipalForRequest` | `SkyveSpringSecurityTest`, `SkyveAuthenticationSuccessHandlerTest`, `LoginServletTest`, `WebUtilH2Test`; SAIL `01-session.xml` in PrimeFaces and SmartClient (good, bad and unknown user) | + `SpringSecurityConfigAccessTest` (sign in through the real chain: success redirect, session id changed, bad password to `/login?error`), `AccountLockoutH2Test` (the real user query). **Covered** |
| Sign out | `LogoutFilter` in `SpringSecurityConfig`, `SkyveRememberMeServices`, `WebUtil.logout`, `SkyveSessionListener` | SAIL `01-session.xml`, `RememberMePerDeviceSailIT`, `SkyveSessionListenerTest` | + chain test: logout ends the session and clears `JSESSIONID`. **Covered** |
| Session timeout | `web.xml` `session-timeout` 30, `SkyveSessionListener`, `StateUtil.checkSession`, `SessionEndedException`, `rest/SessionFilter` | `SkyveSessionListenerTest`, `WebUtilValidatedSessionUserTest`, `SessionTrackingTest`, `SessionFilterTest` | unchanged. **Partly**: the 30 minute value and the session cookie flags set in `SkyveContextListener` (HttpOnly, Secure) are not asserted; the expired page is not driven in a browser |
| Remember me (per device) | `SkyveRememberMeServices`, `SkyveRememberMeTokenRepository`, `SignOut`, `RememberMeTokens` (test) | `SkyveRememberMeServicesTest`, `SkyveRememberMeTokenRepositoryTest`, `UserBizletH2Test`, `SignOutH2Test`, `RememberMePerDeviceSailIT`, `LoginRememberSailIT` | unchanged. **Covered** |
| Two-factor (email push; Authenticator and SMS refused) | `TwoFactorAuthPushFilter`, `TwoFactorAuthPushEmailFilter`, `TwoFactorAuthForwardHandler`, `TwoFactorAuthConfigurationSingleton`, `login.jsp` | `TwoFactorAuthPushFilterTest` (about 60: unsupported factors do not bypass, cooldown, expiry, locked, disabled, expired users cannot start a challenge), `TwoFactorAuthPushFilterLockoutH2Test`, `TwoFactorAuthPushEmailFilterH2Test`, `TwoFactorRememberMeSignInTest`, `TwoFactorRememberSailIT` | unchanged. **Covered** |
| Account lockout and unlock | `SecurityListener`, `SkyveSpringSecurity` (`hasActiveLockout`, `lockoutDurationMillis`, the user query), `UtilImpl.ACCOUNT_LOCKOUT_*` | `SecurityListenerTest` (mocked JDBC), `SkyveSpringSecurityTest` (the arithmetic), the two-factor lockout H2 test; nothing ran the real provider, the real query and the listener together | + `AccountLockoutH2Test` (10): below threshold, at threshold even the right password is refused, an attempt while locked extends the lockout, the lockout expires after failures x duration and a sign in clears the count, threshold 0 turns it off, disabled accounts are refused and not counted, an unknown user looks like a bad password and leaves a failed login record, the count is per customer. **Covered.** There is no admin "unlock" action: unlock is by expiry or a successful sign in, and `authenticationFailures` is on no view |
| Password change by the user (complexity, history, expiry, forced change) | `MakePasswordChange`, `ChangePassword`, `WebUtil.makePasswordChange`, `changePassword.jsp`, `home.jsp`, `LocalDataStoreRepository.populateUser`, `UserBizlet` | `MakePasswordChangeTest` (history, mismatch, same as old), SAIL `03-security-admin.xml` (change password in both renderers) | + `MakePasswordChangeSecurityH2Test` (wrong old password refused, right one works, complexity, a change ends the forced change and clears a reset token), `UserPrivilegesH2Test` (`passwordExpired`, `PASSWORD_EXPIRY_IN_DAYS` old, recent and never-changed, no expiry). **Partly**: BUG-1 (below); `home.jsp`'s redirect to the change page is covered only in the browser |
| Forgotten password and reset by token | `WebUtil.requestPasswordReset`, `WebUtil.resetPassword`, `requestPasswordReset.jsp`, `resetPassword.jsp`, `MakePasswordChange` | `WebUtilH2Test` (mail sent, valid token, unknown token, expired token, mail failure, GeoIP block) | + `PasswordResetTokenH2Test` (11): a token works once, expiry (inside and past), a typo or weak password does not burn the token, a disabled user is refused with the password unchanged, a reset ends Remember me, a new request replaces the old token, unknown email is silent, same email shares one token and one mail. **Covered**, with BUG-2 |
| Self-registration and activation; resend | `Register`, `SelfRegistrationBizlet`, `SelfRegistrationActivationExtension`, `User.generateActivationDetailsAndSave`, `ResendActivation` (both) | `SelfRegistrationBizletH2Test`, `SelfRegistrationActivationExtensionH2Test` (unknown, already, fresh), `ResendActivationTest` (null cases), `WebUtilH2Test.sendRegistrationEmail...` | + `SelfRegistrationActivationExpiryH2Test` (inside expiry, past expiry not activated, no expiry configured, once only). **Partly**: the `Register.execute` pipeline (captcha refused, GeoIP silent pass, no self-registration group, breached password warning) has no test |
| Admin creates, resets password, disables, enables, deletes a user | `UserBizlet`, `UserService.validateUserNameAndPassword`, `GeneratePassword`, `SignOutEverywhere`, `User.xml` views | `UserBizletH2Test` (disable, delete, rename, re-enable), `SignOutH2Test`, `SignOutEverywhereH2Test`, `GeneratePasswordActionTest`, `UserServiceH2Test`, SAIL `03-security-admin.xml` (wizard) | + `UserPasswordAdminH2Test` (hash not plain text, complexity, mismatch, duplicate name, generated password forces a change, typing another one does not), `UserPrivilegesH2Test` (a disabled user cannot be retrieved). **Covered** |
| Sign out of all other sessions | `SignOutOtherSessions`, `SignOut.otherDevices`, `StateUtil.removeOtherSessions` | `SignOutOtherSessionsH2Test`, `SignOutH2Test`, `SessionTrackingTest`, `SignOutOtherSessionsSailIT` and the SmartClient one | + `SignOutOtherSessionsTest` (Part 1). **Covered** |
| Role and group assignment taking effect | `LocalDataStoreRepository.populateUser`, `UserBizlet`, `UserService` | `LocalDataStoreRepositoryH2Test` (legacy id only) | + `UserPrivilegesH2Test` (a removed role is gone for the next retrieval, a group gives and takes away roles, a group alone can sign in, the data group is loaded), `AdminSecurityPrivilegesH2Test`. **Partly**: a session already running keeps the roles it was built with (see notes) |
| Customer and data-group scoping on sign in | the `bizCustomer` clauses of the user query and `populateUser`, `WebUtil.determineCustomerWithoutSession` | `LocalDataStoreRepositoryH2Test`, `CustomerOverrideH2Test` | + the per-customer lockout test, the data group test. **Partly**: a sign in to a customer the user does not belong to is not asserted |
| Login records and My Current Sessions | `WebStatsUtil.recordLogin`, `SecurityListener`, `UserLoginRecordBizlet`, `UserAccountBizlet`, `qMyLoginHistory` | `WebStatsUtilTest`, `UserLoginRecordBizletH2Test`, `UserAccountSecurityH2Test`, `UserAccountBizletH2Test` | + failed attempts leave records (lockout test). **Covered** |
| Impersonation or switch user | there is none; `SwapCustomer` swaps the customer of the DevOps user's session, and `DevLoginFilter` is development only | `SwapCustomerH2Test`, `DevLoginFilterTest` | + `AdminSecurityPrivilegesH2Test` (only DevOps can run `SwapCustomer`). **Covered** for what exists |
| Privileges that guard accounts | `admin.xml` roles | none | + `AdminSecurityPrivilegesH2Test`: only a security administrator runs the `User` actions, creates or deletes `User`, `UserRole` and `Group`; the ordinary roles are the ones that sign out other sessions. **Covered** |
| Access matrix of the Spring chain | `SpringSecurityConfig.filterChain` | `SpringSecurityConfigTest` (details source, Remember me wiring) | + `SpringSecurityConfigAccessTest` (open and secured paths, POST needs a sign in, PUT, DELETE and PATCH refused). **Covered** |
| CSRF on state-changing endpoints | `FacesView.setCsrfToken`, `StateUtil` tokens, `SmartClientListServlet.checkCsrfToken`, `SmartClientEditServlet` | `FacesViewTest` (mismatch redirects), `SessionTrackingTest`, `StateUtilTest` | + `SmartClientCsrfTokenH2Test`. **Partly**: BUG-3, the SmartClient check is inert. Spring's CSRF is off by design (`SpringSecurityConfig`) |
| Security headers (`web.xml` CSP and X-Frame-Options) | `ResponseHeaderFilter`, `web.xml` `SecurityHeadersFilter` | `SecurityHeadersWebXmlTest` (the mapping) | + `SecurityHeadersContentTest` (the headers a response carries; CSP upgrades only over TLS). **Covered**; HSTS is off in `web.xml` and Spring by design |

## Implementation plan

Tests only, each in the package of the class it guards, extending `AbstractH2Test` where the database is used (`docs/ai/test-patterns.md`).

| Test | Module path | What it guards |
| --- | --- | --- |
| `SignOutOtherSessionsTest` | `skyve-war/src/test/java/modules/admin/UserAccount/actions/` | Part 1 |
| `AccountLockoutH2Test`, `SecurityUserRows` | `.../org/skyve/impl/web/spring/` | lockout through `DaoAuthenticationProvider`, `jdbcUserDetailsManager` and `SecurityListener` on H2 |
| `SpringSecurityConfigAccessTest` | same | the real filter chain: paths, methods, session fixation, sign out |
| `UserPrivilegesH2Test` | `.../org/skyve/impl/metadata/repository/` | disabled, expired and role-changed users as the repository builds them |
| `PasswordResetTokenH2Test` | `.../org/skyve/impl/web/` | reset by token |
| `MakePasswordChangeSecurityH2Test` | `.../modules/admin/ChangePassword/actions/` | the user's own password change |
| `SelfRegistrationActivationExpiryH2Test` | `.../modules/admin/SelfRegistrationActivation/` | activation links |
| `UserPasswordAdminH2Test` | `.../modules/admin/User/` | the administrator's password paths |
| `AdminSecurityPrivilegesH2Test` | `.../modules/admin/` | who holds the sensitive privileges |
| `SecurityHeadersContentTest` | `.../org/skyve/impl/web/filter/` | the response headers |
| `SmartClientCsrfTokenH2Test` | `.../org/skyve/impl/web/service/smartclient/` | the SmartClient CSRF check |
| `SelfServiceApplyH2Test` | same | self-service roles cannot reach `admin.User`; the apply is view-bound |
| `RestServiceSelfEscalationH2Test` | `.../org/skyve/impl/web/service/rest/` | REST blocked by default; BUG-4 |

## Acceptance checklist

- [x] `SignOutOtherSessionsTest` exists; the failing generated test is no longer generated; it and the generated `SignOutEverywhereTest` pass.
- [x] Every workflow in the brief is in the inventory with its code, tests before and after, and a rating.
- [x] New tests added for the highest-risk gaps; each guards behaviour the code has.
- [x] Product changes only as decided for BUG-1 and BUG-2 (plus warnings for BUG-3 and BUG-4).
- [x] Each defect has a test naming it: BUG-1 and BUG-2 enabled and passing after their fixes; BUG-3 and BUG-4 `@Disabled` (deferred and not fixed).
- [x] Red/green shown for each new guard.
- [x] Gates: see "AI validation results".

## Better engineering notes

Defects found:

- **BUG-1 (fixed).** `MakePasswordChange` skipped the old password check when `oldPassword` was null, so a signed-in session could change the password by omitting it. Fix: `WebUtil.resetPassword` marks the call as a reset by token in the persistence stash (`AppConstants.PASSWORD_RESET_STASH_KEY`), chosen over a new action or a signature change because it needs no metadata change and `UserBizlet` already uses stash flags; the action consumes the flag, and `WebUtil` also removes it in a `finally` so it cannot outlive the call. Every other call must give a right old password, or fails with the existing `oldPassword` message. The view's `required="true"` stays; `ChangePassword.xml`'s comment is updated. Test: `MakePasswordChangeSecurityH2Test.aSignedInUserCannotChangeThePasswordWithoutGivingTheOldOne` (null and blank), `PasswordResetTokenH2Test` (a token reset still works with no old password; no flag left behind).
- **BUG-2 (fixed).** `WebUtil.resetPassword` threw `SecurityException` for a disabled user's token. It now answers `exception.passwordResetTokenExpired`; the null guard stays because `retrieveUser` is nullable by contract. Test: `PasswordResetTokenH2Test.aDisabledUsersTokenIsAnsweredLikeAnExpiredOneAndThePasswordIsUnchanged`.
- **BUG-3 (known, deferred by Mike, 2026-11-19).** `SmartClientListServlet.checkCsrfToken` is a no-op (body commented out with a TODO about asynchronous problems), so SmartClient add, update, remove and edit requests are not CSRF checked. Tests stay `@Disabled`. Noted in `docs/skyve-10-changes.md` ("Known Security Notes").
- **BUG-4 (known, not fixed: Mike's decision (a), 2026-11-19).** `RestService.insertJSON`, `updateJSON` and `deleteJSON` check no document privilege and update copies every attribute, so where REST is enabled (`ForbiddenFilter` replaced by an authenticating filter) an `AppUser` can add `SecurityAdministrator` to their own `User`. Confirmed by the `@Disabled` `RestServiceSelfEscalationH2Test` test. The two passing tests there pin that `/rest/*` is `ForbiddenFilter` and answers 403. Warnings added in the `web.xml` comment beside the `RestFilter` mapping, the `RestService` Javadoc, `docs/architecture-design.md` (payload shaping) and `docs/skyve-10-changes.md`.

Observations that are design questions, not bugs, so no test pins them:

- A role or group change does not end the user's running sessions (`UserBizlet.postSave` signs out only for a password change, a rename and a disable), so revoked roles apply at the next sign in.
- Disabled means `inactive` or not `activated`; the activation link of a user an administrator has switched to not `activated` re-enables them while the link is within its expiry (or at any time when no expiry is configured).
- `SwapCustomer` mutates the session user's customer; it is held by DevOps only (guarded now).
- **Self-service role escalation (checked at the web layers).** An `AppUser` holds `admin.User` `_RU_U` (update, own record), and `persistence.save` enforces no document privilege (scratch check: a persistence user with only `AppUser` saved their own `User` with an added `SecurityAdministrator` role). So the web layers are the defence. Per route:
  - *SmartClient `smartedit` and PrimeFaces edit:* both call `EXT.checkAccess(user, UserAccess.singular(...))` before the document is opened; `AppUser` and `ViewUser` are refused for `admin.User`, `UserProxy`, `UserRole` and `Group` in every UX/UI (`SelfServiceApplyH2Test`). A `BasicUser` can open `admin.User` (it has read) but holds no update (`SaveAction` and the SmartClient servlet check `canUpdateDocument`; asserted).
  - *View-bound apply:* the SmartClient apply sets only the bindings of the view being edited. The one view with a user bean an `AppUser` reaches is the Profile (`UserDashboard`), whose bindings are name, email, mobile, image and a disabled user name; a hostile payload with `userName`, `password`, `inactive`, `passwordExpired`, `roles` and `groups` changes none of them (`SelfServiceApplyH2Test`, red when the apply is made to take any posted key). PrimeFaces binds only the components of the view.
  - *REST:* blocked by default (`RestFilter` is `ForbiddenFilter` on `/rest/*`, guarded by `RestServiceSelfEscalationH2Test`). **Behind the block it is open: BUG-4.**
  - Other save routes (jobs, `ServerSideAction`s) run application code, not user-supplied bindings.
- `admin.User` can be opened (`canAccess`) by BasicUser, DevOps, JobMaintainer and SAILRecorder as well as SecurityAdministrator (read privilege); that is how the roles are written.

Left uncovered, in priority order:

1. `Register.execute` (captcha, GeoIP silent pass, no self-registration group, breached password).
2. A browser test of a locked account (the page says only "invalid", as it should); a disposable-server IT for BUG-1 once fixed.
3. The session cookie flags in `SkyveContextListener` and the 30 minute `session-timeout`.
4. A sign in to a customer the user does not belong to (multi-tenant).
5. `ResendActivation` sending path (only the null cases are unit tested).
6. Servlet-level proof that a self-service role cannot change its own roles (above).
7. Purging expired `ADM_UserToken` rows (out of scope of the earlier plans too).

## AI validation plan

1. `mvn -o -B -pl skyve-war -am clean compile` then `skyve:generateDomain` in `skyve-war`: no `UserAccount/actions/SignOutOtherSessionsTest.java` is generated.
2. Each new test class alone, then three runs of the ones that touch the shared `Configuration` row.
3. Red/green: break the behaviour, run, see the tests fail, revert.
4. `mvn -o -B -pl skyve-war -am package` on JDK 21 (`/Users/mike/jdk-21.0.2.jdk/Contents/Home`). Never `mvn install`.
5. JDT 3.48 over the new files; Spotless in a scratch git copy with `-DspotlessFiles`; local Sonar (`skyve-sonar`, started and stopped for this).

## AI validation results

- **Part 1.** Red: the generated `UserAccount` `SignOutOtherSessionsTest` failed with "No request is available". Green: with the hand-written test, `generateDomain` (run from `skyve-war`) no longer generates it, and it and the generated `SignOutEverywhereTest` pass.
- **Tests.** Every new class passes; the Part 2 classes hold 70 tests, 3 `@Disabled` (BUG-3 twice, BUG-4 once). `SelfRegistrationActivationExpiryH2Test` was stable over three runs (it first flaked when the singleton configuration cache was bypassed by raw SQL; it now saves through persistence).
- **Red/green** (product broken temporarily, tests failed, reverted): lockout ignored when loading the user; the listener counting nothing toward lockout; the inactive check removed in `populateUser`; reset token never expiring; reset token not cleared; old password check removed (before the BUG-1 fix) and made optional again after it; the BUG-2 catch broken; session fixation off; POST open to anonymous users; `X-Frame-Options` changed; activation expiry off; `canAccess` always true; the SmartClient apply taking any posted key. The two lockout breaks were applied together.
- **Gate 1.** `mvn -o -B -pl skyve-war -am package` on JDK 21, serial: BUILD SUCCESS with the fixes (skyve-core 11622, skyve-ext 3483, skyve-web 3429, skyve-tools 26, skyve-maven-plugin 111, skyve-war 5207 tests, 0 failures, 44 skipped), and again after the JDT, Spotless and Sonar fixes (see the final result in the report). Never `install`.
- **JDT 3.48** (IDE compiler with the repo prefs, over every changed file): warnings on changed lines fixed (a nullable `User` into a `@Nonnull` parameter in `WebUtil`, boxing, `static-method`, unused import, resource fields, a deprecated encoder, a hiding local). Remaining warnings are on lines this branch did not change (deprecated `URL(String)` and nullness in `RestService`), and the repository's `@SuppressWarnings("null")` idiom note in one test.
- **Spotless** (scratch rsync copy, `ratchetFrom` removed, `-DspotlessFiles`; the pattern proved because the check first reported violations): 14 files needed formatting, applied to those files only; now clean. `MakePasswordChange` and `MakePasswordChangeTest`, which were not Spotless-clean before, were reformatted whole by it.
- **Local Sonar** (`skyve-sonar`, project `skyve-local-security-tests`, scan limited to the changed files): 8 issues at first (`java:S1488`, `S5785`, `S1130` x2, `S5778` x3, `S9016`), all fixed; 0 open after the rescan. The container was already running when this work reached it, so it was used and not stopped.
- **Not run:** JaCoCo; SAIL ITs and a disposable server (no browser-level gap needed one; the existing SAIL suites exercise sign in and change of password through the view, which now relies on the old password field being filled); SQL Server, MySQL and PostgreSQL (the new SQL is standard); a Faces postback as an `AppUser` (the access check and `SaveAction`'s `canUpdateDocument` are shared with the SmartClient route that was driven).

## User validation suggestions

- Un-disable `aTokenTheSessionWasNeverIssuedIsRefused` (BUG-3) or `anAppUserCannotGiveThemselfARoleThroughTheRestUpdate` (BUG-4) and watch it fail.
- Reset a password through the emailed link, and change one signed in (old password required) in both renderers.
- Decide whether a role or group change should end running sessions, and whether an administrator's "not activated" should survive the activation link.
