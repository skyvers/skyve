# Skyve 12 Changes

What is new in Skyve 12, and what an application has to do to take it.

**Upgrading?** Read [Upgrading: what breaks](#upgrading-what-breaks) first.

## Headlines

- **One CSRF token per session, for PrimeFaces and SmartClient.** The rotating tokens are
  gone. SmartClient changes (saves, deletes, actions, snapshots and tags) are now checked,
  which they were not before, and PrimeFaces postbacks are checked before any value is applied
  or action run. See [CSRF token](#csrf-token).

## Upgrading: what breaks

Work through this list. Each item links to the detail.

1. **Your own PrimeFaces pages need the token.** A page of yours that uses a Skyve template
   and posts back from a plain `<h:form>` must use `<s:csrfForm managedBean="...">` instead,
   or add the hidden input to the form. Otherwise its postbacks go to the expired page. See
   [CSRF token](#csrf-token).
2. **Your own SmartClient or HTTP clients must send `_csrf`** with every change they post to
   `smartedit`, `smartlist`, `smartsnap` or `smarttag`. Skyve's own clients (the SmartClient
   widgets, the Vue list grid and the Flutter client) already do.
3. **Java that used the old token API no longer compiles.** `CSRFTokenCacheConfig`,
   `UtilImpl.CSRF_TOKEN_CACHE` and the `StateUtil` token-set methods are gone, and
   `StateUtil.createToken()` returns a `String`. See [Java API](#java-api).
4. **Tidy `skyve.json`** (optional). The `state.csrfTokens` block is ignored and can be
   deleted. So can the old `tokens` disk store in the cache directory.

---

## Details

### CSRF token

Each HTTP session has one CSRF token: 128 random bits, kept in the session and never
rotated, so requests in flight together all carry a valid token. Signing in replaces it. A
missing or wrong token is refused and logged, and does not sign the user out. The design,
its scope and why Spring Security's own CSRF filter is not used are in
[Architecture: CSRF token](architecture-design.md#csrf-token).

**PrimeFaces.** `<s:csrfForm>` renders the token in a hidden `csrfToken` input, and the
templates' UX/UI form in `uxuicsrf`. A view that has rendered the token must send it with
every postback; one that does not is sent to the expired page. For a plain `<h:form>` on a
page of your own, add:

```xml
<input type="hidden" name="csrfToken" value="#{bean.csrfToken}" />
```

where `bean` is the page's Skyve view bean (any `FacesView`, or any other bean that extends
`LocalisableView`).

**Templates.** The templates' `uxuicsrf` input and `<s:csrfForm>` no longer auto-update
after each AJAX request, as the token never changes. Copies of the old templates keep
working; re-copying them only saves that update.

**SmartClient.** Every response carries the token in its `X-CSRF-TOKEN` header. Send it
back as the `_csrf` parameter with every change. Fetches and listings are not checked.

### Java API

- Removed: `org.skyve.cache.CSRFTokenCacheConfig`, `UtilImpl.CSRF_TOKEN_CACHE`, and
  `StateUtil.checkToken`, `replaceToken`, `removeToken`, `clearTokens` and
  `evictExpiredSessionTokens`. The Control Panel no longer lists a "CSRF Tokens" cache.
- Added: `StateUtil.getSessionToken(HttpSession)`, `checkSessionToken(HttpSession, String)`,
  `removeSessionToken(HttpSession)` and `isSameToken(String, String)`;
  `FacesUtil.getCsrfToken()`; `LocalisableView.getCsrfToken()`.
- Changed: `StateUtil.createToken()` returns a 32-digit hex `String`, not an `Integer`.
  `FacesView.setCsrfToken(String)` keeps nothing: the token is checked when the view is
  restored.
