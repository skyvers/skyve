# Theme Resolution

Skyve resolves one UX/UI before a Faces view is processed and resolves its Faces theme lazily when
rendering needs it. Device preview is stored
in the HTTP session, so navigation, postbacks, popups, iframes, and background requests share the
same selection without URL propagation. This guide describes the lifecycle and upgrade contract
implemented for Skyve 10, including theme configuration, template migration, and
deployed-container validation.

## Terminology

| Term | Meaning |
|---|---|
| Physical user agent | The `UserAgentType` detected from the HTTP `User-Agent` header, with the existing touch-cookie adjustment. |
| Emulated user agent | A validated `UserAgentType` held in the HTTP session while device preview is active. |
| Request selection | One immutable `RequestUxUiSelection` containing the effective user-agent type, emulation flag, and application-owned `UxUi`. |
| Selected `UxUi` | The application-owned profile returned by the configured `UxUiSelector`. It controls routing, rendering applicability, and UX/UI-sensitive access checks. |
| Faces theme state | The request-scoped Faces values derived from the selected `UxUi`, including the effective template, colour and scheme values, iframe resource family, and layout class fragment. Configured component-theme and SmartClient values remain on the selected `UxUi`. |
| Layout family | The Facelets template family, such as Editorial, External, Ecuador, Ultima, or Diamond. It is distinct from the PrimeFaces component theme. |
| Component theme | The exact PrimeFaces resource-library name exposed as `_skyveTheme.componentThemeName`. It is not inferred from the layout colour. |
| SmartClient skin | The SmartClient presentation skin on the selected `UxUi`. A SmartClient profile may still resolve a Faces fallback for iframe pages. |

## Developer Contract

Selection is fixed for the lifetime of a request:

1. A valid device-preview value in the HTTP session invokes `UxUiSelector.emulate()` and completes selection.
2. Without emulation, Skyve evaluates the effective router's ordered `<direct/>` declarations using
   the current context-relative servlet target and validated physical device type.
3. A matching declaration is resolved by trusted name through `UxUiSelector.resolve(String)` and
   completes selection.
4. When no declaration matches, Skyve invokes `UxUiSelector.select()` for the application's
   public-user, role, session, and device policy.

Query parameters and fragments do not participate in direct matching, and a forwarded
request uses its current target rather than its original source. Request parameters never name an
arbitrary UX/UI profile. The selector owns the configured `UxUi` objects; named resolution returns
those exact objects rather than constructing replacements.

For a Faces request, `SkyveFacesPhaseListener` completes selection after `RESTORE_VIEW` and before
Skyve restores a conversation or starts persistence. Theme expressions resolve and request-cache the
Faces theme on first use. Non-Faces service requests call `UserAgent.getSelection(request)` and do
not acquire a resolved Faces theme.

## Selection Flow

```text
REQUEST / FORWARD / ASYNC
        |
        v
read optional validated preview type from the HTTP session
        |
        v
UserAgent.getSelection()
  -> valid emulation -> selector.emulate(type, request)
  -> otherwise detect physical device type
  -> first matching ordered router <direct/> -> selector.resolve(name)
  -> selector.select(type, request) when unmatched
        |
        v
SkyveFacesPhaseListener after RESTORE_VIEW
  -> RequestUxUiSelection
  -> record or validate selection marker
        |
        v
Facelets construction and primefaces.THEME
  -> FacesThemeView lazily derives and caches Faces rendering values
        |
        +-- render the selected template and resources
        |
        v
navigation, iframe, or UX/UI-sensitive service request
```

`UserAgent.detectType(request)` reports and request-caches the effective device in a partial
`RequestUxUiSelection` without selecting a UX/UI. `UserAgent.getSelection(request)` progressively
completes that same object with the selected UX/UI on first use. The request-scoped
`FacesThemeView` lazily derives and caches Faces rendering values from that selection.

## Direct Requests

For a direct XHTML request, `preRender()` and `preRenderView` run too late to choose the UX/UI:
Facelets may already have evaluated its template and includes. Declare direct request targets in
global or module `router.xml` metadata instead. Skyve evaluates this servlet/router contract before
the configured selector's request-dependent fallback policy; it does not depend on managed-bean
construction or Mojarra APIs, and the metadata is not limited to view resources.

A global router can combine a broad direct-request family with an exact exception. Declaration order
is authoritative, so put the specific rule before the overlapping prefix:

```xml
<router uxuiSelectorClassName="router.DefaultUxUiSelector"
		xmlns="http://www.skyve.org/xml/router">
	<uxui name="specialWelcome">
		<route outcome="/special/welcome.xhtml" />
	</uxui>
	<uxui name="special">
		<route outcome="/special/home.xhtml" />
	</uxui>

	<direct path="/special/welcome.xhtml" uxui="specialWelcome" />
	<direct path="/special/" match="prefix" uxui="special" />
</router>
```

Module-router declarations are prepended to global declarations. A later module merge precedes an
earlier module merge, while order within each file is retained. Duplicate and overlapping rules
are valid; the first effective declaration whose path and optional conditions all match wins.
Skyve does not promote exact matches or sort prefixes by length, so a module prefix may
intentionally mask a global exact rule.

Physical-device-dependent direct selection uses ordered declarations with an optional device
condition and an unconditional fallback:

```xml
<direct path="/public/register.xhtml" uxui="publicPhone" userAgentType="phone" />
<direct path="/public/register.xhtml" uxui="publicDesktopTablet" />
```

Every target name must also be an effective router `<uxui>` definition. The configured selector
resolves that trusted name through its application-owned registry:

```java
private static final Map<String, UxUi> UXUIS = Map.of(
		UxUis.PHONE.getName(), UxUis.PHONE,
		UxUis.EXTERNAL.getName(), UxUis.EXTERNAL,
		UxUis.STARTUP.getName(), UxUis.STARTUP);

@Override
public UxUi resolve(String name) {
	UxUi result = UXUIS.get(name);
	if (result == null) {
		throw new MetaDataException("Unknown configured UX/UI name " + name + '.');
	}
	return result;
}
```

The default `resolve(String)` implementation fails with `MetaDataException`. Applications need to
override it only when their effective router contains direct declarations. An unknown trusted metadata
name is a configuration error and never falls through to `select()`. Selector methods must contain
no servlet-path matching.

Paths begin with `/`. Prefix paths also end with `/`, preventing partial path-segment matches.
Query or fragment content and non-context-relative paths are rejected during metadata validation.
The single framework-owned rule applies consistently to normal initial GET, postback, Ajax, and
forward requests. Emulated requests complete selection through `UxUiSelector.emulate()` before
direct matching.

Do not accept a query parameter or request attribute as a `UxUi` name. Add the route to the
router metadata and the object to the selector's trusted registry instead.

## Downstream Descriptor Migration

Downstream WARs must retain the existing async-capable `SkyveFacesFilter` mapping:

```xml
<filter-mapping>
	<filter-name>SkyveFacesFilter</filter-name>
	<servlet-name>FacesServlet</servlet-name>
</filter-mapping>

```

Remove any `FacesThemeSelectionFilter` declaration or mapping. Retain async support on
`FacesServlet` and `SkyveFacesFilter`; `<async-supported>` belongs on the filter or servlet
declaration, not on `<filter-mapping>`. Configure PrimeFaces from the shared request-scoped theme
facade:

```xml
<context-param>
	<param-name>primefaces.THEME</param-name>
	<param-value>#{_skyveTheme.componentThemeName}</param-value>
</context-param>
```

Do not keep an independent component-theme fallback in `web.xml`.

## Request Lifecycle

`RequestUxUiSelection` contains the validated `UserAgentType`, emulation flag, and selected `UxUi`.
`UserAgent` stores one framework-private `RequestUxUiSelection` attribute: `detectType(request)`
constructs it with effective device fields, while `getSelection(request)` lazily fills its UX/UI
field and returns that same identity for the remainder of the request. Request-scoped
`FacesThemeView` derives its Faces-specific values once on first access. `_skyveTheme` reads
configured values directly from the selected `UxUi` and exposes the cached derived values. No
servlet filter eagerly resolves or wraps the request.

## Postback Validation

An initial Faces view stores one serializable selection marker containing:

- the emulation flag and `UserAgentType`;
- the selected UX/UI name.

On normal and PrimeFaces Ajax postbacks, `SkyveFacesPhaseListener` validates this marker before
it looks up a cached view or conversation, resolves a managed bean, begins persistence, processes
the principal, updates a model, performs an access check, or invokes an action. A missing marker or
different selection field expires the view. A different `UxUi` instance with the same name is
compatible; theme changes do not expire an existing view.

Changing device emulation or UX/UI name therefore requires a fresh GET that builds and marks a new
component tree. Theme configuration can change without invalidating a postback.

Views created before this lifecycle marker existed intentionally expire on their first
post-upgrade postback. Users can lose unsaved browser state and recover by loading the page again.
Before rolling out this change, drain existing Faces sessions or intentionally expire old views;
use a maintenance window or an equivalent session-drain procedure when unsaved work matters. Do
not attempt to reconstruct a marker from request parameters.

## Removed Request-Selection APIs

The request-selection contract has no compatibility layer. The following surfaces have been
removed:

- `UserAgent.getType(HttpServletRequest)`;
- `UserAgent.getUxUi(HttpServletRequest)`;
- `UserAgent.isEmulated(HttpServletRequest)`;
- `UserAgent.freezeSelection(HttpServletRequest)`;
- public UX/UI and user-agent request-attribute constants;
- client-supplied `skyveUxUi` values;
- `FacesView.setUxUi(UxUi)`;
- `FacesView.setUserAgentType(UserAgentType)`;
- the read-only `FacesView` request-selection and theme compatibility getters;
- the interim `UxUiViewPathMappings` Java builder API;
- the `SetUxUi` component; and
- `<s:setUxUi>`.

A `FacesView` extension must remove selection from `preRender()` and register its XHTML target as
router metadata shown above. General server code reads `UserAgent.getSelection(request)`. Faces
rendering code injects `_skyveTheme`.

Adding the default `UxUiSelector.resolve(String)` method is source-compatible for selectors whose
effective router has no direct declarations. Removing the interim mapping API is a source
break for applications that adopted it. All applications must recompile against Skyve 10 after
applying the router and selector migration; Skyve makes no binary-compatibility guarantee for this
transition.

## Mode Switch

`HarnessView.setUxUi(String)` is different from the removed request-theme setters. It is an
authorized session-mode action: it checks `User.canSwitchMode()`, updates or clears the session
preference, and does not mutate the current request. Built-in controls reload after the
action so the selector applies the preference to a new request. A mode switch that does not reload
will continue to display the current request's selection.

## Device Emulation

`_ua` is a one-time command accepted by `device.jsp`, not persistent navigation state. Exactly one
normalized `UserAgentType` name enables or changes preview; one blank value ends preview. Invalid
or duplicate values receive HTTP 400, while a missing parameter leaves the session unchanged.

The selected enum is stored in the HTTP session. This intentionally makes casual demonstration
preview shared by tabs using the same session. It has no independent timeout: logout or normal
session expiry removes it with the session. The session preference used by
`HarnessView.setUxUi(String)` remains a separate mode-switch feature.

`DeviceView` builds a direct `/device.jsp` command URL from the multi-valued request parameter map.
It removes all incoming `_ua` values, UTF-8 encodes every retained name and value, preserves
duplicate and empty values, and appends the selected `_ua` command. `device.jsp` consumes it and
responds with 303 See Other to the application context root without `_ua`; the welcome-page mapping
invokes `home.jsp` internally, so the preview iframe exposes only the final clean URL. Decoded
request data is never concatenated into its URL or JavaScript string.

## URL Handling

There is no emulation URL transformer. Ordinary action, bookmarkable, redirect, history, popup,
iframe, content, SmartClient, and background-service URLs contain no `_ua` state. All requests in
the preview session observe the session value through `UserAgent.getSelection(request)`.

### Routing and login redirects

When `device.jsp` consumes a preview command, it discards every `_ua` value, preserves every value
of every other parameter, UTF-8 encodes parameter names and values, and redirects to the application
context root before routing or login processing. Never concatenate decoded request parameters into
a URL.

## Sensitive Endpoints

Any endpoint that calls `UserAgent.getSelection(request)` is emulation-sensitive because
applicability or access may depend on the selected profile. The browser sends the same session to
built-in endpoints, so their ordinary URLs need no emulation parameter:

| Target family | Session-selection contract |
|---|---|
| `/dynamic.png`, `/dynamic.gif`, `/dynamic.jpeg`, `/report/*`, `/export/*` | The endpoint reads the session-backed request selection and retains its normal access checks. |
| `/meta`, `/resources`, `/images/resources`, `/content` | Authenticated requests use the same session selection; no endpoint accepts a UX/UI name. |
| `/smartlist`, `/smartedit`, `/smartsearch`, `/smartcomplete` | Data-source requests use their ordinary URLs and browser session. |
| `/smartgen`, `/smartsnap`, `/smarttag`, `/map`, `/chart` | RPC requests use their ordinary URLs and browser session. |
| Faces background loads, Upload, action Upload, and Biz Import | Popup, iframe, and background requests use their ordinary URLs and browser session. |

The unassembled `src/js/react/View.js` prototype is not a built-in runtime producer. Logout,
login-expiry, deliberately external destinations, and downloads whose targets do not consult UX/UI
applicability remain reviewed exclusions.

## Security Contract

`_ua` is an untrusted preview command, not an authorization credential. Skyve accepts
only one valid `UserAgentType` name, stores the enum before redirecting, and lets the configured selector
choose an application-owned `UxUi`. It never accepts a caller-provided UX/UI name.

Emulation changes the applicability profile used by the ordinary access chain; it does not grant a
role, create a privilege, or bypass `User.canAccess(...)` or `EXT.checkAccess(...)`. A resource
denied for the emulated profile remains denied. Preview is session-scoped, so simultaneous tabs in
the same browser session intentionally share the selected profile.

## Custom Template Resources

The shared PrimeFaces iframe template selects one complete resource family from the resolved
`_skyveTheme.iframeResourceTemplateName`. A custom full-page family can also style Upload, Biz
Import, and Image Markup iframe pages by providing both of these files:

```text
WEB-INF/pages/templates/{family}/head-resources.xhtml
WEB-INF/pages/templates/{family}/body-resources.xhtml
```

Both includes receive `bean`, the active page bean, and `iframe`, which is `true` for the shared
iframe template and `false` for a built-in full-page template. The head resource is evaluated
inside `h:head`. The body resource is evaluated at the established body-resource position; in the
shared iframe it remains before page-specific head overrides so existing override precedence is
preserved.

Skyve selects a custom family for iframe resources only when both files exist. If neither file or
only one file exists, it selects the complete generic `iframe` pair so family-specific and generic
halves are never mixed. The custom full-page template remains selected independently.

No client-emulation bootstrap is required because preview state is read from the HTTP session.

Faces tools opened from a SmartClient UX/UI, including Upload, Biz Import, and Image Markup,
remain light when the browser or operating system prefers dark mode. This deliberately matches the
light SmartClient desktop. PrimeFaces UX/UIs continue to follow their configured forced-dark value
or the browser preference.

## UX/UI Theme Configuration

Declare the layout family, exact PrimeFaces component-theme resource, and layout colour through
the existing flat `UxUi` factories:

```java
import org.skyve.metadata.router.UxUi;

UxUi editorial = UxUi.newPrimeFaces("phone", "editorial", "skyve", "blue");
UxUi brandedEditorial = UxUi.newPrimeFaces(
		"phone", "editorial", "customer-prime-theme", "mybrand");
UxUi ultima = UxUi.newPrimeFaces(
		"tablet", "ultima", "ultima-indigo", "indigo");
UxUi diamond = UxUi.newPrimeFaces(
		"tablet", "diamond", "diamond-indigo-light", "indigo-light");
UxUi custom = UxUi.newPrimeFaces(
		"public", "customer-template", "customer-prime-theme", "brand-ocean");
```

For Editorial, configure `skyve` for normal colours and `skyve-dark` when the layout colour is
`dark`. Diamond keeps its base colour and scheme in the historical `<base>-<scheme>` colour token,
for example `indigo-light`. The resolver derives the bundled Diamond and Ultima layout classes from
those established colour conventions. Custom family values pass through without family-specific
interpretation.

SmartClient profiles must declare the PrimeFaces theme used by Upload, Biz Import, Image Markup,
and other iframe pages explicitly:

```java
UxUi desktop = UxUi.newSmartClient(
		UxUi.DESKTOP_NAME, "Tahoe", "casablanca", "smartclient");
```

The resolver trims template and effective colour values and applies the established blue/indigo
fallbacks. Exact component-theme and SmartClient skin values retain their configured form.
Configure shared `UxUi` instances completely before serving requests: an active request keeps its
immutable resolved snapshot even if a shared configuration object is later mutated.

## Testing

Run the focused API, lifecycle, and assembled-WAR structure checks before deploying:

```bash
mvn -pl skyve-core -Dtest=UxUiTest test \
  -DskipIntegrationTests=true -DskipUnitTests=false
mvn -pl skyve-web -am test -DskipIntegrationTests=true -DskipUnitTests=false
mvn -pl skyve-war -am \
  -Dtest=ThemeFaceletsStructureTest,ThemeWebXmlStructureTest,ThemeRoutingStructureTest,UxUisTest \
  -Dsurefire.failIfNoSpecifiedTests=false test \
  -DskipIntegrationTests=true -DskipUnitTests=false
```

Deployed tests use the standard `*IT` Failsafe convention and the generic `deployed-it` profile.
First validate selection and redaction without a server:

```bash
SKYVE_DEPLOYED_IT_TEST=example.ReusableHarnessIT tools/deployed-it/run.sh --validate-only
```

The release owner-approved WildFly URL, checksum, and bundled Mojarra version are pinned in
`tools/deployed-it/runtime.properties`. Prepare that runtime and an isolated H2 configuration as
described in `tools/deployed-it/README.md`, then run the theme suite through the single entry point:

```bash
SKYVE_DEPLOYED_IT_TEST=org.skyve.impl.web.faces.ThemeResolutionLifecycleIT \
SKYVE_DEPLOYED_IT_RUNTIME_DIR=/absolute/path/to/wildfly \
SKYVE_DEPLOYED_IT_CONFIG_DIR=/absolute/path/to/isolated-test-config \
SKYVE_DEPLOYED_IT_BASE_URL=http://127.0.0.1:8080/skyve/ \
tools/deployed-it/run.sh
```

Results are written to
`skyve-war/target/deployed-it/org.skyve.impl.web.faces.ThemeResolutionLifecycleIT/results.md`;
Failsafe XML is under
`skyve-war/target/failsafe-reports`, and WildFly/browser diagnostics are retained beside the
result. `.github/workflows/deployed-it.yml` exposes the same runner through `workflow_call` and
`workflow_dispatch`; `.github/workflows/theme-resolution-deployed-it.yml` selects this suite.
The workflow uses ephemeral H2/bootstrap state without repository credentials. A future suite
that needs protected credentials must be limited to trusted events and cannot be a required
fork-pull-request check.

To add another deployed suite, place its test in `skyve-war/src/test/java` with an `IT` suffix,
reuse `util.deployed`, and keep feature vocabulary and hooks in its overlay adapter. Do not copy
the Maven profile, scripts, transport, or workflow. SAIL is a future consumer of this contract;
migrating existing SAIL suites is outside the Skyve 10 theme change.

The deployed suite observes only fixture-owned model updates and actions. It does not install a
production lifecycle observer. Focused `SkyveFacesPhaseListener` tests prove that incompatible
selection rejection precedes conversation, persistence, principal, model, and action processing.

## Troubleshooting

| Symptom | Check |
|---|---|
| Correct layout, wrong component styling | Confirm `primefaces.THEME` is `#{_skyveTheme.componentThemeName}` and that the exact resource name, not the layout colour, was configured. |
| Wrong template on a direct page | Declare its normalized servlet target in the effective router and return the configured object from `resolve(String)`. A `preRender()` change is too late. |
| Direct GET works but normal/Ajax postback expires | Ensure ordered router metadata selects the same configured object on every dispatch. Use a fresh GET after changing selection. |
| First post-upgrade postback expires | The view predates the selection marker. Reload with a fresh GET; drain old sessions before rollout when unsaved state matters. |
| Preview stops after login/session expiry | The preview state belonged to the expired HTTP session. Select a device again on `device.xhtml`. |
| Mode switch appears unchanged | `HarnessView.setUxUi(String)` changes the session preference for the next request; reload after the action. |
| Custom iframe has generic styling | Both custom `head-resources.xhtml` and `body-resources.xhtml` must exist. A missing half intentionally selects the complete generic pair. |
| Deployed test fails before assertions | Check the approved runtime pin/checksum, bundled Mojarra verification, Java 17, Chrome/driver availability, isolated configuration, readiness log, Failsafe XML, and redacted browser diagnostics. |

## Ordered Skyve 10 Upgrade Guide

1. Adopt the Skyve 10 dependency/version and assemble into a reviewable branch. Preserve custom
   WAR resources and descriptor changes before continuing. Recompile the application against
   Skyve 10; no binary-compatibility guarantee applies to this transition.
2. Inventory every directly addressed XHTML page and replace `preRender()` selection with fixed or
   physical-device-dependent ordered `<direct/>` declarations in global or module `router.xml`.
   Put specific declarations before broad prefixes where that precedence is required.
3. Remove calls to the two deleted `FacesView` setters and remove any `SetUxUi` or `<s:setUxUi>`
   reference. Remove all use of the interim Java mapping API. Implement strict
   `UxUiSelector.resolve(String)` over the application's configured registry when the effective
   router contains direct declarations, and remove path checks from `select()` and `emulate()`. Keep
   `HarnessView.setUxUi(String)` only for authorized next-request mode switching.
4. Remove any `FacesThemeSelectionFilter` declaration or mapping, retain async support on
   `SkyveFacesFilter`, and set `primefaces.THEME` to `#{_skyveTheme.componentThemeName}`.
5. Keep application profiles on the existing `UxUi` factories. Supply the exact component theme
   separately from the layout colour and give SmartClient profiles their iframe theme values.
6. Update custom full-page templates to consume `_skyveTheme` and supply both iframe resource files
   or neither. Remove any obsolete `client-emulation.xhtml` include.
7. Remove custom `_ua` propagation from navigation, popup, iframe, fetch, XHR,
   `RestDataSource.dataURL`, and `RPCManager.actionURL` producers.
8. Reconcile every custom endpoint that calls `UserAgent.getSelection(request)` with all of its browser
   producers and retain the normal applicability and authorization checks.
9. Use `device.xhtml` for the preview UI; its controls send one-time `/device.jsp?_ua=...` commands
   to the command-only JSP and provide an explicit end-preview action. Do not propagate `_ua`
   through application URLs.
10. Drain or intentionally expire pre-upgrade Faces views before moving traffic. Tell users that an
   old or mismatched postback can expire and that a fresh GET is the recovery path.
11. Run focused unit/structure tests, the root package build, and the checksum-pinned deployed
    lifecycle suite in CI. Verify direct GET, normal/Ajax postback, mode-switch reload, device
    preview, navigation, popup/iframe, and sensitive background requests in emulated and normal
    windows.
12. Roll out with the previous WAR/configuration available. Roll back both application code and
    descriptor/template changes together if selection, component-theme loading, or postback
    behavior regresses; expect views created by either incompatible version to require a fresh GET.

## Downstream Migration Status

The following ledger records the known 13 removed-setter calls across 12 classes. The owner,
target branch, and migration commit require coordination in each downstream repository. Every
entry remains pending; this repository has not edited those applications and does not claim that
they are compatible with the removed API.

| Repository | Class / calls | Required router metadata and selector resolution | Owner | Target branch | Migration commit | Status |
|---|---|---|---|---|---|---|
| Recruitment Register | `CasualTeacherView` (1) | Router exact `/casualTeacher.xhtml` to External; resolve External by identity | TBD | TBD | — | Pending |
| Skyve Foundry | `SelfRegistrationView` (1) | Router exact `/register.xhtml` to External; resolve External by identity | TBD | TBD | — | Pending |
| Skyve Foundry | `ProjectView` (1) | Router exact `/project.xhtml` to External; resolve External by identity | TBD | TBD | — | Pending |
| PIRSA FSP | `MeatApplicationView` (1) | Router exact `/meatApplication.xhtml` to FSP; resolve FSP by identity | TBD | TBD | — | Pending |
| RSAOL Rev | `RevView` (1) | Router prefix `/external/` to External; resolve External by identity and audit every XHTML using the base `rev` bean | TBD | TBD | — | Pending |
| RSAOL Rev | `NewUserView` (1) | Router prefix `/external/` to External; resolve External by identity | TBD | TBD | — | Pending |
| RSAOL Rev | `sdi.RegistrationView` (1) | Router prefix `/external/` to External; resolve External by identity | TBD | TBD | — | Pending |
| RSAOL Rev | `prt.CalculatorView` (1) | Router prefix `/external/` to External; resolve External by identity | TBD | TBD | — | Pending |
| RSAOL Rev | `ltp.LandTaxMindmapView` (1) | Router exact `/desktop/landTaxMindmap.xhtml` to External before broad prefixes; resolve External by identity | TBD | TBD | — | Pending |
| RSAOL Rev | `finance.TransactionMindmapView` (1) | Router prefix `/external/` to External; resolve External by identity | TBD | TBD | — | Pending |
| RSAOL Rev | `sdc.RegistrationView` (1) | Router prefix `/external/` to External; resolve External by identity | TBD | TBD | — | Pending |
| RSAOL RevBate | `RegisterView` (2) | Ordered exact declarations for `/public/register.xhtml` and `/public/phone/register.xhtml`, with `userAgentType` conditions where required; resolve the named existing public phone or desktop/tablet profiles by identity | TBD | TBD | — | Pending |
| PIRSA FSP | Commented `<s:setUxUi>` example | Remove the obsolete commented tag during adoption | TBD | TBD | — | Pending |
| DairySafe | Commented `<s:setUxUi>` example | Remove the obsolete commented tag during adoption | TBD | TBD | — | Pending |

The final two rows are not active calls, but remain in the ledger so the obsolete tag is not
copied back into use during later maintenance.

An application may be marked migrated only after its router declarations and strict trusted-name
resolution are implemented, selector path logic and late setter/commented tag references are
removed, it recompiles against Skyve 10, and direct GET plus normal/Ajax postback behavior passes
on its target branch.
