# SAIL Browser Testing

SAIL (Skyve Application Integration Language) is an XML language for scripting
what a user does in a Skyve application: sign in, open menus and views, enter
data, press actions, and assert what the application shows. A SAIL script is
renderer-neutral. Its steps name bindings, actions and menu items from the
application's metadata, not HTML ids or CSS selectors.

The interpreted executors run a script in a real browser through Selenide,
against a running Skyve server. One executor drives PrimeFaces (PF) and one
drives SmartClient (SC). This guide covers writing scripts, the complete
op-code set and how each renderer supports it, and running the suites.

```xml
<automation uxui="tablet" userAgentType="tablet" testStrategy="Assert"
			xmlns="http://www.skyve.org/xml/sail">
	<before>
		<login user="admin" password="admin" />
		<testSuccess />
	</before>
	<interaction name="Contacts: create and find">
		<method>
			<navigateMenu module="admin" item="Contacts" />
			<listGridNew module="admin" document="Contact" />
			<dataEnter binding="name" value="SAIL contact {run}" />
			<save />
			<ok />
			<testGridRows module="admin" document="Contact" rowContaining="SAIL contact {run}" minRows="1" maxRows="1" />
		</method>
	</interaction>
	<after>
		<logout />
	</after>
</automation>
```

Runtime bugs that the suites found are listed in
[SAIL runtime issues](sail-runtime-issues.md).

## Status

- The language is complete. The schema is `skyve-war/src/main/java/schemas/sail.xsd`,
  with an identical copy in `skyve-core/src/test/resources/schemas/sail.xsd`.
- The PF executor, the PF kitchen sink suite and the PF admin suite are on the
  `sail` branch.
- The SC executor's support for the newer steps, the updated SC kitchen sink
  and the SC admin suite are on `sail-sc` and are not merged yet. Until then the
  SC executor on `sail`:
  - throws "not implemented yet" for `upload`, `testMessage`, `testGridRows`,
    `testWidget`, `navigateMenu`, `testMenu` and `switchMode`;
  - fails grid steps that use `rowContaining`;
  - still switches to SmartClient inside `<login/>`;
  - still has fixed polling sleeps in `SmartClientSelenide`.

  The SC column of the op-code reference below describes `sail-sc`.

## How the Interpreted Executors Work

A SAIL IT is a JUnit 5 test in `skyve-war/src/test/java/sail/**`. It reads a
script from the test classpath and runs it with an interpreted executor:

| Class | Role |
|---|---|
| `util.AbstractPrimeFacesInterpretedSail`, `util.AbstractSmartClientInterpretedSail` | Test bases. Start and stop the browser, unmarshal the script (`sailResource`, `sailFile`, `sailString`) and run it. |
| `util.sail.InterpretedWebDriverExecutor` | Shared executor base. Runs the automation, applies the test strategy, records failures, and implements login, logout, URL navigation, the context steps, `testSuccess`, `testFailure`, `comment`, `execute` and `pause`. |
| `util.sail.PrimeFacesInterpretedWebDriverExecutor`, `util.sail.SmartClientInterpretedWebDriverExecutor` | Renderer executors. They locate widgets and implement the remaining steps. |
| `util.sail.PrimeFacesSelenide`, `util.sail.SmartClientSelenide`, `util.sail.Selenide` | Browser gestures and waits. |
| `util.sail.BrowserConfiguration`, `util.sail.CrossBrowserSelenium`, `util.sail.Devices` | Browser, user agent, window size and Chrome options. |

### Metadata in the test JVM

The executors locate widgets from the application's metadata, so the test JVM
needs a running Skyve. The test bases extend `util.AbstractH2Test`, which boots
Skyve in the test JVM on an in-memory H2 database, as customer `bizhub`. This
local Skyve only provides metadata and builds views. The browser meanwhile
signs in to the remote server, usually as `admin` of customer `demo`.

The two can differ: customer overrides, data-driven combo values and bizIds
exist only on one side. Scripts should therefore find rows by their content
(`rowContaining`), not by bizId, and should not depend on values from the
server's data.

- **PrimeFaces.** For each edit or list context, the PF executor rebuilds the
  view's JSF component tree in the test JVM with a collecting component builder
  (`ComponentCollectingComponentBuilder`). This yields the client ids the server
  will generate, and the executor then finds elements by those ids. Before it
  clicks a button, it checks that the page shows the same label as the component
  it built. A drifted id therefore fails the step instead of clicking another
  button.
- **SmartClient.** The SC executor generates `sail:` locators: a JSON spec such
  as `{"view":"kitchensink.KitchenSink","kind":"item","name":"text"}`. A browser
  helper, `util/sail/smartclient-sail.js`, resolves each spec against the live
  view through Skyve's client object model and `isc.AutoTest`. No component
  counters or window indexes are derived in Java.

### The test user

Generated ids depend on what the user may see. A Save button that the user
cannot use is not rendered, and every later id shifts. The executor therefore
builds views as a user with every role of every module, as Skyve's bootstrap
user has (`util.sail.SailUsers.bootstrapLikeUser`). Scripts should sign in as
the bootstrap user, or as a user with the same permissions. A script can sign in
as a less privileged user, as `03-security-admin.xml` does. It must then keep to
navigation and assertions that the full-permission view does not affect, such as
menus, sign-in outcomes and screens where both users see the same thing.

### UX/UI and user agent

The router chooses the renderer from the browser's user agent. Desktop browsers
get PrimeFaces by default (`router.DefaultUxUiSelector`).

- The PF ITs set an iPad user agent (`Devices.ipad`), so the server uses the
  `tablet` UX/UI, and the scripts declare `uxui="tablet" userAgentType="tablet"`.
- The phone smoke test uses `Devices.iphone` with `uxui="phone"`.
- SC scripts declare `uxui="desktop" userAgentType="desktop"`. They follow
  `<login/>` with `<switchMode/>` to leave PrimeFaces.

The `uxui` attribute must name the UX/UI the server actually renders.
Otherwise the views built in the test JVM do not match the page.

## Writing Scripts

Scripts live beside the ITs that run them, under `skyve-war/src/test/java/sail/`.
The `skyve-war` pom copies `sail/**/*.xml` from there to the test classpath. A
`schemaLocation` pointing at `sail.xsd` gives editor completion and validation.
`sail.SailScriptsUnmarshalTest` unmarshals every shipped script, so a schema
error fails the unit build.

### Automation

`<automation>` is the root element.

| Attribute | Required | Meaning |
|---|---|---|
| `uxui` | yes | The UX/UI the script targets: `desktop` (SmartClient), or a PrimeFaces UX/UI such as `external`, `tablet` or `phone`. It must match what the server renders for the IT's user agent. `<switchMode/>` uses it to choose the renderer. |
| `userAgentType` | yes | `desktop`, `tablet`, `phone` or `other`. It selects the device variant of the views built in the test JVM. |
| `testStrategy` | no | `Assert`, `Verify` or `None`; see [Test Strategies](#test-strategies-and-failure-reports). If it is absent, the strategy is Assert. |

The children are an optional `<before>`, one or more `<interaction>` elements
and an optional `<after>`.

### Interactions and procedures

An `<interaction name="...">` is the unit of a script and appears in failure
messages. Its steps go in `<method>`. It can also have its own `<before>` and
`<after>`. Each of `<before>`, `<method>` and `<after>` takes any sequence of
the steps in the [op-code reference](#op-code-reference).

- The automation's `<before>` runs once, then each interaction, then the
  automation's `<after>`. A step that throws in the automation's `<before>` or
  `<after>` always stops the run, whatever the strategy.
- An interaction's own `<before>` steps are run as part of the interaction. Its
  `<after>` steps run after the method, even when the method failed under Verify.
- After an interaction that did not complete, the context stack is cleared, so
  the next interaction starts clean. Begin each interaction with a navigation
  step.

### Contexts

The executor keeps a stack of contexts. Each context names a module, a document
and a view type (`edit` or `create`), or a list. Steps that take a binding
resolve it against the top context. The navigation, zoom and New steps push
contexts, and OK, Cancel, Delete, Remove and ZoomOut pop them. Usually you never
manage contexts yourself.

The context steps are for when a script reaches a view in a way the executor
cannot follow, for example through `<navigateLink/>` or `<execute>`:

```xml
<navigateLink href="?a=e&amp;m=kitchensink&amp;d=ActionsFixture" />
<pushEditContext module="kitchensink" document="ActionsFixture" />
<!-- steps on ActionsFixture -->
<popContext />
```

`<clearContext/>` empties the stack.

### The run token

`{run}` in a value is replaced with a token unique to the run. The token is the
current time in milliseconds in base 36: 8 or more lower-case letters and
digits. A new token is made for each automation, which means once per script
file. In a multi-file suite, a value created with `{run}` in one file cannot be
found with `{run}` in the next file.

Use `{run}` to make the records a script creates unique, so a script can run
again, or alongside another script, without clashing with leftovers. It is
substituted in:

- `dataEnter@value` and `testValue@value`;
- `lookupDescriptionAutoComplete@search`;
- `login@user` and `login@password`, to sign in as a user created earlier in the
  same run;
- `testFailure@message` and `testMessage@contains`;
- every `rowContaining`.

It is not substituted in other attributes, such as `navigateEdit@bizId` or
`navigateLink@href`.

### Values

| Widget | `dataEnter` value | `testValue` value |
|---|---|---|
| Text, text area, password, masked, date, time, spinner, colour | The text a user would type. A newline becomes a space, because a typed newline would trip the form's default button. | The displayed text, trimmed. Colours ignore case. |
| Check box | `true` or `false` | `true`, `false`, or empty for unset (PF) |
| Combo, radio | The zero-based position of the option. For a combo, this counts the empty first entry when there is one. | The option's position, code or label |
| Slider | A number | The number |
| Rich text, HTML | Plain text | The plain text or the HTML |
| Lookup description | Not supported: use `lookupDescriptionAutoComplete` or `lookupDescriptionPick` | The displayed description |
| List membership | The `\|`-separated descriptions of the items to select, for example `Admin\|Basic`. Every other item is deselected. | The members, `\|`-separated |

An empty membership cannot be expressed, because a blank value counts as absent.
`testDataEnter` skips membership widgets.

## Op-code Reference

Every element in `sail.xsd`. "Required" attributes are marked with `*`. Some
combination rules cannot be expressed in the XSD. The step checks these when it
runs and fails with a `MetaDataException`, so a bad combination fails when the
step is reached, not when the script loads.

Support: **yes** means implemented; **no** means the step fails with a reason
(by design, when the renderer has no such UI).

### Session

| Element | Attributes | Semantics | PF | SC |
|---|---|---|---|---|
| `login` | `user`\*, `password`\*, `customer` | Signs in and waits for the next page. If a session is still signed in, it signs out first. A rejected sign in fails the step, unless the next step is `<testFailure/>` (which then asserts the rejection) or the strategy is None. `<testSuccess/>` straight after a login asserts that it succeeded. Does not switch renderers. | yes | yes |
| `logout` | | Signs out. | yes | yes |
| `switchMode` | none | Switches to the renderer the automation's `uxui` targets by clicking the user-visible Switch Mode control: `desktop` means SmartClient, and any other UX/UI means PrimeFaces. Does nothing if the target renderer is already showing. Fails if the control is missing (`User.canSwitchMode()` is false). | yes: to PF only, from SmartClient's header menu. Fails for `uxui="desktop"`. | yes: to SC only, through PF's user menu (avatar, then Switch mode). Fails for other UX/UIs. |

### Navigation

| Element | Attributes | Semantics | PF | SC |
|---|---|---|---|---|
| `navigateList` | `module`\*, `document`, `query`, `model` | Pushes a list context and opens `?a=l&m=<module>&q=<query or document>`, or `&d=<document>&q=<model>` for a model. Needs a query, or a document with an optional model. | yes | yes |
| `navigateEdit` | `module`\*, `document`\*, `bizId` | Pushes an edit context and opens `?a=e&m=…&d=…`. Without a `bizId`, it opens a new instance. | yes | yes |
| `navigateTree` | as `navigateList` | Opens `a=t`. | no: no PF tree page | URL only: waits for the view, with no tree gestures |
| `navigateMap` | as `navigateList`, plus `geometryBinding`, `refreshTimeInSeconds`, `showRefreshControls` | Opens `a=m` with `&b=<geometryBinding>`. `geometryBinding` is required unless a model is named. The two refresh attributes are accepted and ignored. | yes | URL only |
| `navigateCalendar` | as `navigateList`, plus `startBinding`, `endBinding` | Opens `a=c`. The bindings are accepted and ignored. | no: no PF calendar page | URL only |
| `navigateLink` | `href`\* | Opens an absolute URL as is, or otherwise a URL relative to the base URL. The top context, if any, loses its document and view type. Push a context afterwards if later steps need one. | yes | yes |
| `navigateMenu` | `module`\*, `item`\* | Clicks through the rendered menu, then sets the context as the equivalent navigate step would. `item` is the menu item's name, or its shown label. Use `/` to qualify it with group names, for example `Security Admin/Users`. | yes; tree and calendar items fail | yes; fails if a window covers the menu |
| `testMenu` | `module`\*, `item`, `present` | Asserts that the module, or an item of it, is shown (`present` true or absent) or not shown (`false`). `item` is named as for `navigateMenu`. | yes | yes |

### Contexts

| Element | Attributes | Semantics | PF | SC |
|---|---|---|---|---|
| `pushListContext` | `module`\*, `document`, `query`, `model`, `uxui`, `userAgentType` | Pushes a list context without touching the browser. | yes | yes |
| `pushEditContext` | `module`\*, `document`\*, `createView`, `uxui`, `userAgentType` | Pushes an edit context without touching the browser. Use `createView="true"` for a document whose create view differs from its edit view. | yes | yes |
| `popContext` | | Pops the top context. | yes | yes |
| `clearContext` | | Empties the context stack. | yes | yes |

`uxui` and `userAgentType` on the push steps default to the automation's.

### Data entry

| Element | Attributes | Semantics | PF | SC |
|---|---|---|---|---|
| `tabSelect` | `path`\* | Selects a tab by its title. | yes; title only, so nested tabs with the same title clash | yes; `Outer::Inner` selects each tab in turn |
| `dataEnter` | `binding`\*, `value`\*, `navigateToWidget` | Enters the value into the first displayed, enabled widget for the binding; see [Values](#values). Fails if no widget can take it. `navigateToWidget` is accepted and ignored: the executors always scroll the target into view. | yes | yes; after `dataGridNew`/`dataGridEdit` on an inline grid, bindings are tried as the grid's columns first |
| `testDataEnter` | `fixture` | Builds a bean with `DataBuilder` (the named fixture, or the `sail` fixture), then enters its values into every scalar widget of the view. Membership widgets are skipped. | yes | yes |
| `upload` | `binding` or `action` (exactly one), `file`\* | Uploads a file from the **test** classpath (for example `sail/files/sail-image.png` from `skyve-war/src/test/resources`). With `binding`, it uses the content widget's upload page, and the widget must hold content afterwards. With `action`, it presses the upload action's button, chooses the file and submits it. | yes | yes |
| `lookupDescriptionAutoComplete` | `binding`\*, `search`\* | Types the search text into the lookup and picks the first suggestion. Fails if nothing matches. | yes | yes |
| `lookupDescriptionPick` | `binding`\*, `row`\* | Opens the lookup's drop-down and picks the zero-based row. | yes | yes |
| `lookupDescriptionNew` | `binding`\* | Opens New on the lookup and pushes the related document's context. | no: PF lookups have no New | yes |
| `lookupDescriptionEdit` | `binding`\* | Opens Edit on the lookup and pushes the related document's context. | no: PF lookups have no Edit | yes |

### Actions

Every action step takes `testSuccess`. Unless it is `false`, the step checks
afterwards that no error is shown. Set it to `false` when the next step is a
`<testFailure/>`.

`confirm` answers the confirmation dialog and any server-side questions the
action raises, in turn. These are located by their role, not their caption.

- `confirm="true"` or absent answers positively (Yes, OK or Continue) if
  something asks. It passes if nothing asks.
- `confirm="false"` answers negatively (No or Cancel). It fails if nothing
  asks. A declined step keeps its context and makes no success check.

| Element | Attributes | Semantics | PF | SC |
|---|---|---|---|---|
| `action` | `name`\*, `confirm`, `testSuccess` | Presses the view's button for the action. | yes | yes; an implicit Upload action fails: use `<upload action="…"/>` |
| `save` | `confirm`, `createView`, `testSuccess` | Presses Save. `createView` sets the context's view type for what follows: `true` for create, `false` for edit. | yes | yes |
| `ok` | `confirm`, `testSuccess` | Presses OK, then pops the context. | yes | yes |
| `cancel` | `testSuccess` | Presses Cancel, then pops the context. | yes | yes |
| `delete` | `confirm`, `testSuccess` | Presses Delete, then pops the context. | yes | yes |
| `remove` | `confirm`, `testSuccess` | Presses Remove in a zoomed-in child view, then pops the context. | yes | yes |
| `zoomOut` | `confirm`, `testSuccess` | Presses Zoom Out, then pops the context. | yes | yes |
| `zoomIn` | `binding`\*, `testSuccess` | Presses the zoom-in button of an association, then pushes the related document's context. If the zoom is refused, for example by validation with `testSuccess="false"`, it stays in the view and pushes nothing. | yes | yes |

A view opened by URL (`navigateEdit`) has no Delete, OK or Cancel buttons:
Skyve renders it as the only view (`onlyView`), which is by design. To exercise
them, reach the view from a list (`listGridZoom`, `listGridNew`) or by zooming
in.

### Grids

Row steps take exactly one of `row` and `rowContaining`. `row` is zero-based.
`rowContaining` picks the first row, in the order the grid shows its rows,
that has a cell containing the text, compared case-sensitively. Matching sees
only the rows the browser has: the current page of a paged list. The list grid
steps identify the grid by `module`, `document`, `query` and `model`, and each
of these defaults from the current context.

| Element | Attributes | Semantics | PF | SC |
|---|---|---|---|---|
| `dataGridNew` | `binding`\* | Adds a row. For a grid that is not inline, this opens the row's view and pushes its context. | yes; inline grids add in place | yes; inline grids start an inline edit |
| `dataGridZoom` | `binding`\*, `row` or `rowContaining` | Zooms into the row and pushes its context. | yes | yes |
| `dataGridEdit` | `binding`\*, `row` or `rowContaining` | Starts an inline edit of the row. | no: PF grids have no row edit gesture; inline cells are inputs | yes; the following `dataEnter`, `testValue` and `testWidget` steps address the row's columns, and any other step ends the edit |
| `dataGridRemove` | `binding`\*, `row` or `rowContaining` | Removes the row from the collection. The record itself is not deleted. | yes | yes |
| `dataGridSelect` | `binding`\*, `row` or `rowContaining` | Selects the row without zooming. | only on grids with a `selectedIdBinding`; on other grids a selection zooms | yes |
| `listGridNew` | `module`, `document`, `query`, `model`, `createView` | Presses New and pushes the new document's context. | yes | yes |
| `listGridZoom` | as `listGridNew` without `createView`, plus `row` or `rowContaining` | Opens the row and pushes its context. | yes | yes |
| `listGridSelect` | as `listGridZoom` | Selects the row without opening it. | only on grids with a `selectedIdBinding` | yes |

### Assertions

Assertions follow the [test strategy](#test-strategies-and-failure-reports).
Under None, they are skipped.

| Element | Attributes | Semantics | PF | SC |
|---|---|---|---|---|
| `testValue` | `binding`\*, `value`\* | Asserts that the widget shows the value; see [Values](#values). | yes | yes |
| `testSuccess` | | Asserts that no error message, growl or field error is shown. Information messages do not count. Straight after a login, it asserts that the login succeeded. | yes | yes |
| `testFailure` | `message` | Asserts that an error is shown and, if `message` is given, that the error contains it. Straight after a login, it asserts that the login was rejected. | yes | yes; an unanswered question or a field error also counts |
| `testMessage` | `contains`\*, `type` (`info`, `warning`, `error`) | Asserts that a growl or message containing the text, case-sensitively, is shown. With `type`, the message must have that severity. Unlike `testSuccess`/`testFailure`, it sees information and warning messages too. | yes: global messages, growls and field messages | yes: `isc.say` is info, `isc.warn` is warning, the Problems dialog and field errors are error, and growls go by their class |
| `testGridRows` | `binding`, or `module`/`document`/`query`/`model`; `rowContaining`, `minRows`, `maxRows`, `timeoutMillis` | Counts the rows, or only those containing the text, and asserts `minRows ≤ count ≤ maxRows`. An absent `minRows` means 0 and an absent `maxRows` means no maximum. With neither, the step asserts at least one row. `maxRows="0"` asserts absence. `binding` (a data grid) cannot be combined with the list attributes. With `timeoutMillis`, a list grid is reloaded and recounted every second until the bounds hold or time runs out. Use it to wait for a server job. | yes | yes; data grids are not reloaded |
| `testWidget` | exactly one of `binding`, `action`, `tab`; `visible`, `enabled` | `visible="true"` asserts the target is shown. `visible="false"` asserts it is hidden or not rendered. `enabled` asserts it is shown and is, or is not, enabled. With neither, the step asserts visible. `visible="false"` cannot be combined with `enabled`. `tab` is a tab path as for `tabSelect`. | yes | yes |

`testMenu` is listed under [Navigation](#navigation).

### Other steps

| Element | Content or attributes | Semantics | PF | SC |
|---|---|---|---|---|
| `comment` | text | Written to the trace. Does nothing else. | yes | yes |
| `execute` | JavaScript text | Runs the script in the browser page. Use it sparingly: it bypasses what SAIL is testing. | yes; then waits for any request or page load the script started | yes; checks for open dialogs first, then waits for SmartClient to settle |
| `pause` | `millis`\* | Sleeps. Avoid it: see [Synchronisation Rules](#synchronisation-rules). | yes | yes |

## Test Strategies and Failure Reports

| Strategy | A failed assertion | A step that throws | At the end |
|---|---|---|---|
| `Assert` (or absent) | Stops the run | Stops the run | |
| `Verify` | Is recorded; the run continues with the next step | Is recorded; the run continues with the next interaction. The interaction's `<after>` still runs, and the context stack is cleared. | Throws if anything was recorded, listing every failure |
| `None` | Assertions are not made | Stops the run | |

Assertions are `testValue`, `testSuccess`, `testFailure`, `testMessage`,
`testGridRows`, `testWidget`, `testMenu` and the implicit success check after
actions. A step throws when it cannot do what it says: its widget is missing,
disabled or covered, a row is missing, nothing asks for a `confirm="false"`
step, or a wait times out. Steps the renderer does not support also throw.

Every failure is an `AssertionError`, so it fails the test. Its message names
the interaction, the step's number within it, the step as XML, the reason and
the page URL:

```text
Interaction "Contacts: create and find" step 4 <save />: expected no error messages after [save] but found: ... [at http://localhost:8180/skyve/?a=e&m=admin&d=Contact]
```

Under Verify, the message starts with `SAIL automation failed with N failure(s):`
and lists one failure per line. Nothing is only logged: a script never prints a
failure and then passes.

Two system properties help when a run fails:

- `-Dsail.testStrategy=Verify` overrides every script's strategy, so a single
  run lists all the failures.
- `-Dsail.failureDir=<dir>` saves a screenshot (`.png`) and the page source
  (`.html`) for each failure, named `NN-<interaction>-stepNN`.

`PrimeFacesSailTestStrategyIT` runs one script under each strategy against the
kitchen sink, and checks that each strategy behaves as this table says.

## Running the Tests

### A disposable server

SAIL ITs save, delete, back up, restore and truncate data. Run them only
against a server whose data you can throw away, and never against your
development server.

1. Build the war and the content add-in without running tests. `-DskipTests`
   still runs some skyve-war tests, so use `-Dmaven.test.skip=true`:

   ```bash
   mvn -pl skyve-war,skyve-content -am package -Dmaven.test.skip=true
   ```

2. Make a private WildFly standalone base: a new directory holding a copy of
   the `standalone/configuration` of a WildFly that is configured for Skyve.
   Start it with `-Djboss.server.base.dir=<base>` and a port offset
   (`-Djboss.socket.binding.port-offset=100` serves on 8180), so it cannot
   collide with your usual server on 8080. Point its `java:/SkyveDB` H2
   datasource at a new database file.
3. Deploy `skyve-war` with a `skyve.json` copied from `skyve-war/config/skyve.json`,
   changed as follows:
   - `content.directory`: a new, empty directory;
   - `url.server`: the private port;
   - `environment.accessControl`: `false`. The kitchen sink fixtures have no
     menu items, so access control answers "Forbidden" for them.
   - `environment.showSetup`: `false`. Otherwise admin's first sign in goes to
     the setup page.
   - `environment.jobScheduler`: `false`. The suites expect it off.
   - Keep `environment.identifier` non-null and the `bootstrap` section as it
     is (`admin`/`admin`, customer `demo`). The bootstrap user is not created
     in a production environment.
4. Copy the `skyve-content` add-in zip, built in step 1 under
   `skyve-content/target`, into `<content directory>/addins/`. The bootstrap
   user is created only after the content manager starts, which needs the
   add-in. Without it, every sign in fails.
5. Start the server and sign in once as `admin`/`admin` to check it.

To reset the server, stop it, delete the H2 database file and the content
directory (including backups), and start it again.

### Commands

Run the ITs through surefire with an explicit `-Dtest`. The root pom excludes
`**/*IT` from surefire, and an explicit `-Dtest` overrides that exclusion.
Failsafe is skipped by default (`skipIntegrationTests`).

```bash
mvn -pl skyve-war -am test \
	-Dtest='PrimeFacesKitchenSinkSailIT,PrimeFacesAdminSuiteSailIT' \
	-Dsurefire.failIfNoSpecifiedTests=false \
	-DforkCount=1 \
	-Dsail.baseUrl=http://localhost:8180/skyve/
```

Name the ITs explicitly. A pattern such as `*Sail*` also matches older JUnit 4
classes in `sail/admin/pf`, and it runs browser ITs where you meant to run unit
tests. The SAIL ITs are:

| IT | Script | Renderer |
|---|---|---|
| `PrimeFacesKitchenSinkSailIT` | `sail/kitchenSink/pf/pf-kitchensink-sail.xml` | PF, tablet |
| `PrimeFacesSailTestStrategyIT` | inline scripts, 3 tests | PF, tablet |
| `PrimeFacesAdminSuiteSailIT` | `sail/admin/pf/suite/*.xml` | PF, tablet |
| `PrimeFacesAdminSailIT` | `sail/admin/pf/pf-admin-sail.xml` (the older single-file admin script) | PF, tablet |
| `PrimeFacesAdminPhoneSailIT` | `sail/admin/pf/admin-phone-smoke.xml` | PF, phone |
| `SmartClientKitchenSinkSailIT` | `sail/kitchenSink/sc/sc-kitchensink-sail.xml` | SC |
| `SmartClientAdminSailIT` | `sail/admin/sc/sc-admin-sail.xml` | SC |
| `SmartClientAdminSuiteSailIT` (on `sail-sc`) | `sail/admin/sc/suite/*.xml` | SC |

To run the SAIL unit tests without any browser IT, exclude ITs and give a base
URL that nothing answers:

```bash
mvn -pl skyve-core,skyve-ext,skyve-web,skyve-war -am test \
	-Dtest='org/skyve/impl/sail/**/*Test,org/skyve/metadata/sail/**/*Test,org/skyve/impl/generate/sail/**/*Test,modules/admin/ControlPanel/actions/*SAIL*,sail/SailScriptsUnmarshalTest,!*IT' \
	-Dsurefire.failIfNoSpecifiedTests=false \
	-Dsail.baseUrl=http://localhost:1/skyve/
```

If your IDE compiles into `target/classes` while Maven runs (VS Code's Java
support does), run Maven in a copy of the checkout instead.

### System properties

| Property | Default | Meaning |
|---|---|---|
| `sail.baseUrl` | none (required) | The server's base URL, ending in `/`, for example `http://localhost:8180/skyve/` |
| `sail.headless` | `true` | `false` shows the browser window |
| `sail.timeoutSeconds` | `30` | The limit for every wait on the browser. It is a limit, not a delay: waits end as soon as their event happens. |
| `sail.testStrategy` | the script's | Overrides every script's strategy |
| `sail.failureDir` | none | Directory for failure screenshots and page sources |
| `sail.suite.only` | all files | Admin suites only: comma-separated file prefixes to run, for example `03,04` |
| `forkCount` | `2` (skyve-war pom) | Must be `1` for SAIL ITs |

**Why there is no default base URL.** SAIL scripts change data. Early in this
work, a run that did not name a server fell back to `http://localhost:8080/skyve/`
and ran an admin script against a developer's own server. That run changed the
admin contact and wrote sign-in and password history. `BrowserConfiguration.getBaseUrl()`
now throws if `-Dsail.baseUrl` is not set, so a run must name its disposable
server.

**Why `-DforkCount=1`.** The skyve-war pom sets `forkCount` to 2, so surefire
runs two test classes at once. SAIL ITs against one server then share the
server's data and its `admin` user. When the PF and SC admin suites ran
together, they failed with `StaleObjectStateException` on the admin user, and
one suite's backup or restore would undo the other's work. Run SAIL ITs
serially.

**Headless and headed.** Headless is the default. Headless Chrome is sized to
1920×1080, because its default of 800×600 squeezes the layouts. A headed window
(`-Dsail.headless=false`) keeps the size Chrome gives it, which on macOS can be
about 1000×576 CSS pixels, and it may not have the OS focus. These are realistic
conditions, so the harness does not hide them by enlarging the window. A suite
is considered green only when it passes repeatedly both headless and headed.
Chrome always runs with `--disable-backgrounding-occluded-windows`,
`--disable-renderer-backgrounding` and `--disable-background-timer-throttling`.
Without these flags, a locked screen or a window behind other apps stalls the
page's timers.

**Reruns.** The root pom hard-codes `rerunFailingTestsCount` to 2, and a value
written in the plugin configuration cannot be overridden with
`-Dsurefire.rerunFailingTestsCount=0`. A SAIL IT that fails is therefore run up
to twice more. If it passes on a rerun, surefire reports it as a flake and the
build passes. A rerun of an admin suite also repeats its destructive steps. When
you need a strict result, for example when proving a fix, change the pom
setting to a property in your local copy and pass
`-Dsurefire.rerunFailingTestsCount=0`.

## The Suites

### Kitchen sink: conformance

The kitchen sink scripts are the renderer conformance suites. They exercise
every op-code that the renderer supports against the `kitchensink` module's
fixtures, with `testStrategy="Assert"`.

`pf-kitchensink-sail.xml` has these interactions:

- sign in failure and success;
- lookup values created and found by `{run}` content;
- generated `testDataEnter`;
- `dataEnter` and `testValue` on every KitchenSink widget, with values checked
  again after server round trips;
- data grids: container, ordered and inline grids;
- ActionsFixture: toolbar, form and tab buttons, change events and uploads;
- DialogsFixture: confirmations and server-side questions, answered yes and no;
- EscapingFixture: zoom in and out;
- contexts, links, a map, and `execute`;
- menus;
- switch mode;
- sign out.

`sc-kitchensink-sail.xml` covers the same ground for SmartClient, except
membership. The only kitchen sink membership widget is on ConditionsFixture,
which never settles in SmartClient (runtime issue SC-4).

Both kitchen sinks make their records unique with `{run}`, so they can be rerun
without resetting the database.

### Admin: application suite

The admin suites test the admin module as an application, through its menus,
as an administrator would use it. The PF suite is `sail/admin/pf/suite/`, run by
`PrimeFacesAdminSuiteSailIT`:

| File | Covers |
|---|---|
| `00-backup.xml` | Precondition: no other backup exists. Backs up with no redaction, waits for the backup job, and leaves a "SAIL restore marker" data group. |
| `01-session.xml` | User Dashboard; sign in with a bad password, an unknown user and the right one; sign out and in. |
| `02-contacts-communications.xml` | Contact create, find, change and delete, with an image upload and a declined delete; Communication Templates; Communications: every tab, attachments, and the send actions' refusals. |
| `03-security-admin.xml` | Data Groups; Groups (roles through list membership); the User wizard; the new user signs in, sees a role-filtered menu and changes their password; Sign In History; Security Log; Configuration tabs; Mail Log. |
| `04-devops.xml` | Control Panel (every tab, a query, cache statistics, generated SAIL); Data Maintenance (the backup list, import/export); Document Creator; Monitoring Dashboard. |
| `05-reports-misc.xml` | Reports, Audits, System Dashboard, Document Numbers, Jobs. |
| `98-destructive.xml` | Configuration saved with safe values (`mailBogusSend` on, so mail is only logged); a mail send that is refused; generate, rerun and delete tagged test data; delete completed jobs (declined, then confirmed). |
| `99-restore.xml` | Restores the 00 backup (declined, then confirmed), signs in again, checks the marker has gone, and deletes the backup. |

**Ordering.** The IT runs the files in this order in one browser session. Each
file is its own automation and signs in for itself. The IT stops at the first
file that fails, because later files build on earlier ones. Dangerous actions
run in 98, after everything that depends on the data they change.

**The backup/restore bracket.** 00 backs up the data and 99 restores it, so the
suite leaves the server as it found it and can be run again. The IT runs 99
whenever 00 was among the files run, even when a later file failed. It then
reports both outcomes with `assertAll`.

**The no-stray-backup precondition.** The backups list shows the oldest backup
first (runtime issue P1), and 99 restores row 0. A backup left by anything
else would therefore be restored instead of the suite's own. 00 refreshes the
backup list and asserts `<testGridRows module="admin" query="BackupsModel" maxRows="0"/>`
before it backs up. If it fails, delete the stray backups on the server by hand.
The suite asserts rather than deleting them, because a test should not decide to
delete someone's backup. For the same reason, the older `pf-admin-sail.xml`
no longer makes a backup.

The admin suites must never lock `admin` out. They run no `testDataEnter` on
Configuration, change passwords only for the user they create, and back up with
data sensitivity "none". A redacted backup cannot be restored safely (runtime
issue P2).

The SC suite is `sail/admin/sc/suite/` on `sail-sc`, run by
`SmartClientAdminSuiteSailIT`. It mirrors the PF suite file by file and
interaction by interaction. It adds the menu items and actions that only
SmartClient has, among them Heap Dumps, Report Manager, Snapshots and Tags, and
the DropIndexing, TruncateAuditLog and Truncate actions. Truncate runs in 99,
just before the restore. Its precondition
also asserts that no restore marker is left over. If 00 fails before its
precondition has passed, the SC IT does not run 99, so it can never restore a
stray backup. The PF IT runs 99 whenever 00 ran.

With both suites following the precondition, and each deleting its own backup
in 99, the PF and SC suites can run one after the other in either order against
the same server.

## Synchronisation Rules

Timing problems show up as flakes, especially in SmartClient and in headed runs.
The harness follows three rules, and changes to it must keep them.

1. **No sleeps or fixed delays.** Every wait waits for a named asynchronous
   event: a page or ajax response, the PrimeFaces ajax queue emptying,
   SmartClient having no pending RPC or deferred timer, a widget being drawn, a
   dialog opening or closing, an animation ending, or an upload finishing. The
   one deliberate delay is SmartClient's double-click interval. Two clicks on
   the same widget within it are paired into a double click, which silently
   swallows the second click, so a click waits only when it would pair with
   the previous one.
2. **One condition-wait mechanism.** All waits go through `Selenide.waitFor(...)`
   (a `WebDriverWait` polling every 100 ms), or Selenide's `should(...)`. Both
   are bounded by `Selenide.TIMEOUT` (`-Dsail.timeoutSeconds`). The only other
   polling interval is the one an author asks for with `testGridRows@timeoutMillis`,
   which checks every second.
3. **No script clicks for user gestures.** A click goes through WebDriver, after
   the target is scrolled to the centre of the window and nothing covers it. A
   click a user could not make fails the step, with a diagnosis: the element,
   its position, the window, and what covers it. A script click would hide
   exactly the faults SAIL exists to find. The named exceptions are widget
   operations with no reliable pointer gesture:
   - PF: the editorial side bar toggle, pressed with a real pointer click at its
     drawn centre after checking that it is the topmost element.
   - SC: the slider value, set with `Slider.setValue()`.
   - SC: committing typed text with `FormItem.updateValue()`.
   - Grid and menu scrolling.

   Two script clicks remain in `PrimeFacesSelenide`, and they break this rule:
   - The answer button of a server-side question (`SKYVE.PF.ask`) is clicked
     from JavaScript.
   - A tablet action upload page that stays open is closed from JavaScript,
     as a workaround for runtime issue P3.

   They should become pointer clicks.

Scripts follow the same rules. Use `testGridRows timeoutMillis` or
`testMessage` to wait for a server job, not `<pause>`.

## Known Limitations

- **No tree or calendar in PF.** The PF UX/UIs have no tree or calendar page,
  so `navigateTree`, `navigateCalendar` and `navigateMenu` to such items fail
  in PF. SC only opens these views; there are no steps that act inside them.
- **Missing gestures.** PF has no gesture for `lookupDescriptionNew`,
  `lookupDescriptionEdit` or `dataGridEdit`. PF select steps work only on
  grids with a `selectedIdBinding`, so the same script can behave differently
  by renderer.
- **`checkMembership` has no PF component.** `FacesViewRenderer` renders none.
- **No step for**: content signatures, downloads (content, reports, exports),
  or opening a collapsed box. The Monitoring Dashboard's Refresh buttons sit in
  boxes that start collapsed, so they are not tested.
- **Server configuration.** InviteUsers, CreateUsers, SendMail, JobSchedule
  editing and RunJobNow are not covered. They need mail or the job scheduler,
  which the test server runs without.
- **Single test user.** Views are built as an every-role user. A script that
  signs in as a less privileged user must avoid views that differ for them.
- **Unused attributes.** `dataEnter@navigateToWidget`,
  `navigateMap@refreshTimeInSeconds`, `navigateMap@showRefreshControls`,
  `navigateCalendar@startBinding` and `navigateCalendar@endBinding` are accepted
  and ignored.
- **Paged lists.** `rowContaining` and `testGridRows` see only the rows the
  browser has, which is the first page of a paged list.
- **Pauses in the admin suite.** `98-destructive.xml` (two 5 s pauses after
  test data jobs) and `99-restore.xml` (a 10 s pause after the restore job) still
  use `<pause>`. These are the remaining exceptions to the no-sleep rule.
- **Update My Details is commented out** of both admin scripts (runtime issue
  P4). Other runtime bugs have workarounds in the scripts or the executors;
  see the issues list.
- **`sc-admin-sail.xml` on `sail`** still has the Communications recipient
  actions commented out, for SC finding 1. That finding was later withdrawn,
  and `sail-sc` restores the actions.

## Runtime Bugs Found

The suites found Skyve runtime bugs that are outside SAIL's scope. They are
reported, not fixed, and the scripts avoid or work around them. Each one, with
its reproduction and suspected cause, is in
[SAIL runtime issues](sail-runtime-issues.md).

## Other Executors

The `Executor` visitor (`org.skyve.metadata.sail.execution.Executor`) has
other implementations, which generate code instead of driving a browser: the
skyve-ext `WebDriverExecutor` and `SeleneseExecutor`, and their PrimeFaces
subclasses. They emit a "not supported by this generator" comment for the
newer steps (`upload`, `testMessage`, `testGridRows`, `testWidget`,
`navigateMenu`, `testMenu` and `switchMode`). The SAIL generator
(`org.skyve.impl.generate.sail.Generator`), used by the Control Panel's generate
SAIL actions, writes menu and view scripts. It adds `<switchMode/>` after the
generated `<login/>` for desktop automations.
