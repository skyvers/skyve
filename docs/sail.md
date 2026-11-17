# SAIL Browser Testing

SAIL (Skyve Application Integration Language) is an XML language for scripting
what a user does in a Skyve application: sign in, open menus and views, enter
data, press actions, and assert what the application shows. A SAIL script is
renderer-neutral. Its steps name bindings, actions and menu items from the
application's metadata, not HTML ids or CSS selectors.

The interpreted executors run a script in a real browser through Selenide,
against a running Skyve server. One executor drives PrimeFaces (PF) and one
drives SmartClient (SC). This guide covers writing scripts, the complete
op-code set and how each renderer supports it, adding SAIL to an application,
running the suites, recording, and adding an op-code.

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

The schema is `skyve-war/src/main/java/schemas/sail.xsd`, with an identical copy
in `skyve-core/src/test/resources/schemas/sail.xsd`. Both executors implement
the whole language. A step that a renderer has no UI for fails with a reason; the PF and SC
columns of the [op-code reference](#op-code-reference) say which.
Code that implements `Executor` must be updated for Skyve 10; see
[Upgrading](#upgrading).

## How the Interpreted Executors Work

A SAIL IT is a JUnit 5 test in `skyve-war/src/test/java/sail/**`. It reads a
script from the test classpath and runs it with an interpreted executor:

| Class | Role |
|---|---|
| `util.AbstractPrimeFacesInterpretedSail`, `util.AbstractSmartClientInterpretedSail` | Test bases. Take the server lock, start and stop the browser, unmarshal the script (`sailResource`, `sailFile`, `sailString`) and run it. |
| `util.AbstractInterpretedSailWorkflow` | Test base for [workflows](#workflows): `sailWorkflow(...)` runs several scripts, or a `<workflow>` file, as one run across renderers. |
| `util.sail.SailWorkflow` | Runs a workflow: picks the executor for each automation, starts a browser only when the device changes, and shares the run token and variables. |
| `util.sail.SailServerLock` | A cross-JVM file lock per base URL, so that only one SAIL IT at a time uses a server; see [One test per server](#one-test-per-server). |
| `util.sail.SailSuite` | Runs a multi-file suite: set-up files, suite files and a gated restore; see [Admin: application suite](#admin-application-suite). |
| `util.sail.InterpretedWebDriverExecutor` | Shared executor base. Runs the automation, applies the test strategy, records failures, and implements login, logout, URL navigation, the context steps, `testSuccess`, `testFailure`, `comment`, `execute` and `pause`. It also keeps the [variables](#the-run-token-and-variables), checks the list grid steps' columns, and switches the [view roles](#the-test-user) at each login. |
| `util.sail.PrimeFacesInterpretedWebDriverExecutor`, `util.sail.SmartClientInterpretedWebDriverExecutor` | Renderer executors. They locate widgets and implement the remaining steps, and the base's renderer hooks; see [Renderer hooks](#renderer-hooks). |
| `util.sail.SailUsers`, `util.sail.SailViewRoles` | The users the views are built as; see [The test user](#the-test-user). |
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

  By default, PF list views and list grids are rendered by `VueListGrid`,
  which mounts skyve-vue's ListGrid (a PrimeVue data table) in the grid's
  container. The executor finds such a Vue list grid by its container's client
  id. The grid's toolbar, rows and buttons are made in the browser, not in the
  component tree, so the executor finds them in the page by their
  `skyve-vue-list-grid` classes. A list grid rendered as a PrimeFaces data table
  is still driven through its built components. A list grid's
  `componentBuilderClass` property (`vue`, `faces` or a builder class) chooses
  how it is rendered. The executor checks each grid in the page, so it follows
  the property grid by grid; a custom builder must render one of these two.
- **SmartClient.** The SC executor generates `sail:` locators: a JSON spec such
  as `{"view":"kitchensink.KitchenSink","kind":"item","name":"text"}`. A browser
  helper, `util/sail/smartclient-sail.js`, resolves each spec against the live
  view through Skyve's client object model and `isc.AutoTest`. No component
  counters or window indexes are derived in Java.
  The helper is a test resource in `skyve-war/src/test/resources`. An app
  gets it from `skyve:assemble`, which copies `src/test/resources/util/**`
  as well as `src/test/java/util/**`.

### The test user

Generated ids depend on what the user may see. A Save button that the user
cannot use is not rendered, and every later id shifts. The executor therefore
builds views as a user with every role of every module, as Skyve's bootstrap
user has (`util.sail.SailUsers.bootstrapLikeUser`).

A script that signs in as a user with fewer roles needs the views built with
that user's roles. The test JVM cannot read the server's users, so the IT
registers each such user's roles before it runs the script:

```java
@Test
void clientWorkflow() {
	viewRoles("client", "todo.ToDoClient");
	viewRoles("tim", "timesheet.TimesheetUser", "admin.BasicUser");
	sailResource("sail/apps/todo-client.xml");
}
```

- `viewRoles(user, roles...)` is on both test bases. It fills a
  `util.sail.SailViewRoles`.
- A role is `module.Role`, or the name of a customer role. A customer role
  gives each of its module roles, as Skyve does when it reads a user.
- At each `<login/>`, the executor builds the views for the rest of the run as
  a user with exactly the registered roles of the user signing in. A user that
  is not registered gets every role. One script can sign in as several users.
- The user name is matched as written in the script, then with `{run}` and
  `{var:name}` substituted. So `viewRoles("jane{run}", "admin.BasicUser")`
  matches a user that the script created.
- The view user has the test's customer (`bizhub`), name and id. Only the roles
  differ. A role the customer does not have fails the login step.
- The test's user is restored when the automation ends.

**The server's roles.** When the server has `sail.recording` on, the harness
does not need the registration: after each successful `<login/>` of a user that
is not registered, it asks the server (`sailrecording?roles`, in the browser's
own session) for the signed-in user's effective roles, with the roles their
groups give, and builds the views for the rest of the run with exactly those.
The trace says which roles it used, and leaves out a role the test's customer
does not have. A registration always wins. If the server does not answer (the
flag is off, or an older server), the views keep every role, as before.

Without a registration, and without `sail.recording` on the server, a script that signs in as a less privileged user must
keep to navigation and assertions that the full-permission view does not
affect, as `03-security-admin.xml` does: menus, sign-in outcomes and screens
where both users see the same thing.

### Renderer hooks

`InterpretedWebDriverExecutor` implements the parts of the newer steps that do
not depend on the renderer: token substitution, variables, column checks and
the empty-value rule. It leaves the gestures to the renderer executors as
protected hooks. Each hook throws `UnsupportedOperationException` ("is not
implemented yet for …") until the renderer overrides it, so a script never
passes a step that did nothing.

| Hook | For |
|---|---|
| `filterListGrid(ListGridFilter, String column, String value, FilterOperator)` | `listGridFilter`; a null value clears the column |
| `clearListGridFilters(ListGridClearFilter)` | `listGridClearFilter` |
| `sortListGrid(ListGridSort, String column, boolean descending)` | `listGridSort` |
| `readWidgetValue(CaptureValue)` | `captureValue`; returns the displayed text |
| `readGridCell(CaptureCell)` | `captureCell`; returns the cell's displayed text |
| `readViewTitle(CaptureTitle)` | `captureTitle`; returns the view's title as shown |
| `clickDataGridLink(DataGridLink, String text, boolean action)` | `dataGridLink`; clicks the link in the row, and for an action answers its confirmation; returns true if declined |
| `clickLinkContaining(NavigateLink, String text)` | `navigateLink text`, with or without `exact` |
| `clickLinkIntoNewWindow(NavigateLink, String text)` | `navigateLink text window="new"`; returns the handle of the window the link was clicked in |
| `pressListGridTool(ListGridTool, Tool)` | `listGridTool`; PF fails it, as its list grids have no such tools |
| `enterOption(DataEnter, String value, Match)` | `dataEnter match="code\|label"`, called from the renderer's `executeDataEnter` |
| `pickLookupDescriptionRowContaining(LookupDescriptionPick, String text)` | `lookupDescriptionPick rowContaining`, called from the renderer's `executeLookupDescriptionPick` |
| `autoCompleteLookupDescriptionAndPick(LookupDescriptionAutoComplete, String search, String pick)` | `lookupDescriptionAutoComplete pick`, called from the renderer's `executeLookupDescriptionAutoComplete` |

Hooks get their text with the tokens substituted. A hook that changes a list
waits for the list's fetch to end before it returns.

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

### Workflows

A workflow is several automations run in order as one run. Each automation
keeps its own `uxui`, `userAgentType` and `testStrategy`, so a workflow can move
between renderers: a client raises a request in a PrimeFaces UX/UI, then staff
handle it in SmartClient. The automations share the
[run token and the variables](#the-run-token-and-variables), so a later
automation finds what an earlier one made, by `{run}` or by a captured value.

A workflow is either several script files or one file whose root is
`<workflow>`, holding the `<automation>` elements:

```xml
<workflow xmlns="http://www.skyve.org/xml/sail">
	<automation uxui="tablet" userAgentType="tablet">
		<interaction name="Client: raise a request">...</interaction>
	</automation>
	<automation uxui="desktop" userAgentType="desktop">
		<interaction name="Staff: find it by {var:number}">...</interaction>
	</automation>
</workflow>
```

An IT runs a workflow with `util.AbstractInterpretedSailWorkflow`:

```java
class RequestWorkflowSailIT extends AbstractInterpretedSailWorkflow {
	@Test
	void request() {
		viewRoles("client{run}", "todo.ToDoClient");
		sailWorkflow("sail/apps/request-1-client.xml", "sail/apps/request-2-staff.xml");
		// or sailWorkflow("sail/apps/request-workflow.xml") for a <workflow> file
	}
}
```

- Each automation runs in SmartClient when its `uxui` is `desktop`, and in
  PrimeFaces otherwise.
- The browser is chosen by the automation's `userAgentType`: an iPad's user
  agent for `tablet`, an iPhone's user agent and window size for `phone`, and a
  desktop Chrome otherwise. `browser(UserAgentType, Supplier)` replaces one.
- A new browser starts only when an automation needs another device than the
  previous one. Otherwise the next automation drives the same browser, even in
  the other renderer, and so the same session. Each automation signs in for
  itself.
- The server lock is taken once, by the first automation, and held until the
  test class has finished.
- [View roles](#the-test-user) work as in a single script, including users
  named with `{run}`, which is the workflow's token.
- The workflow stops at the first automation that fails, and the failure names
  the file and the automation.
- `util.sail.SailWorkflow` does the work, so a test that extends another base
  (as the Biz Hub Apps ITs extend `AbstractH2Test`) can use it directly.

`XMLMetaData.unmarshalSAILWorkflowFile` and `unmarshalSAILWorkflowString` read a
`<workflow>`, or a single `<automation>` as a workflow of one. The existing
`unmarshalSAIL*` methods read an `<automation>` only.

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

**Windows.** Each window has its own stack. `<navigateLink window="new"/>`
sets the stack of the window it runs in aside, untouched, and the new window
starts with an empty one: push a context for its page. `<closeWindow/>` drops
the closed window's stack and puts back the opener's, as it was. Windows nest:
each `<closeWindow/>` returns to the window that opened the one it closes.

```xml
<navigateLink text="Open a new lookup value in a new window" window="new" />
<pushEditContext module="kitchensink" document="LookupDescription" />
<dataEnter binding="description" value="Made in a new window {run}" />
<save />
<closeWindow />
<!-- back in the opener, with its contexts -->
<listGridTool module="kitchensink" query="qListGridFixtureVue" tool="refresh" />
```

A window left open is closed when its interaction does not complete, and when
the automation ends, so the next interaction or automation starts in the first
window.

### The run token and variables

`{run}` in a value is replaced with a token unique to the run. The token is the
current time in milliseconds in base 36: 8 or more lower-case letters and
digits. A new token is made for each automation, which means once per script
file, unless the automation is part of a [workflow](#workflows), whose
automations share one token. In a multi-file suite (`SailSuite`), a value
created with `{run}` in one file cannot be found with `{run}` in the next file.

Use `{run}` to make the records a script creates unique, so a script can run
again, or alongside another script, without clashing with leftovers.

`{var:name}` is replaced with the value of a variable. Three steps set one:
`<captureValue/>` from what a widget shows, `<captureCell/>` from a list or
data grid cell, and `<captureTitle/>` from the view's title. Use them for values
the application made, such as a generated number, that a later step must find
again:

```xml
<save />
<captureValue binding="number" variable="number" />
<ok />
<listGridFilter module="kitchensink" document="LookupDescription" column="number" value="{var:number}" operator="equals" />
<testGridRows module="kitchensink" document="LookupDescription" minRows="1" maxRows="1" />
```

- The value is the displayed text, trimmed, as `testValue` reads it.
- With `pattern`, a Java regular expression, the value is the part of the text
  that the pattern finds (the first match, not the whole text): its first
  group, or the whole match if it has no group. For example
  `<captureTitle variable="request" pattern="Request (\d+)"/>` keeps the
  number of "Request 1234 - printer". A pattern that finds nothing fails the
  step and shows the text that was read.
- A widget, cell or title that shows no value fails the step. A blank variable
  would match any row.
- A name is a letter, then letters, digits or underscores.
- Capturing the same variable again replaces its value.
- Variables last for one automation, which means one script file, or for a
  whole [workflow](#workflows). A suite's next file starts with none.
- Using a variable that has not been captured fails the step, and the message
  lists the variables that are defined.
- A substituted value is not substituted again.
- A test can give every script it runs variables to start with: `variable(name, value)` on
  `AbstractSmartClientInterpretedSail` and `AbstractPrimeFacesInterpretedSail` (or `useVariables` on the executor).
  A recorded script signs in with `{var:password}`, so its test defines `password`.

`{run}` and `{var:name}` are substituted in:

- `dataEnter@value` and `testValue@value`;
- `lookupDescriptionAutoComplete@search` and `@pick`;
- `listGridFilter@value`;
- `login@user`, `login@password` and `login@customer`, to sign in as a user
  created earlier in the same run;
- `testFailure@message` and `testMessage@contains`;
- every `rowContaining`, including `lookupDescriptionPick@rowContaining`;
- `navigateEdit@bizId`, `navigateLink@href`, `navigateLink@text` and
  `dataGridLink@text`;
- `comment` text, which the trace then shows with the values.

They are not substituted in attributes that name metadata, such as `module`,
`binding`, `column` or a menu item, nor in numbers, flags, file names,
`pattern`s or `execute` scripts (`{` is normal JavaScript).

### Values

| Widget | `dataEnter` value | `testValue` value |
|---|---|---|
| Text, password, masked, date, time, spinner, colour | The text a user would type. A newline becomes a space, because a typed newline would trip the form's default button. | The displayed text, trimmed. Colours ignore case. |
| Text area | The text. Each newline is typed as Enter. | The text, newlines included (`\r\n` reads as `\n`) |
| Check box | `true` or `false` | `true`, `false`, or empty for unset (PF) |
| Combo, radio | By default, the zero-based position of the option. For a combo, this counts the empty first entry when there is one. With `match="code"` or `match="label"`, the option's code or label; see [Data entry](#data-entry). | The option's position, code or label |
| Slider | A number | The number |
| Rich text, HTML | Plain text. Each newline is typed as Enter. | The plain text or the HTML |
| Lookup description | PF: not supported, so use `lookupDescriptionAutoComplete` or `lookupDescriptionPick`. SC: the search text; the first match is picked. | The displayed description |
| List membership | The `\|`-separated descriptions of the items to select, for example `Admin\|Basic`. Every other item is deselected. New members are added in the order the value names them in SC, and in the candidates' order in PF; use `listMembershipMove` to put an ordered membership's members in order. | The members, `\|`-separated. For an ordered membership (its collection is ordered) they are compared in order; for an unordered one, in any order |

Write a newline in a value as `&#10;`, for example
`value="09:00 10:30 Client A&#10;&#10;13:00 17:00 Client B"`. XML turns a
literal newline inside an attribute into a space before SAIL reads it. Rich
text reads as laid out: a line for each line break or block, and an empty
block is a blank line.

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
| `navigateLink` | `href` or `text` (exactly one), `exact`, `window` | With `href`, opens an absolute URL as is, or otherwise a URL relative to the base URL. With `text`, clicks the first displayed link (`<a>`), in page order, whose visible text contains the text (case-sensitive), such as a link in a blurb; with `exact="true"`, the link's visible text, trimmed, must equal the text. No match fails the step with the page's link texts. `exact` with `href` fails the step. Without `window`, a link that opens a new window (a `target` other than `_self`, `_top` or `_parent`) fails the step, and the top context, if any, loses its document and view type: push a context afterwards if later steps need one. With `window="new"`, `text` follows a link that opens a new tab or window, such as a report or a `target="_blank"` link, and `href` opens the URL in a new tab; the step waits for the new window's page (a PDF or other non-HTML page counts once the browser shows it) and the later steps run in that window, which starts with no contexts (see [Contexts](#contexts)). A link that loads its page in the same window, or opens no window, then fails the step. | yes | yes; with `text`, if the link leads to another page the step waits for it to load, then for SmartClient to settle |
| `closeWindow` | none | Closes the window that `navigateLink window="new"` opened (or that its page closed already) and returns to the window that opened it, then waits for its page. The closed window's contexts are dropped and the opener's are back as they were. Windows nest. In the window the automation started in, the step fails: there is no window to close. | yes | yes |
| `navigateBack` | none | Goes back to the previous page, as the browser's Back button does, and waits for it (and for SmartClient to settle); a back to an earlier fragment of the same page counts once the URL changes. No earlier page in the window's history fails the step. The top context, if any, loses its document and view type, as after `navigateLink`: push a context for the page. | yes: PF views are pages, so back returns to the previous view | yes: SmartClient opens views inside one page, so back returns to the previous page load (a navigate step, a link or a sign in), not to the previous view |
| `refresh` | none | Reloads the page, as the browser's Reload button does, and waits for it (and for SmartClient to settle). Values that were not saved are lost. The contexts are unchanged. | yes | yes: the reload shows the view the page's URL names, which is the last page load's; after menus, zooms or New, pop or push contexts to match |
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
| `dataEnter` | `binding`\*, `value`\*, `navigateToWidget`, `match` | Enters the value into the first displayed, enabled widget for the binding; see [Values](#values). Fails if no widget can take it. `navigateToWidget` is accepted and ignored: the executors always scroll the target into view. | yes; after `dataGridNew` or `dataGridEdit` on an inline grid, a binding that is one of the grid's columns is that row's cell (see `dataGridEdit`) | yes; after `dataGridNew`/`dataGridEdit` on an inline grid, bindings are tried as the grid's columns first |
| `dataEnter match` | `index` (default), `code`, `label` | How the value names a combo or radio option: its position, its code or its label. A code or label must name exactly one option. `code` and `label` fail on any other widget. Prefer `code`: labels change with the language, and positions change when options are added. | yes: the option's value or its label as the page shows them (a combo's empty first entry counts) | yes: the option's position is found from the item's options, then it is chosen as with an index |
| `testDataEnter` | `fixture` | Builds a bean with `DataBuilder` (the named fixture, or the `sail` fixture), then enters its values into every scalar widget of the view. Membership widgets are skipped. | yes | yes |
| `upload` | `binding` or `action` (exactly one), `file`\*, `testSuccess` | Uploads a file from the **test** classpath (for example `sail/files/sail-image.png` from `skyve-war/src/test/resources`). With `binding`, it uses the content widget's upload page, and the widget must hold content afterwards. With `action`, it presses the upload action's button, chooses the file and submits it; in SmartClient, `action` can also name a BizPort import action, whose import page (`bizImport.xhtml`) the step drives the same way. An upload action's page that succeeds, and an import page whose import has no problems, close themselves and show their messages in the view's growl, which `testMessage` can check; one with problems (warnings too) stays open, so an import that warns needs `testSuccess="false"`. When the upload page opens the chosen file in its image editor, as it does for an image field, the step presses the editor's Upload button (`#crop`), which uploads it. Otherwise it presses the file list's Upload button. With `testSuccess="false"`, the upload may be refused: the step does not check that the widget holds content or that no error shows. A refusal shown on the upload page is kept, and the upload page is closed with its close button, as a user does after reading it. A following `<testFailure message="..."/>` then checks the refusal, wherever it shows: on the upload page or in the view. | yes; in the image editor, it waits for the cropper before pressing Upload | yes |
| `lookupDescriptionAutoComplete` | `binding`\*, `search`\*, `pick` | Types the search text into the lookup and picks the first suggestion. With `pick`, it picks the first suggestion, in the order shown, that contains the text (case-sensitive). Fails if nothing matches. | yes | yes; only the rows the drop-down has fetched are searched. After `dataGridNew`/`dataGridEdit` on an inline grid, a column of the binding is used first, as for `dataEnter` |
| `lookupDescriptionPick` | `binding`\*, `row` or `rowContaining` (exactly one) | Opens the lookup's drop-down and picks the zero-based row, or the first row, in the order shown, that contains the text (case-sensitive). | yes; `rowContaining` sees only the rows the drop-down shows | yes; `rowContaining` sees only the rows the drop-down has fetched. After `dataGridNew`/`dataGridEdit` on an inline grid, a column of the binding is used first, as for `dataEnter` |
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
| `action` | `name`\*, `confirm`, `testSuccess` | Presses the view's button for the action. | yes; an upload action only opens its upload page, which stays open: use `<upload action="…"/>` | yes; an implicit Upload action fails: use `<upload action="…"/>`; a BizPort import only opens its import page: use `<upload action="…"/>` |
| `save` | `confirm`, `createView`, `testSuccess` | Presses Save. `createView` sets the context's view type for what follows: `true` for create, `false` for edit. | yes | yes |
| `ok` | `confirm`, `testSuccess` | Presses OK, then pops the context once the view has closed. | yes | yes |
| `cancel` | `testSuccess` | Presses Cancel, then pops the context once the view has closed. | yes | yes |
| `delete` | `confirm`, `testSuccess` | Presses Delete, then pops the context once the view has closed. | yes | yes |
| `remove` | `confirm`, `testSuccess` | Presses Remove in a zoomed-in child view, then pops the context once the view has closed. | yes | yes |
| `zoomOut` | `confirm`, `testSuccess` | Presses Zoom Out, then pops the context once the view has closed. | yes | yes |
| `zoomIn` | `binding`\*, `testSuccess` | Presses the zoom-in button of an association, then pushes the related document's context. If the zoom is refused, for example by validation with `testSuccess="false"`, it stays in the view and pushes nothing. | yes | yes |

If a closing step is declined, or is refused with errors (expected with
`testSuccess="false"`), its view stays open and its context is kept, so the
script can carry on in that view. A save that is declined or refused keeps its
view type.

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
| `dataGridNew` | `binding`\* | Adds a row. For a grid that is not inline, this opens the row's view and pushes its context. | yes; inline grids add in place, and the steps that follow address the new row's cells (it is added last), as for `dataGridEdit` | yes; inline grids start an inline edit |
| `dataGridZoom` | `binding`\*, `row` or `rowContaining` | Zooms into the row and pushes its context. | yes | yes |
| `dataGridEdit` | `binding`\*, `row` or `rowContaining` | Starts an inline edit of the row. | inline grids only, with no gesture (the cells are inputs). The following `dataEnter` (also with `match`), `testValue`, `lookupDescriptionPick` and `lookupDescriptionAutoComplete` steps address the row's cells: a binding that is one of the grid's columns means only the row's cell (a cell that is not on the page fails the step; the view's widget of the same binding is not used), and any other binding means the view's widget. Steps that do not touch the page (`comment`, `pause`, `captureValue`, `testWidget`, `testMessage`, `testSuccess`, `testFailure`, `testMenu`) keep the row; any other step ends it, as in SmartClient, including `captureCell` and `testGridRows` (which read the grid's saved rows), `captureTitle` and `testDataEnter`. `rowContaining` also reads the values of the row's inputs. A `row` past the grid's last row fails the step (a data grid shows all its rows: it has no pages). Other grids have no row edit gesture: use `dataGridZoom` | yes; the following `dataEnter`, `testValue`, `testWidget`, `lookupDescriptionPick` and `lookupDescriptionAutoComplete` steps address the row's columns, and any step that touches the page ends the edit |
| `dataGridRemove` | `binding`\*, `row` or `rowContaining` | Removes the row from the collection. The record itself is not deleted. | yes | yes |
| `dataGridSelect` | `binding`\*, `row` or `rowContaining` | Selects the row without zooming. | only on grids with a `selectedIdBinding`; on other grids a selection zooms | yes |
| `dataGridMove` | `binding`\*, `row` or `rowContaining`, `to`\* | Drags the row of an ordered data grid to the zero-based position `to`, counted in the rows as shown, with a real drag-and-drop gesture. The rows are numbered again in their new order, which a save keeps. Fails on a grid that is not ordered (its rows cannot be dragged) and when `to` is past the last row. The context is unchanged. | yes: the row is dragged by its drag handle, and the server reorders the collection at once (`rowReorder`) | yes; it ends any inline edit first |
| `listMembershipMove` | `binding`\*, `row` or `rowContaining`, `to`\* | Drags a member of an ordered list membership to the zero-based position `to` in its member list, counted in the members as shown, with a real drag-and-drop gesture. `row` is the member's position in the member list; `rowContaining` is text its description contains (case-sensitive). The members are numbered again in their new order, which a save keeps. Fails on a membership that is not ordered and when `to` is past the last member. The context is unchanged. | yes: the member is clicked, then its pick list's move up or down button (shown for an ordered membership) once per position | yes; it ends any inline edit first |
| `dataGridLink` | `binding`\*, `row` or `rowContaining`, `text`\*, `confirm`, `testSuccess` | Clicks the first displayed link in the row whose visible text contains `text` (case-sensitive), such as a "Revoke Access" link in every row. A link that runs an action is pressed as an [action step](#actions) presses its button: `confirm` answers its confirmation and questions, and the step checks for errors unless `testSuccess="false"` or it was declined; the view and its context stay. Any other link is followed as `navigateLink` follows one: the top context loses its document and view type. Which kind a link is comes from the view's metadata: the grid's links whose shown value contains the text (or every link of the grid if none does) all have an action reference. No such link in the row fails the step, listing the row's links. | yes; a click on a row's link does not select the row, so it works on a grid that zooms when a row is selected | yes |
| `listGridNew` | `module`, `document`, `query`, `model`, `createView` | Presses New and pushes the new document's context. | yes; on a Vue list grid, its toolbar's New button, or the floating one a list view shows above its paginator | yes |
| `listGridZoom` | as `listGridNew` without `createView`, plus `row` or `rowContaining` | Opens the row and pushes its context. | yes; on a Vue list grid, the row's View Detail button, or a click on the row when the grid zooms on a row click | yes |
| `listGridSelect` | as `listGridZoom` | Selects the row without opening it. | only on grids with a `selectedIdBinding`. On a Vue list grid, it clicks the row. A row that already shows as selected is clicked twice, to unselect and select it: the grid restores its selection by design, without running the selected action. The step fails if the row does not show as selected afterwards. | yes |
| `listGridFilter` | `module`, `document`, `query`, `model`; `column`\*, `value`, `operator` | Filters a column, as a user does in the grid's column filters. `column` is the column's binding. `operator` is `contains` (the default), `startsWith`, `equals`, `notEquals`, `lessThan` or `greaterThan`. Filters on different columns are ANDed, and a new filter on a column replaces its old one. Without `value`, the step clears that column's filter; `operator` then fails the step. | yes; see [Filtering lists](#filtering-lists) | yes; see [Filtering lists](#filtering-lists) |
| `listGridClearFilter` | `module`, `document`, `query`, `model` | Clears every column filter. | yes: Clear in each filtered column's menu (Vue); in a data table it deletes each typed filter's text, chooses each combo filter's empty option and unsets each check box | yes: the toolbar's Clear filter button, which clears the filter row and the advanced filter |
| `listGridSort` | `module`, `document`, `query`, `model`; `column`\*, `descending` | Sorts by the column alone, ascending unless `descending="true"`. Sort before a `rowContaining` when more than one row could match. | yes: clicks the column title until it alone shows the direction. PrimeFaces ignores a sort click on an empty data table, so that fails | yes: clicks the column header, once for ascending and again to reverse |
| `listGridTool` | `module`, `document`, `query`, `model`; `tool`\* | Presses a list grid tool that is not a view action, as a user does with the grid's toolbar. `tool` is `refresh`, which fetches the grid's rows again with its filters and sort (rows added elsewhere show up), or `clearSelection`, which deselects the selected rows (and so clears the grid's `selectedIdBinding`); the schema rejects any other value. The context is unchanged. | no: neither the Vue list grid nor the PrimeFaces data table has a Refresh or Deselect tool, so the step fails saying so; `<refresh/>` reloads the page, and its list grids with it | yes: the toolbar's Refresh button, then waits for the rows; the Deselect button, then checks that no row is selected. A toolbar without the tool fails the step |

A list step that names no `module`, `document`, `query` or `model` uses the
list the context shows: the one `navigateList` or a list menu item opened, with
its query or model. Name the list when the view shows more than one, such as an
edit view's list grids.

`listGridFilter` and `listGridSort` check `column` against the list's query
before they touch the page. A column that is not in the query, or that the
query says cannot be filtered or sorted, fails the step with the query's
columns. A model's columns are not checked. Filter or sort a list with more
rows than one page before you find a row in it: `rowContaining` sees only the
rows the browser has.

### Filtering lists

A filter without `operator` means `contains`, so filters on enum, boolean,
date and time columns need `operator="equals"` (or another operator the column
takes). Values are written the same way in both renderers:

- **Enum or constant domain:** the option's shown label exactly, or else its
  code exactly. The step fails, listing the `code=label` pairs, if neither
  matches or more than one option does.
- **Boolean:** `true` or `false`.
- **Date, dateTime, timestamp and time:** as `dataEnter` takes them for that
  field, which is the attribute's converter format. With the default
  converters: `07-Nov-2025`, `07-Nov-2025 15:38`, `07-Nov-2025 15:43:16` and
  `11:11`; with others, for example `06/05/2026 02:47 PM` (`MM_DD_YYYY_HH_MI`)
  or `Jun-05-2026` (`MMM_DD_YYYY`). A date time or timestamp whose converter
  shows only the date (`*_DateTime`, `*_Timestamp`) is filtered by the whole
  local day in both renderers: `equals` is that day, `notEquals` any other,
  `lessThan` before it and `greaterThan` after it. In SmartClient's advanced
  filter, `between` is after the start day and before the end day,
  `betweenInclusive` from the start day to the end day, and Match Any and Match
  None (and groups within them) combine these days; export, charts and Tag or
  Untag All use the same days as the rows shown.
- **Numbers:** as the column's cells show them, which is the converter's
  format: `1,234.56`, `12%` (a percentage converter reads it as 0.12, but
  `SimplePercentage` as 12), `-0:30` (`Decimal5TimeDuration`). `equals`
  compares the stored value exactly, so a value a cell shows rounded (`2.35`
  for a stored 2.345) or as a magnitude (`Decimal2DollarsAndCentsAbsolute`)
  is not found by what it shows: filter by what is stored (see
  [Known Limitations](#known-limitations)). In SmartClient's filter row, a
  value the column's converter refuses (`+5`, `1e3`, `$5` in a column that is
  not dollars) shows an error on the cell and the list is not filtered.

A step whose operator the column cannot take fails and names the operators it
can take. Filter, clear and sort steps fail if an error message shows
afterwards.

| Column type | PF Vue list grid | PF data table | SC list grid |
|---|---|---|---|
| Text | `contains`, `startsWith`, `equals`, `notEquals`; all ignore case | `contains` only, as the server decides (also for a variant domain's description and an association's business key) | `contains` in the filter row (ignores case); the others in the advanced filter (`contains` and `startsWith` ignore case) |
| Number | `equals`, `notEquals`, `lessThan`, `greaterThan` | `equals` only | `equals` in the filter row; the others in the advanced filter, where the value is read as the column's field reads what a user types (`1,234.56`, `12%`; signed dollars, `-7.50`, for `Decimal2DollarsAndCentsAbsolute`) |
| Enum, constant domain | `equals`, `notEquals`, from the menu's drop-down | `equals`, from the filter's combo | `equals`, from the filter row's select |
| Boolean | `equals`, `notEquals`, with the three-state check box | `equals`, with the filter's check box | `equals`, with the filter row's three-state check box |
| Date, dateTime, timestamp | `equals`, `notEquals`, `lessThan` (Before), `greaterThan` (After) | `equals` only | `equals` in the filter row; `notEquals`, `lessThan` and `greaterThan` in the advanced filter |
| Time | `equals` | `equals` only | `equals` in the filter row when the list shows the column; `equals`, `notEquals`, `lessThan` and `greaterThan` in the advanced filter, for a shown or hidden column |
| Association | as text, on its business key | as text | `equals`, by the related record's description, from the filter row's select. Use a column through the association, such as `selection.description`, for `contains` |

**PF details.** The Vue list grid uses each column's filter menu. A new filter
on a column that already has one presses Clear first. The step fails on a
column the grid does not show (show it with the column chooser first) and on a
grid whose toolbar shows Match Any, because its filters are ORed. A Vue grid's
filter calendar takes the column's converter format, so the harness types the
value as written (read and written again by the converter: `5/6/2026` is typed
`05/06/2026`). A number column's filter is a text field that sends the text as
typed. A data table filters each column one way, chosen by the server; the step
opens the filter row with Toggle filters when it is hidden.

**SC details.** The filter row is used for the operators it applies:
`contains` on text, `equals` on anything else, and clearing. Other operators,
and columns the row does not show, use the advanced filter. Its toggle and
Filter button are clicked, but the clauses are set through the filter
builder's API, as a saved snapshot sets them; the field, operator and value
pickers are not clicked. The filter row's existing filters become clauses. The
grid then stays in advanced filtering until the view is opened again, so later
filters on it are clauses too. An association `equals` fails once the grid is
in advanced filtering, because that hides the row.

### Assertions

Assertions follow the [test strategy](#test-strategies-and-failure-reports).
Under None, they are skipped.

| Element | Attributes | Semantics | PF | SC |
|---|---|---|---|---|
| `testValue` | `binding`\*, `value`\* | Asserts that the widget shows the value; see [Values](#values). | yes | yes |
| `testSuccess` | | Asserts that no error message, growl or field error is shown. Information messages do not count. Straight after a login, it asserts that the login succeeded; straight after an `upload testSuccess="false"`, that the upload was not refused. | yes | yes |
| `testFailure` | `message` | Asserts that an error is shown and, if `message` is given, that the error contains it. Only errors count: error messages and growls, field errors, and in SC the Problems dialog. Straight after a login, it asserts that the login was rejected. Straight after an `upload testSuccess="false"`, a refusal on the upload page counts too. | yes | yes |
| `testMessage` | `contains`\*, `type` (`info`, `warning`, `error`) | Asserts that a growl or message containing the text, case-sensitively, is shown. With `type`, the message must have that severity. Unlike `testSuccess`/`testFailure`, it sees information and warning messages too. | yes: global messages, growls and field messages | yes: `isc.say` is info, `isc.warn` is warning, the Problems dialog and field errors are error, and growls go by their class |
| `testGridRows` | `binding`, or `module`/`document`/`query`/`model`; `rowContaining`, `minRows`, `maxRows`, `timeoutMillis` | Counts the rows, or only those containing the text, and asserts `minRows ≤ count ≤ maxRows`. An absent `minRows` means 0 and an absent `maxRows` means no maximum. With neither, the step asserts at least one row. `maxRows="0"` asserts absence. `binding` (a data grid) cannot be combined with the list attributes. With `timeoutMillis`, the grid is reloaded from the server and recounted every second until the bounds hold or time runs out. Use it to wait for a server job. A timed step still waits under None. | yes; a Vue list grid refetches its rows as it does after an ajax update, and the step waits for that fetch | yes; only list grids are reloaded |
| `testWidget` | exactly one of `binding`, `action`, `tab`; `visible`, `enabled` | `visible="true"` asserts the target is shown. `visible="false"` asserts it is hidden or not rendered. `enabled` asserts it is shown and is, or is not, enabled. With neither, the step asserts visible. `visible="false"` cannot be combined with `enabled`. `tab` is a tab path as for `tabSelect`. | yes | yes |

`testMenu` is listed under [Navigation](#navigation).

### Other steps

| Element | Content or attributes | Semantics | PF | SC |
|---|---|---|---|---|
| `comment` | text | Written to the trace, with `{run}` and `{var:name}` substituted. Does nothing else. | yes | yes |
| `execute` | JavaScript text | Runs the script in the browser page. Use it sparingly: it bypasses what SAIL is testing. | yes; then waits for any request or page load the script started | yes; checks for open dialogs first, then waits for SmartClient to settle |
| `pause` | `millis`\* | Sleeps. This is a last resort, for when no observable condition exists: see [Synchronisation Rules](#synchronisation-rules). The kitchen sinks pause once, for 100 ms, only to test the op-code. | yes | yes |
| `captureValue` | `binding`\*, `variable`\*, `pattern` | Keeps the value the widget shows in a variable for `{var:name}`; see [The run token and variables](#the-run-token-and-variables). It is not an assertion, so it runs under every strategy. | yes | yes |
| `captureCell` | `variable`\*, `column`\*, `row` or `rowContaining`, `binding` or `module`/`document`/`query`/`model`, `pattern` | Keeps the text a grid cell shows: the cell in the column whose binding is `column`, of the row. The grid is a data grid named by `binding`, or a list grid named as the [grid steps](#grids) name it, each attribute defaulting from the context; `binding` cannot be combined with the list attributes. A list grid's column is checked against its query first. A cell's input (an inline grid) reads as its value. | yes: a data table's column by its header, a Vue list grid's by its title; the title a card layout repeats in a cell is left out | yes: the grid field of the sanitised binding |
| `captureTitle` | `variable`\*, `pattern` | Keeps the title the current view shows. | yes: the template's title | yes: the view's header (a list view's too) |

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

In PF, the messages listed after "but found:" also include the page's script
errors, as `(page script errors: …)`. A script error can stop a response from
being applied.

**Lost OS focus.** A headed browser can lose the OS focus to another window,
such as another headed browser, in the middle of a step. SmartClient then
closes an open pick list (its field blurs) and, when the focus comes back,
selects a text field's whole value, so the next key typed replaces it. To show
when this happened, the harness counts the page's `window` blur events
(`window.__sailBlurs`, installed in each page) before each step. When a step
fails and the count went up during it, the message gets, before the URL:

```text
...; the browser window lost the OS focus during this step (1 time); SmartClient closes an open pick list when its field loses the focus, ... [at ...]
```

If the window did not have the OS focus when the step failed
(`document.hasFocus()`), the message says so instead. A page loaded during
the step (as PF loads a page per view) starts a new count, so losses before it
are unknown and are not reported as none. Opening, closing or switching
windows (`navigateLink window="new"`, `closeWindow`, closing windows left open)
blurs pages, and a blur can arrive in the next step, so losses are unknown in a
step that changes windows and in the step after one. Focus moving into, within
or out of the page's frames (SmartClient's upload page, a rich text editor)
blurs the window it leaves while the browser keeps the OS focus, so a blur
counts only if the page no longer has the focus (`document.hasFocus()`) as it
fires; once a same-origin frame has the focus, its window's blur is what
counts. A page that is not HTML,
such as a PDF in the browser's viewer, has no count. This is a diagnostic only: it never
waits, retries or changes whether a step passes, and if the probe itself fails
it says nothing.

Under Verify, the message starts with `SAIL automation failed with N failure(s):`
and lists one failure per line. Each failure from a step that threw is also
attached to the `AssertionError` as a suppressed exception, with the original
exception as its cause, so the test report shows its stack trace; the trace
shows the cause chain and the harness frames. Nothing is only logged: a script
never prints a failure and then passes.

Two system properties help when a run fails:

- `-Dsail.testStrategy=Verify` overrides every script's strategy, so a single
  run lists all the failures. The value is matched ignoring case. An invalid
  value fails before any step runs, with a message that lists the valid
  strategies.
  - The admin suites refuse the override, as does `PrimeFacesSailTestStrategyIT`:
    the test fails fast if the property is set. Under Verify a failed
    precondition would not stop a suite, and under None nothing would be
    checked before the restore.
- `-Dsail.failureDir=<dir>` saves a screenshot (`.png`) and the page source
  (`.html`) for each failure, named `NN-<interaction>-stepNN`.

`PrimeFacesSailTestStrategyIT` runs one script under each strategy against the
kitchen sink, and checks that each strategy behaves as this table says.

## Adding SAIL to an Application

This section is for an application built on Skyve, such as one made with Skyve
Foundry. Skyve's own tests are set up in `skyve-war` already.

### The harness

The harness is not a jar. It is test source in `skyve-war/src/test/java/util/**`
and `skyve-war/src/test/resources/util/**`, and `skyve:assemble` copies both
folders into the app's `src/test` tree with the rest of the Skyve template.
Assemble also changes the customer in `util.InternalBaseH2Test` to the app's
customer, so the views are built for it.

- Do not edit the harness in an app. Assemble deletes the app's files under
  both `util` folders before it copies, so changes are lost. Fix the harness
  in Skyve instead.
- Keep the app's own tests out of `util`, for example in a `sail` package.
- An app assembled before Skyve 10 has no `util/sail/smartclient-sail.js`, and
  every SC test fails saying it is not on the test classpath. Assemble again.

### Dependencies

The harness needs these test-scope dependencies, as `skyve-war/pom.xml` has
them. Most Skyve apps have all but Selenide and Mojarra. Use the versions the
app's pom manages, or else Skyve's root `pom.xml`.

```xml
<!-- JUnit 5 -->
<dependency>
	<groupId>org.junit.jupiter</groupId>
	<artifactId>junit-jupiter-engine</artifactId>
	<scope>test</scope>
</dependency>
<!-- CDI for Skyve on H2 in the test JVM -->
<dependency>
	<groupId>org.jboss.weld.se</groupId>
	<artifactId>weld-se-core</artifactId>
	<scope>test</scope>
</dependency>
<!-- The browser driver -->
<dependency>
	<groupId>com.codeborne</groupId>
	<artifactId>selenide</artifactId>
	<version>7.12.1</version>
	<scope>test</scope>
</dependency>
<!-- Mojarra, for the PF executor's component trees; without it:
     ClassNotFoundException: com.sun.faces.util.Util -->
<dependency>
	<groupId>org.glassfish</groupId>
	<artifactId>jakarta.faces</artifactId>
	<version>4.0.11</version> <!-- Skyve's mojarra.version -->
	<scope>test</scope>
</dependency>
<!-- Jakarta Mail and EL implementations, and mocks for the OmniFaces CDI bootstrap -->
<dependency>
	<groupId>org.eclipse.angus</groupId>
	<artifactId>jakarta.mail</artifactId>
	<scope>test</scope>
</dependency>
<dependency>
	<groupId>org.glassfish.expressly</groupId>
	<artifactId>expressly</artifactId>
	<scope>test</scope>
</dependency>
<dependency>
	<groupId>org.springframework</groupId>
	<artifactId>spring-test</artifactId>
	<scope>test</scope>
</dependency>
```

### Where scripts go

| Location | Test call | Maven setup |
|---|---|---|
| `src/test/resources/sail/**` | `sailResource("sail/contacts.xml")` | None |
| Beside the tests, `src/test/java/sail/**/*.xml`, as `skyve-war` does | `sailResource("sail/contacts.xml")` | The test resource below |
| Any file | `sailFile("src/test/java/sail/contacts.xml")` | None, but the path is relative to the directory Maven runs in |

Declaring `testResources` replaces Maven's default, so repeat
`src/test/resources`:

```xml
<testResources>
	<testResource>
		<directory>src/test/resources</directory>
	</testResource>
	<!-- SAIL scripts live beside the SAIL tests that run them -->
	<testResource>
		<directory>src/test/java</directory>
		<includes>
			<include>sail/**/*.xml</include>
		</includes>
	</testResource>
</testResources>
```

To catch schema errors in the unit build, add a test like
`sail.SailScriptsUnmarshalTest`, which unmarshals every script with
`XMLMetaData.unmarshalSAILFile` (or `unmarshalSAILWorkflowFile` for a
`<workflow>`).

### Keeping SAIL out of the build

A plain `mvn test` or `mvn verify` must never run SAIL tests: they need a
server, and they change its data.

- Name every SAIL test class `*SailIT`. Skyve's root pom excludes `**/*IT`
  from surefire; check that the app's pom does too.
- Failsafe is skipped unless `skipIntegrationTests` is `false`.
- Run SAIL tests through surefire with an explicit `-Dtest`, which overrides
  the exclusion, as in [Commands](#commands).
- Turn reruns off; see **Reruns** under [System properties](#system-properties).
  Biz Hub Apps, whose pom fixes the rerun count, has a profile for it:

```xml
<!-- SAIL ITs: no reruns (a rerun repeats destructive steps); mvn -Psail-it -->
<profile>
	<id>sail-it</id>
	<build>
		<plugins>
			<plugin>
				<groupId>org.apache.maven.plugins</groupId>
				<artifactId>maven-surefire-plugin</artifactId>
				<configuration>
					<rerunFailingTestsCount>0</rerunFailingTestsCount>
				</configuration>
			</plugin>
		</plugins>
	</build>
</profile>
```

```bash
mvn -Psail-it test -Dtest='ContactsSailIT' \
	-Dsurefire.failIfNoSpecifiedTests=false \
	-Dsail.baseUrl=http://localhost:8180/myapp/
```

Run against a disposable server, set up as in
[A disposable server](#a-disposable-server) with the app's own datasource and
JSON. Turn off anything in the app that sends mail or posts to outside
services.

### A test class

A test extends one of the test bases in
[How the Interpreted Executors Work](#how-the-interpreted-executors-work).
Its constructor chooses the browser, whose user agent decides the UX/UI the
server renders (see [UX/UI and user agent](#uxui-and-user-agent)):

```java
package sail;

import org.junit.jupiter.api.Test;

import util.AbstractPrimeFacesInterpretedSail;
import util.sail.BrowserConfiguration;
import util.sail.BrowserConfiguration.Browsers;
import util.sail.Devices;

class ContactsSailIT extends AbstractPrimeFacesInterpretedSail {
	ContactsSailIT() {
		// The script declares uxui="tablet" userAgentType="tablet"
		super(new BrowserConfiguration()
				.browser(Browsers.chrome)
				.userAgentString(Devices.ipad.userAgentString));
	}

	@Test
	void contacts() {
		viewRoles("clerk", "admin.BasicUser"); // only for users with fewer roles
		sailResource("sail/contacts.xml");
	}
}
```

- **Phone:** add `.resolution(Devices.iphone.resolution.getWidth(),
  Devices.iphone.resolution.getHeight())` with `Devices.iphone`'s user agent,
  for `uxui="phone"`.
- **SmartClient:** extend `AbstractSmartClientInterpretedSail` with
  `new BrowserConfiguration().browser(Browsers.chrome)`, for `uxui="desktop"`.
  The script follows `<login/>` with `<switchMode/>`.
- `sailResource`, `sailFile` and `sailString` run a script from the classpath,
  a file or a string. `viewRoles` is described in [The test user](#the-test-user)
  and `variable` in [The run token and variables](#the-run-token-and-variables).
  Workflows use `AbstractInterpretedSailWorkflow` (see [Workflows](#workflows)),
  and multi-file suites `util.sail.SailSuite`.
- A failed step throws an `AssertionError`, so the test needs no assertions of
  its own.
- An app with its own `router.DefaultUxUiSelector` may route users by role.
  Check what it gives the user agent and each user the scripts sign in as.

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
   - `sail.recording`: `true`, only for `SmartClientRecorderRoundTripSailIT`. The other ITs do not mind it.
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
	-Dsurefire.rerunFailingTestsCount=0 \
	-DforkCount=1 \
	-Dsail.baseUrl=http://localhost:8180/skyve/
```

Name the ITs explicitly. A pattern such as `*Sail*` also matches older JUnit 4
classes in `sail/admin/pf`, and it runs browser ITs where you meant to run unit
tests. The SAIL ITs are:

| IT | Script | Renderer |
|---|---|---|
| `PrimeFacesKitchenSinkSailIT` | `sail/kitchenSink/pf/pf-kitchensink-sail.xml`, plus a test that switches to SmartClient with PrimeFaces' user menu and checks `<switchMode/>` back, one that checks the find steps' failures, one that checks the window steps' and `listGridTool`'s failures (`windowAndToolFailures`), and `rowLink` (`pf-kitchensink-row-link.xml`) | PF, tablet |
| `PrimeFacesConvertersSailIT` | `sail/kitchenSink/pf/pf-converters-*.xml`, a test per group of converters: entry in fields and inline grid cells, then Vue list grid and data table filters | PF, tablet |
| `PrimeFacesSailTestStrategyIT` | inline scripts, 3 tests | PF, tablet |
| `PrimeFacesFocusSailIT` | where a page puts the focus: a rerender low in a long view keeps the scroll and the focus; a loaded view focuses its first input | PF, tablet |
| `PrimeFacesRequiredMessagesSailIT` | when required-field messages show: not when a view first renders, after a rerender without client validation or for an action with `clientValidation="false"`; only for an action with client validation. One inline script per UX/UI | PF, tablet, phone and external |
| `PrimeFacesSelectMenuSailIT` | a select menu step ends only when its drop down has closed, even when the item was clicked while the drop down was still opening (its transitions are slowed); the focus PrimeFaces gives the menu at the end of the closing does not scroll the page under the next step | PF, tablet |
| `PrimeFacesActionUploadSailIT` | a successful action upload closes its upload page, refreshes the view and shows the page's messages in the view's growl, the upload's and the action's own, with the close icons hidden so that the upload step cannot close the page itself; a growl pushed just before the messages and one pushed after them all stay showing. One inline script per UX/UI | PF, tablet, phone and external |
| `PrimeFacesAdminSuiteSailIT` | `sail/admin/pf/suite/*.xml` | PF, tablet |
| `PrimeFacesAdminSailIT` | `sail/admin/pf/pf-admin-sail.xml` (the older single-file admin script) | PF, tablet |
| `PrimeFacesAdminPhoneSailIT` | `sail/admin/pf/admin-phone-smoke.xml` | PF, phone |
| `SmartClientKitchenSinkSailIT` | `sail/kitchenSink/sc/sc-kitchensink-sail.xml`, `sc-kitchensink-sail-find.xml` (`find`), and inline scripts that check the find steps' failures (`findFailures`) and the window steps' failures (`windowFailures`) | SC |
| `SmartClientConvertersSailIT` | `sail/kitchenSink/sc/sc-converters-*.xml`, a test per group of converters: entry in fields and inline grid cells, then list grid filters | SC |
| `SmartClientAdminSailIT` | `sail/admin/sc/sc-admin-sail.xml` | SC |
| `SmartClientAdminSuiteSailIT` | `sail/admin/sc/suite/*.xml` | SC |
| `SmartClientDataGridSortSailIT` | an inline script, then a column header sort of a data grid and a Save through `SmartClientSelenide` (no SAIL step sorts a data grid) | SC |
| `SmartClientFastTypingSailIT` | the User wizard's new password, whose change handler rerenders, typed in one burst of keys through Selenium, then left with Tab, then typed again with Next clicked at once | SC |
| `SmartClientListGridSelectedEventSailIT` | selects a List Grid Fixture list grid row whose selected event rerenders: one request and one selection event, and the selection kept | SC |
| `SmartClientPushGrowlSailIT` | a growl, then a pushed growl: both stay showing | SC |
| `SmartClientActionUploadSailIT` | a refused action upload leaves its window open showing why; a successful one closes its window, refreshes the view and shows the page's messages in the view's growl, the upload's and the action's own, with the close buttons hidden so that the upload step cannot close the window itself; a growl pushed as the page refreshes the view, and a growl in the refresh's response, leave the messages showing; a BizPort import that warns leaves its window open, and a clean one (an empty workbook into admin's Data Maintenance) closes it and shows "The import completed successfully." in the view | SC |
| `SmartClientServerRolesSailIT` | `sail/admin/sc/suite/03-security-admin.xml` with no `viewRoles` registration: the new user's views are built with the roles the server reports. Needs `sail.recording` on. | SC |
| `SmartClientRecorderRoundTripSailIT` | records `sail/kitchenSink/sc/sc-recorder-round-trip.xml`, `sc-recorder-round-trip-more.xml` (grids, lookups, uploads, list filters, captures, questions), `sc-recorder-round-trip-windows.xml` (new windows, list grid tools, back and refresh), `sc-recorder-round-trip-converters.xml` (dates, times and numbers on other converters than the defaults, and a decimal with no converter), an ordered list membership, an admin contacts workflow and checks added through the panel, and replays the recordings; also checks that the home page is unchanged unless recording. See [Recording](#recording). Needs `sail.recording` on. | SC |
| `PrimeFacesRecorderRoundTripSailIT` | records `sail/kitchenSink/pf/pf-recorder-round-trip.xml`, `pf-recorder-round-trip-more.xml` (grids, lookups, uploads, list filters and a data table's filters and sorts, captures, questions), `pf-recorder-round-trip-windows.xml` (new windows, back and refresh), `pf-recorder-round-trip-converters.xml` (dates, times and numbers on other converters than the defaults, and a decimal with no converter), an admin contacts workflow and checks added through the panel, and replays the recordings; also checks the sign out warning and that pages are unchanged unless recording. See [Recording](#recording). Needs `sail.recording` on. | PF, tablet |
| `KitchenSinkWorkflowSailIT` | `sail/kitchenSink/workflow/`: three files (PF tablet, SC, PF tablet) and one `<workflow>` file (PF external, then SC in the same browser) | both |

To run the SAIL unit tests without any browser IT, exclude ITs and give a base
URL that nothing answers:

```bash
mvn -pl skyve-core,skyve-ext,skyve-web,skyve-war -am test \
	-Dtest='org/skyve/impl/sail/**/*Test,org/skyve/metadata/sail/**/*Test,org/skyve/impl/generate/sail/**/*Test,modules/admin/ControlPanel/actions/*SAIL*,sail/SailScriptsUnmarshalTest,util/sail/*Test,!*IT' \
	-Dsurefire.failIfNoSpecifiedTests=false \
	-Dsurefire.rerunFailingTestsCount=0 \
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
| `sail.testStrategy` | the script's | Overrides every script's strategy. Refused by the admin suites and `PrimeFacesSailTestStrategyIT`. |
| `sail.failureDir` | none | Directory for failure screenshots and page sources |
| `sail.snapshotDir` | none | Directory for a screenshot after every step and a film strip of the run; see [Snapshots and video](#snapshots-and-video) |
| `sail.snapshotSource` | `false` | `true` also saves each step's page source with its snapshot |
| `sail.videoDir` | none | Directory for a video of the run (Chrome's screencast) and its player; see [Snapshots and video](#snapshots-and-video) |
| `sail.suite.only` | all files | Admin suites only: comma-separated prefixes of the suite files to run, for example `03,04`. The set-up files and the restore always run. |
| `surefire.rerunFailingTestsCount` | `2` (root pom) | Pass `0` for SAIL runs |
| `forkCount` | `2` (skyve-war pom) | `1` is recommended; `2` is safe, because the server lock serialises SAIL ITs |

**Why there is no default base URL.** SAIL scripts change data. Early in this
work, a run that did not name a server fell back to `http://localhost:8080/skyve/`
and ran an admin script against a developer's own server. That run changed the
admin contact and wrote sign-in and password history. `BrowserConfiguration.getBaseUrl()`
now throws if `-Dsail.baseUrl` is not set, so a run must name its disposable
server.

<a id="one-test-per-server"></a>**One test per server.** SAIL ITs against one server share its data and
its `admin` user. When the PF and SC admin suites once ran at the same time,
they failed with `StaleObjectStateException` on the admin user. Worse, one
suite's backup or restore would undo the other's work.

The skyve-war pom sets `forkCount` to 2, so surefire runs two test classes at
once in separate JVMs. To keep SAIL ITs serial anyway, both test bases take
`util.sail.SailServerLock` for their base URL before the browser starts. They
hold it until the IT class has finished.

- The lock is an operating-system file lock on
  `skyve-sail-<base url>.lock` in `java.io.tmpdir`.
- It therefore works across forked JVMs, and the operating system releases it
  if a JVM dies.
- A test that has to wait prints a line naming the lock file.
- It serialises only tests on the same machine that share a temporary
  directory.

`-DforkCount=1` is still the simplest way to run, because tests then run in
order and their logs do not interleave.

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

**Reruns.** The root pom sets surefire's rerun count from the property
`surefire.rerunFailingTestsCount`, which defaults to 2. Pass
`-Dsurefire.rerunFailingTestsCount=0` for SAIL runs. With reruns on, a SAIL IT
that fails and then passes on a rerun is reported as a flake, and the build
passes. A rerun of an admin suite also repeats its destructive steps.

### Snapshots and video

Two opt-in system properties record what a run did, for a person to look
through afterwards. Both renderers have them. Without the properties, the
executor does no extra work.

```bash
mvn ... -Dsail.snapshotDir=/tmp/sail-media -Dsail.videoDir=/tmp/sail-media
```

- **Step snapshots (`sail.snapshotDir`).** After every step, and when a step
  fails (before the test strategy acts on it), the executor saves a
  screenshot, named by sequence, interaction and step, for example
  `012-Contacts-create-save.png`. With `-Dsail.snapshotSource=true` it also
  saves the page source. The run's `index.html` is a film strip: each step's
  number, interaction, the step as SAIL XML, its outcome (with the failure),
  its duration and its screenshot. Failed steps are highlighted, and a link
  jumps to the first failure.
- **Video (`sail.videoDir`).** The executor records Chrome's DevTools
  screencast (`Page.startScreencast`: JPEG, quality 60, at most 1280×800),
  saving each frame with the time Chrome drew it, and each step's start and end.
  The run's `player.html` plays the frames at their real timing, captioned
  with the step that was running. It has play and pause, a speed of 0.5x to 4x,
  a slider, and a step list to jump to. The screencast uses raw CDP commands
  through Selenium's DevTools, so it does not depend on the DevTools version
  Selenium was built for. Frames are written and acknowledged on a thread of
  their own, so the test does not wait for them. A browser without DevTools
  (not Chromium) has no video; the run carries on and the trace says so.
- **Folders.** Each automation run gets its own folder under each directory,
  named by its start time, the test (`Class.method`) and, in a suite, the
  script file. Suites, workflows and ITs with several tests therefore keep
  every run. Each directory's `index.html` lists the runs, newest first. The
  pages are static files that open from the file system. When both
  properties name the same directory, a run's film strip and player are in one
  folder and link to each other.

Snapshots slow a run by the time each screenshot takes. The screencast
costs little. The pages show a login's password as `********` (a
`{var:...}` password is shown as written). Recordings still show whatever
the browser showed, so a run against real data records real data: names,
addresses, amounts. Keep such recordings out of shared places and out of
the repository.

## The Suites

### Kitchen sink: conformance

The kitchen sink scripts are the renderer conformance suites. They exercise
every op-code that the renderer supports against the `kitchensink` module's
fixtures, with `testStrategy="Assert"`.

`pf-kitchensink-sail.xml` has these interactions:

- sign in failure and success;
- lookup values created and found by `{run}` content;
- ListGridFixture: the same lookup values in a Vue list grid and a PrimeFaces
  data table, chosen by each list grid's `componentBuilderClass` property
  (`vue`, `faces`). The same steps run on each grid: count, select, zoom, New,
  and delete from the zoomed record;
- generated `testDataEnter`;
- `dataEnter` and `testValue` on every KitchenSink widget, with values checked
  again after server round trips;
- data grids: container, ordered and inline grids;
- ActionsFixture: toolbar, form and tab buttons, change events and uploads,
  an upload that is refused, a click low on the page straight after an
  upload (PF), a row's cell and the view's title;
- DialogsFixture: confirmations and server-side questions, answered yes and no;
- EscapingFixture: zoom in and out;
- contexts, links, a map, and `execute`;
- windows, back and refresh: a link into a new window where a lookup value is
  made, a nested window opened by `href`, a PDF in a new window, then back and
  refresh with a `testValue` after each;
- menus;
- switch mode;
- sign out.

`sc-kitchensink-sail.xml` covers the same ground for SmartClient, except
membership and ListGridFixture (`componentBuilderClass` only applies to
PrimeFaces). It also clicks the ActionsFixture's Run From Row link in a row,
declined and confirmed, which PF does in its separate `rowLink` test. The SC kitchen sink also covers ConditionsFixture's visible and enabled
conditions: it enters a value in its slider, then turns Show and Allow off and
on, checking with `testWidget` that a slider and a password field are hidden,
disabled and enabled again. Its list membership (the kitchen sink's only one)
is covered in SmartClient by the admin suite only.

The SmartClient windows interaction also presses the List Grid Fixture's list
grid tools: Deselect, which clears the selected id, and Refresh, which shows
the lookup value made in the new window. PrimeFaces has no such tools, so its
kitchen sink reloads the page instead, and `windowAndToolFailures` checks why
`listGridTool` fails there. Each kitchen sink IT's window failure test checks
the window steps' failures: a link that loads its page in the same window
under `window="new"`, a link that opens a new window without it,
`closeWindow` with no window to close, and `navigateBack` with no earlier
page; and that a window an interaction left open is closed when the
interaction fails.

Both kitchen sinks make their records unique with `{run}`, so they can be rerun
without resetting the database.

Kitchen sink fixtures for the newer steps:

- `KitchenSink.combo` and `KitchenSink.radio` have codes `one`, `two` and
  `three`, and labels `First`, `Second` and `Third`, for `dataEnter match`.
- `LookupDescription` has a `number` that its Bizlet sets from a number
  sequence when a value is first saved. It is the generated value for
  `captureValue`. Its default query, listed by the "Lookup values" menu item,
  has the text column `description` and the number column `number`, for
  `listGridFilter` and `listGridSort`.
- `KitchenSink.lookupDescription` picks from the same lookup values, for
  `lookupDescriptionPick rowContaining` and `lookupDescriptionAutoComplete
  pick`. The kitchen sinks also read a lookup value's number from its list cell
  with `captureCell`.
- ModelLookupFixture's inline data grid `rows` has model lookup description
  columns, `selection` and `singleSelection`, which the form above it also
  has. The SC kitchen sink sets them in a new row, and changes one again
  after `dataGridEdit`.
- `OrderedGridFixture` (the "Ordered Grid Fixtures" menu list) is saved and has
  three ordered collections:
  - `rows`, a data grid whose rows are added through their own view. Both
    kitchen sinks add three rows, `dataGridMove` them, save, and reopen the
    fixture from its list to check each row's position with `captureCell` and
    a `pattern`.
  - `lines`, an inline data grid. The SC kitchen sink adds lines inline, moves
    one, edits one, saves and checks their positions once reopened (the PF
    kitchen sink types into the KitchenSink inline grid's cells, a combo by code
    and by label, and a ModelLookupFixture inline row's lookups; the converters
    scripts type into inline cells of every converter in both renderers).
  - `members`, an ordered list membership of lookup values. `dataEnter`
    assigns members in the order its value names them in SC, and in the
    candidates' order in PF. Both kitchen sinks then reorder them with
    `listMembershipMove` (a drag in SC; PF's move up and down buttons, which
    Skyve shows for an ordered membership), save, and check
    their order once reopened.
- ListGridFixture's "PrimeFaces data table of column types" has a filter
  parameter (`text` not null), so SmartClient completes its criteria in the
  browser; the SC kitchen sink filters its time column there.
  PF does not use that grid.
- ActionsFixture's Upload action refuses a file named `sail-refused.txt` with
  "The file sail-refused.txt already exists" (an `UploadException`), as Upload
  Backup refuses a backup that exists, and one named `sail-invalid.txt` with
  "The file sail-invalid.txt is not a valid upload" (a `ValidationException`),
  and one named `sail-unavailable.txt` as an unavailable action ("This action
  is no longer available."), for `upload testSuccess="false"`. The upload page
  shows the first two as the upload's problems, and the third as the refusal of
  an unavailable action. A file it takes adds a growl, "Actions Upload took the
  file", which the upload page carries to the view as it closes.
- ActionsFixture's rows grid has a Run From Row link in each row, whose action
  asks for a confirmation, for `dataGridLink`.
- ListGridFixture shows `selectedVueId`, the selected id of its first list grid,
  for `listGridTool tool="clearSelection"`. It has two blurb links with
  `target="_blank"`, for `navigateLink window="new"`: "Open a new lookup value in
  a new window", and "Open a PDF in a new window", which opens the kitchensink
  module's resource `sail-new-window.pdf`. Chrome shows a PDF in the new tab,
  headless or headed, and the tab closes like any other.
- `ListAttributes` has dates and times on converters other than the defaults
  (`monthFirstDate` MM_DD_YYYY, `twelveHourDateTime` MM_DD_YYYY_HH_MI,
  `monthNameTimestamp` MMM_DD_YYYY_HH_MI_SS, `twelveHourTime` HH_MI_SS) and a
  date time shown as a date (`dateTimeAsDate`, MM_DD_YYYY_DateTime; its Bizlet
  keeps it equal to `dateTime`), listed by ListGridFixture's "Vue list grid of
  column types". The PF kitchen sink filters each one with values written as
  their cells show them, including `05/06/2026` against a `06/05/2026` that
  would match if day and month were swapped, and `dateTimeAsDate` by day with
  each operator; both recorder round trips record and replay such filters
  unedited.

`KitchenSinkWorkflowSailIT` is the conformance for [workflows](#workflows):
PrimeFaces makes a lookup value and keeps its number, then SmartClient finds it
by the number and `{run}`, and so on back to PrimeFaces.

**Converters.** `kitchensink.ConvertersFixture` has an attribute for each of the
60 converters (`ConverterName`), named after it in lower camel case
(`DD_MMM_YYYY_HH24_MI` is `ddMmmYyyyHh24Mi`) and labelled with the converter's
name, of the type the converter takes, and `plainDecimal2`, a decimal with no
converter. Its edit view groups them (dates, date times, timestamps, date times
and timestamps shown as dates, times, numbers) in bordered forms, each with an
inline data grid of `ConvertersFixtureRow` (the same attributes), and a query
per group (`qConvertersDates`, `qConvertersDateTimes`, `qConvertersTimestamps`,
`qConvertersDateOnly`, `qConvertersTimes`, `qConvertersNumbers`) lists its
columns: the list views are the PF Vue list grid and the SC list grid, and
`ConvertersDataTables` shows the same queries as PrimeFaces data tables. Its
menu list, "Converters Fixtures", shows a few non-default converters and
`plainDecimal2` (the recorder round trips' sample). `ConvertersFixtureBizlet`
stores the absolute converter's value negated when a fixture is created with a
name that ends with " negative", once, so that the scripts check both how a
stored negative is shown and what a later save without typing stores: the record
is saved again with OK, which stores its magnitude in PrimeFaces (the absolute
column's `lessThan 0` filter finds no row) and keeps the negative in SmartClient
(it finds the record, as does `equals -7.50`).

`PrimeFacesConvertersSailIT` and `SmartClientConvertersSailIT` run a script per
group (`pf/pf-converters-<group>.xml`, `sc/sc-converters-<group>.xml`): records
a, b and c (and d and e in the numbers group) are typed into every field as a
user types them, and into an inline grid row with the next record's values (so
that a cell's value differs from its field's); both are checked with `testValue`
after the save and once reopened from the list; then each list grid is filtered,
column by column, with values written as the cells show them, and the rows each
filter leaves are counted and named. The values catch a day and month swapped (a
is 05 Jun, b 06 May), a day above 12, 12 hour times at PM, noon and midnight,
month names, seconds, negatives, separators, `$` (typed, not shown), percentages
typed with and without `%` and over 1000%, rounding past the shown places,
durations rounded to the minute (`0:20` for 0.33333 hours, `0:29` for 0.49166,
`2:30` for 2.4999, `-0:01`, `1:00` for 0.99999, typed with decimal minutes), the
absolute converter's stored negative shown as its magnitude, and values half way
between two shown values (1.005 is `1.01`, -2.345 is `-2.35`, 0.12345 is
`12.35%`), a negative that rounds to zero (`0.00`) and the largest whole number
a browser holds exactly (9,007,199,254,740,991). `ConvertersFixtureBizlet`
stores the values a field does not keep when they are typed, for a fixture whose
name ends with " half way". The rules they check are in [Converters](#converters).

### Converters

The rules the converters scripts check, and what changed for users:

- **`Decimal2DollarsAndCentsAbsolute`** is for entering positive dollars, or for
  showing a magnitude whose sign the label gives (an amount due, a balance
  carried forward). Its fields show a value without its sign in every renderer.
  In an editable field, a stored negative is saved as its magnitude: in
  PrimeFaces on any save of the form, and in SmartClient once the field has been
  focused (a click into it and out again is enough); a save in SmartClient
  without focusing the field keeps the negative. Show a signed amount that must
  keep its sign in a read-only field, or with the display-only `Absolute*`
  formatters (`FormatterName.AbsoluteTwoDecimalPlaces` ...). Its list column is
  filtered by signed dollars in SmartClient (`-7.50` is -7.5).
- **`Decimal5TimeDuration`** shows hours and minutes: the hours truncated, the
  minutes rounded once, half up, from the exact fraction of an hour (0.49166 is
  `0:29`, 2.4999 is `2:30`, -0.01 is `-0:01`); a value that rounds to no minutes
  is `0:00`. It reads hours, a colon and minutes, each a decimal number (`1:75`
  is 2.25 hours, `-0:30` is -0.5); `1.5` (no colon) is not a duration, in any
  renderer.
- **The other number converters** show the value rounded once, half up, from the
  decimal (1.005 is `1.01` in every renderer, not the `1.00` of its binary
  fraction), grouped in thousands, with no sign on a value that rounds to zero
  (`0.00`, not `-0.00`); a percentage converter shows the value times 100 with a
  `%` (`1,250%`), `SimplePercentage` the whole number of percent without
  grouping (`1000%`). They read the whole text, trimmed: an optional `-`, ASCII
  digits with optional `,` grouping between them and an optional `.` with
  places; a percentage with or without a `%` at its end (rounded to the places
  shown); dollars with or without a `$` before the number or between a `-` and
  it (`$-1.00`, `-$1.00`); whole numbers also an optional `+`. Anything else
  (`12abc`, `1E3` or `1e3`, `12%3`, `1$2`, a `,` not between digits such as `1,`
  or `,1`, other digits than 0-9) is refused in every renderer. A number typed
  with more places than shown is kept to the type's scale by the converter, then
  stored as shown when its form is saved (see
  [Known Limitations](#known-limitations)). The symbols are English whatever the
  JVM's locale. A BizPort import with one of these converters also reads the
  number at the start of a cell (`1,234.50 kg` is 1234.50), as it did before.
- **User-visible changes in SmartClient:** 12 hour times show `AM`/`PM` in upper
  case for every locale, as the converters write them and PrimeFaces' pickers
  show them (`02:47 PM`, not `2:47 pm`); a duration field refuses decimal hours
  without a colon (`2.5`), as PrimeFaces does; a date-only date time field keeps
  a value's time unless a different day is typed; a number the filter row cannot
  read shows an error instead of being dropped; the absolute dollars column is
  filtered by signed dollars; a list with a date-only column fetches from the
  server when its filter changes.
- **User-visible change on the server:** the number converters (and
  `DecimalFormatter`, so the number formatters) write and read English numbers
  whatever the JVM's locale. Before, a German JVM showed `1.234,50` and read
  `1.5` as 15 and `12.5%` as 1.25 (while SmartClient and the Vue list grid
  showed `1,234.50`), and whether `1E3` or `1e3` was read as 1000 depended on
  the locale.

### Admin: application suite

The admin suites test the admin module as an application, through its menus,
as an administrator would use it. The PF suite is `sail/admin/pf/suite/`, run by
`PrimeFacesAdminSuiteSailIT`:

| File | Role | Covers |
|---|---|---|
| `00-precondition.xml` | set up | No "SAIL restore marker" data group is left over from an earlier run |
| `00-backup.xml` | set up | Clears the completed jobs list. Backs up with no redaction, waits for the backup job, and leaves the restore marker data group. |
| `01-session.xml` | suite | User Dashboard and Update My Details; sign in with a bad password, an unknown user and the right one; sign out and in; Update My Details again |
| `02-contacts-communications.xml` | suite | Contact create, find, change and delete, with an image upload and a declined delete; Communication Templates; Communications: every tab, attachments, and the send actions' refusals |
| `03-security-admin.xml` | suite | Data Groups; Groups (roles through list membership); the User wizard; the new user signs in, sees a role-filtered menu and changes their password; Sign In History; Security Log; Configuration tabs; Mail Log |
| `04-devops.xml` | suite | Control Panel (every tab, a query, cache statistics, generated SAIL); Data Maintenance (the backup list, import/export); Document Creator; Monitoring Dashboard |
| `05-reports-misc.xml` | suite | Reports, Audits, System Dashboard, Document Numbers, Jobs |
| `98-destructive.xml` | suite | Configuration saved with safe values (`mailBogusSend` on, so mail is only logged); a mail send that is refused; generate tagged test data, check that its job cannot be rerun, and delete the data; delete completed jobs (declined, then confirmed) |
| `99-restore.xml` | restore | Restores the backup (declined, then confirmed) and waits for the restore job. Signs in again, checks that the marker has gone, deletes the backup, and uses Update My Details. |

The SC suite, `sail/admin/sc/suite/` run by `SmartClientAdminSuiteSailIT`,
mirrors it file by file and interaction by interaction, with these differences:

- The marker is a set-up file of its own, `00-marker.xml`.
- It covers the menu items and actions that only SmartClient has, among them
  Heap Dumps, Report Manager, Snapshots and Tags. Its 98 also drops the
  content index and reindexes, and truncates the audit log.
- Its last suite file, `99-truncate.xml`, first checks that exactly one backup
  is listed. It then truncates all data, keeping the bootstrap user, and checks
  the result.

**How `SailSuite` runs a suite.** Both ITs run their files with
`util.sail.SailSuite`, in one browser session. Each file is its own automation
and signs in for itself.

1. The set-up files run first, in order.
2. The suite files run in order. The run stops at the first file that fails,
   because later files build on earlier ones. Dangerous actions run in 98 (and
   SC's 99-truncate), after everything that depends on the data they change.
3. The restore file runs last, even after a failure, including a failure of
   the truncate. It runs only if the set-up files it requires have passed:
   `00-precondition.xml` and `00-backup.xml`, in both suites.

   If a required file did not pass, the suite prints that the restore was held
   back. The decision depends on which files passed, not on message text.
4. The test then fails with every outcome: the first file that failed and, if
   it failed too, the restore.

`-Dsail.suite.only=03,04` runs only the suite files with those prefixes. The
set-up files and the restore always run, so the bracket stays intact. The
suites refuse `-Dsail.testStrategy`.

**The backup/restore bracket.** 00-backup backs up the data and 99-restore
restores it, so a suite leaves the server as it found it and can be run again.

**Which backup is restored.** The backups list shows the newest backup first,
and 99-restore restores row 0, the backup 00-backup made. Other backups can be
on the server: they are older, so they are not restored. A backup made by
something else while a suite runs, such as a scheduled backup, would be newer
and would be restored instead, so run the suites with the job scheduler off,
as the test servers are. 99-restore deletes row 0 after the restore, and checks
the delete by the backup being deselected, as other backups may be listed. The
restore is held back unless 00-backup passed, so it never restores a backup
that is not this run's. (Until a fix in 2026-11, the list showed the
oldest backup first, and 00-precondition asserted that no backup existed.)

**The leftover-marker precondition.** 00-precondition asserts that no restore
marker is left over. A marker is left when a run was interrupted, or its
restore failed, so the data may hold what that run did. If it fails, check the
data (or restore that run's backup), delete the marker by hand, and run again.
The suite asserts rather than restoring or deleting, because a test should not
decide what someone's data should be. For the same reason, the older
`pf-admin-sail.xml` makes no backup.

**Waiting for jobs.** Backup, restore and test data generation run as jobs.
The suites wait for each job with `testGridRows ... timeoutMillis` on its row
in the completed jobs list (`qCompletedJobs`). To make the rows of this run
distinguishable, 00-backup clears the completed jobs list first, so the
server's earlier job history is lost. No `<pause>` is used.

The admin suites must never lock `admin` out. They run no `testDataEnter` on
Configuration, change passwords only for the user they create, and back up with
data sensitivity "none". A backup with any other data sensitivity redacts every
password, by design, so after its restore `admin` could not sign in until its
password was reset.

With each suite deleting its own backup in 99-restore, the PF and SC suites can
run one after the other, in either order, against the same server.

## Synchronisation Rules

Timing problems show up as flakes, especially in SmartClient and in headed runs.
The harness follows these rules, and changes to it must keep them.

1. **No sleeps or fixed delays.** Every wait waits for a named asynchronous
   event, such as:
   - a page or ajax response;
   - the PrimeFaces ajax queue emptying;
   - a Vue list grid being mounted in its container, and its fetch ending (its
     paginator spinner shows from the fetch's debounce until the rows arrive);
   - SmartClient having no pending RPC or deferred timer;
   - a widget being drawn;
   - a dialog opening or closing;
   - an animation ending;
   - an upload finishing;
   - a new window opening and loading its page, a back or reload replacing
     the page (the page is marked with a token first, so the new one can be
     told from it), and a closed window's opener settling again.

   In PF, after a confirmation is accepted, the executor waits for the request
   it sends to start, because the page can look idle in between.

   SmartClient pairs two clicks on the same widget within its double-click
   interval into a double click, which silently swallows the second click.
   A click therefore waits for that interval to pass, but only when it would
   pair with the previous click. The only `Thread.sleep` in the interpreted
   harness is behind the `<pause>` op-code. (The older `PrimeFacesSelenium`
   class, used only by the legacy generated tests, has its own.)
2. **One condition-wait mechanism.** All waits go through `Selenide.waitFor(...)`
   (a `WebDriverWait` polling every 100 ms), or Selenide's `should(...)`. Both
   are bounded by `Selenide.TIMEOUT` (`-Dsail.timeoutSeconds`). The only other
   polling interval is the one an author asks for with `testGridRows@timeoutMillis`,
   which checks every second. The one deliberate wait outside this mechanism is
   `SailServerLock`, which blocks while another JVM's SAIL test is using the
   same server.
3. **No script clicks for user gestures.** A click goes through WebDriver, after
   the target is scrolled to the centre of the window and nothing covers it. In
   a Vue list grid, the click also waits for the target to be in the same place
   twice in a row: the grid lays its toolbar out again over the next animation
   frames when it is shown, for example when its tab is selected. A
   click a user could not make fails the step, with a diagnosis: the element,
   its position, the window, and what covers it. A script click would hide
   exactly the faults SAIL exists to find. Two pointer gestures are named
   per-widget cases, and both are real pointer actions, not scripts:
   - **PF side bar toggle.** The editorial side bar toggle hangs outside its
     off-canvas side bar, so WebDriver reports it as not interactable,
     although it is drawn on top. It is pressed with a pointer click at its
     drawn centre, after `elementFromPoint` confirms that the toggle is the
     topmost element there (`PrimeFacesSelenide.clickToggle`).
   - **SC hover.** When SmartClient's hover covers the target, the pointer
     is first moved onto the target, which hides the hover, as a user's pointer
     would. The click is then made.

PF drives every widget with real gestures. The slider is dragged, then
adjusted with the arrow keys. A colour picker is typed into and left with Tab
(a click would open its popup). Pick list items are clicked, then Add or Remove.
Both executors use a few script calls that are not gestures:

| Renderer | Script call | Why |
|---|---|---|
| both | Scrolling: DOM `scrollIntoView` (PF also `window.scrollBy`); SC `Canvas.scrollIntoView()`, `ListGrid.scrollToRow()` and the menu tree's `scrollToRow()` | Bring the target to the centre before a real click |
| both | Reloading a grid for `testGridRows timeoutMillis`: PF the data table's `filter()`, or for a Vue list grid setting its `skyveConfig.renderedAt`, as an ajax update does; SC `BizListGrid.refresh()` or `invalidateCache()` | Refetch the rows from the server, as the author asked |
| SC | `Slider.setValue()` after the thumb is scrolled into view and checked | The widget's own API, which fires the same change events as a drag |
| SC | Selecting existing text before typing: `select()`, a DOM range in rich text, `execCommand('selectAll')` in the HTML editor | So that typing replaces the text |
| SC | `blur()` when SmartClient keeps the focus after Tab | The field must be left before the next click |
| SC | `FormItem.updateValue()` / `RichTextItem.storeValue()` | Confirm that the typed value reached the form and its ValuesManager. This is a no-op when it already has. |

Scripts follow the same rules. Use `testGridRows timeoutMillis` or
`testMessage` to wait for a server job, not `<pause>`.

A headed run must not share the screen with another headed browser: losing the
OS focus mid-step closes SmartClient's pick lists and makes it select a field's
text when the focus returns. The harness does not try to hide this; a failure
caused by it says so (see [Lost OS focus](#test-strategies-and-failure-reports)).

## Recording

The SAIL recorder writes a script of what a user does in SmartClient or PrimeFaces. The script is a first draft: an
editor (a person or an AI) adds assertions, puts `{run}` into values that must be unique, and removes steps that do not
matter. The plan it was built to is [plan_sail_recorder.md](ai/plan_sail_recorder.md).

### The gate

A session records only when all three of these hold. When any fails, the server sends no recorder code: the pages are
byte for byte what they are without the recorder.

1. **Configuration.** `sail.recording` in `skyve.json` is `true`. It defaults to `false`, and an absent `sail` section
   means `false`. Recording is allowed in production (a null `environment.identifier`), but Skyve then logs a warning at
   startup, and Start warns the user that the script will hold the data they see and type.
2. **Role.** The user has `admin.SAILRecorder`. The role is separate from DevOps: it gives the Control Panel menu item
   and only its recording actions. A user without DevOps sees only the Control Panel's Recording controls.
3. **Started.** Recording was started for the session in the Control Panel, and has not been stopped.

`sail.maximumSteps` (default 5000) caps the steps one recording keeps. When a recording is full, the recorder stops and
tells the user to stop and download it; nothing is lost silently.

### Start, stop and download

1. Open **DevOps / Control Panel**, select the **SAIL** tab, and press **Start** in the Recording box. In SmartClient
   the recorder loads into the page at once, without a reload; in PrimeFaces the page loads again, with the recorder
   (the reload is not recorded).
2. Use the application. Every page of the session loads the recorder until recording stops, and shows its panel.
3. Press **Stop** in the panel, or in the Control Panel. The recorder sends its last steps first, then the server
   checks and builds the script. Going back to the Control Panel to stop is left out of the script.
4. Press **Download** (the panel offers it as soon as recording stops) for the script,
   `sail-recording-<yyyyMMdd-HHmm>.xml`. It can be downloaded again until the next recording starts or the session ends.

The recording lives only in the server session. It is never written to the database, and signing out or the session
expiring discards it, so download it first. The panel asks before the header menu's Logout signs out while recording,
and offers to stop and download first.

### The panel

While recording, each page shows the recorder's panel. It is drawn in a Shadow DOM root, so the application's styles
and the panel's cannot affect each other, and nothing done in the panel is recorded or reaches the application.

- **Collapsed**, it is a small bar in the bottom right corner (in PrimeFaces, 76 pixels from the right edge, clear
  of a scrolled list's floating New button), which can be dragged elsewhere: a red dot, the number of recorded gestures, **Check**, **Note**, **Stop**,
  and a control that expands it.
- **Expanded**, it slides out on the right and shows the script as it is written, newest at the bottom, with the context
  comments greyed. Scrolling to the top loads earlier entries. The script is read-only: each gesture has a delete
  button, **Undo** removes the last gesture (with the context recorded after it), and **Copy** and **Download** take the
  script as it stands. Edit it later, in the IDE or with AI.
- **Check** turns Check mode on: the field, message or grid under the pointer is outlined, and a click adds a check
  instead of reaching the application. **Esc** turns it off. An alt-click adds a check without turning Check mode on. A
  short notice confirms each step added. Checks go into the script where they are added:
  - a field adds `testValue` with the value it shows, written as `dataEnter` would enter it (a combo's code; in
    SmartClient, a list membership's members, `|`-separated, in order); a field showing a validation error adds
    `testMessage contains` with the error instead;
  - a growl or a dialog's message (an `isc.say`, `isc.warn` or the Problems dialog) adds `testMessage contains` with
    its text;
  - a grid row adds `testGridRows rowContaining` with the row's bizKey or most distinctive cell, and `minRows="1"`;
    elsewhere on a grid, `testGridRows` with `minRows` and `maxRows` set to the rows it shows.
- **Note** opens a small input in the panel: the text becomes a `<comment>` step.
- The panel's state (expanded or not, where the bar is, the scroll) is kept in the browser tab's `sessionStorage`.
  PrimeFaces loads a page per view: each page comes back with the panel as it was, without its slide-in, from that
  state and from the recording's state, which the page carries (see [How it works](#how-it-works)).

### What is recorded

A SmartClient recording has `uxui="desktop"` and `userAgentType="desktop"`; a PrimeFaces recording has the UX/UI and user
agent type the user got (for example `uxui="tablet" userAgentType="tablet"`), so that it replays through the PrimeFaces
executor. The script has one step per line, and each element's attributes in a fixed order. Its `<before>` signs in as
the user who recorded, then switches mode (to SmartClient for desktop; in PrimeFaces, when PrimeFaces already shows, it
does nothing):

```xml
<before>
	<login user="admin" password="{var:password}"/>
	<switchMode/>
</before>
```

The password is never recorded. The login takes it from the variable `password`, so a test runs the script as
downloaded by defining the variable, with `variable("password", "...")` on the test base (see
[The run token and variables](#the-run-token-and-variables)); or an editor replaces `{var:password}`. The login names the
customer only when the sign in page asks for one (no `environment.customer`).

| Gesture | Step |
|---|---|
| A menu item | `navigateMenu`, the item's shown labels with its groups, for example `DevOps/Control Panel` |
| A page loaded with `a=e` or `a=l` in its URL (a link, a typed URL) | `navigateEdit` (with `bizId` from `i`) or `navigateList` |
| The browser's Reload, or Back | `refresh` or `navigateBack`, read from how the page that loads was loaded (Navigation Timing's `reload` or `back_forward`; the Navigation API's activation says back from forward), followed by `pushEditContext` or `pushListContext` for the view the page's URL names, since the executors keep the context as it was (refresh) or drop its document (back). In SmartClient, menus, zooms and New do not change the URL, so Back returns to the last full page load (a navigate step, a link or a sign in) and Reload shows the URL's view, and that is what is pushed. Forward, or a move through the history whose direction the browser does not tell, is an unmapped comment before the page's `navigateEdit` or `navigateList`. |
| A link that opens a new window (a `target` other than `_self`, `_top` or `_parent`, or a `window.open` from the link) | `navigateLink window="new"` with `text`, the link's visible text, when it finds the link (the first displayed link whose text contains it; with `exact="true"` the first whose text equals it), otherwise `href`, the URL relative to the application. Recording goes on in the new window if it shows a Skyve page (the recording is the session's): its first page is not loaded again, but becomes `pushEditContext` or `pushListContext`. A new window that no recorded link opened (a typed URL, the browser's "Open in new tab", a window a button opened) is `navigateLink href window="new"` with its first page's URL, opened from the window in front. |
| Closing a new window, or coming back to the window that opened it | `closeWindow`, once for each window closed, before the first step made in (or the "in front again" signal of) a window under it. Each step carries its browser window's id, so a step from the opener is a reliable sign that the user left the windows above it; a window that opened one also says when its tab shows or it gets the focus again, which covers coming back from a page that records nothing (a PDF) and then stopping. A step made in a window the script has closed, or in one the recording did not see open, is an unmapped comment: SAIL cannot switch windows. |
| Typing into or changing a field | `dataEnter` with the final value, written before the next step or server response. Combos and radios record the option's code with `match="code"`; check boxes `true` or `false`; dates, times and numbers the text as shown (SmartClient: `12.50%`, `1,234.56`, not the field's value 0.125 or 1234.56); rich text its plain text. New lines are written as `&#10;`. |
| Selecting a tab | `tabSelect`, with `::` for nested tabs. Selecting the tab already shown records nothing. |
| A view's buttons | `ok`, `save`, `cancel`, `delete`, `zoomOut`, `remove`, or `action name="..."` for the view's own actions. A declined confirmation adds `confirm="false"`. |
| A list grid's New, or zooming into a row | `listGridNew`, `listGridZoom`. A row is named with `rowContaining`: its bizKey when a cell shows it, otherwise the cell the fewest loaded rows show, the leftmost on a tie; `row` only when no text finds the row first. The list view's own grid is named by the context; a grid in a view by `module` and `query` (or `document` and `model`). In PrimeFaces, both a Vue list grid (its New, its View Detail or a zoomable row) and a PrimeFaces data table. |
| Selecting a list grid row | `listGridSelect`, for a list grid whose selection does something (a `selectedIdBinding` or a selected action); a click on another list grid's row is not a gesture. In SmartClient the selection is written only if the next step does not act on the same grid. |
| Filtering, clearing the filter of, or sorting a list grid | `listGridFilter` for each column whose filter changed (no `value` for a column the user cleared), `listGridClearFilter` when the user cleared them all, `listGridSort` with `descending="true"` for a descending sort. `operator` is written unless it is `contains`; an enum's value is its label; a date, time or number as the column shows it (its converter's format: `06/05/2026 02:47 PM`, `12.50%`, `1,234.56`); in PrimeFaces, a number column's filter as typed. SmartClient: the filter row and the advanced filter; PrimeFaces: a Vue list grid's column filters and headers, and a PrimeFaces data table's column filters (typed text, a constant domain's combo by its label, a boolean's check box) and headers, whose operator is the one its filter makes (`contains` for text, `equals` otherwise). A data table sends a request for each change (a typed filter at each pause, a check box filter for each state it passes through, a header for each click), so its filters and its sort are written as they end up, once the user goes on to something else. A filter SAIL cannot write as column filters ANDed together (an OR, two clauses on a column, an operator SAIL has not) and a sort by more than one column are unmapped comments. |
| A data grid's New, zoom, remove, select or row edit | `dataGridNew`, `dataGridZoom`, `dataGridRemove`, `dataGridSelect` and (SmartClient's inline grids) `dataGridEdit`, named by `binding` and the row as for list grids. A selection followed by a step on the same grid's row is written as that step only. |
| Typing into an inline data grid's cells (SmartClient) | `dataEnter` into the grid's columns, after the `dataGridNew` or `dataGridEdit` that started the row's edit. An inline lookup's pick is `lookupDescriptionPick`. |
| Dragging an ordered data grid's row | `dataGridMove` with the row and its new position, `to`. |
| The list grid toolbar's Refresh or Deselect (SmartClient) | `listGridTool` with `tool="refresh"` or `tool="clearSelection"`, naming the grid as the other list grid steps do. The same items in the grid's context menu are unmapped comments. |
| Changing a list membership's members (SmartClient) | `dataEnter` with the members' descriptions, `\|`-separated, in the order shown, written as the final value like a field's, whether the user used the assign and unassign buttons or dragged members between the lists. In an ordered membership, dragging a member to another position is `listMembershipMove` with the member (`rowContaining` its description, or `row` when an earlier member's description contains it) and its new position, `to`; a member dropped in from the candidates anywhere but last is also moved there after the `dataEnter`, which adds new members last. Removing every member is an unmapped comment (SAIL cannot enter an empty value). |
| A link in a data grid's row | `dataGridLink` with the row and the link's text; a declined confirmation adds `confirm="false"`. |
| Picking from a lookup | `lookupDescriptionPick rowContaining` the description picked; when the user typed into the lookup first, `lookupDescriptionAutoComplete` with `search` (what was typed) and `pick` (the description picked). Never a row number, and never `dataEnter`. SmartClient's New and Edit on a lookup are `lookupDescriptionNew` and `lookupDescriptionEdit`. |
| Uploading a file | `upload` with the content widget's `binding` or the upload `action`, and `file="sail/files/<the file's name>"`, with a comment saying the editor supplies the file there (the test classpath). An upload the page refused is written with `testSuccess="false"`, and the refusal as context. |
| Answering a server-side question No or Cancel | `confirm="false"` on the step that raised it. |
| A tree, map or calendar menu item, or such a page loaded by URL | `navigateTree`, `navigateMap` (with `geometryBinding`) or `navigateCalendar`; PrimeFaces has a map page only. What is done inside the tree, map or calendar is unmapped. |

In PrimeFaces, data entry is read from the fields the page's map names: what a field shows when the next step is taken,
or when the next ajax request is sent, is compared with what it showed when the page loaded or the last response
arrived, and each change becomes a `dataEnter` with the final value, in page order. A check box that can be empty keeps
its empty, true or false state. A widget the PrimeFaces executor cannot enter data into (a geometry, a comparison) is an
unmapped comment for its change.

**Values the application made.** When a value the user types later (or filters by, or searches a lookup for), or the
text a later step finds a row by, equals a value the application showed earlier and the user did not type or choose,
the script captures it where it was shown and uses `{var:name}` later: `captureValue` after the response that showed
it in a field, `captureCell` before the zoom or select of the row that showed it in a cell, or `captureTitle` before
the step on the view whose title it was. For example, a lookup value's generated number, shown after the save, then
typed into the list's filter:

```xml
<save/>
<captureValue binding="number" variable="number"/>
<ok/>
<listGridFilter column="number" value="{var:number}" operator="equals"/>
```

A value counts when it is not blank, not `true` or `false`, and has a digit or at least 4 characters; the latest value
shown wins. The variable is named after the binding or column (its last part), or `title`, numbered if the name is
taken. No `pattern` is written: the whole text is captured. The script is built this way on Stop
(`SailRecordingCaptures`).

The script also carries context for whoever edits it, as XML comments:

- a header: who recorded it and when, the password note, the user's effective roles (groups expanded) written as
  `viewRoles("user", "module.Role", ...)` ready to paste into the test, and the UX/UI the user got;
- a heading whenever the screen changes: module and document (with its name), the edit or create view, and the
  renderer, or the list's data source;
- before each `dataEnter`: the field's label and the value it showed before the change;
- before each `listGridZoom`: the row's bizKey and the values its cells showed;
- after each step: the growls, messages, problems and field errors the response showed (`shown: ...`), and the bound
  values the response changed on screen, without the user's own entries (`changed on screen: ...`).

An action whose response showed errors (a problem, field errors, an error message or growl, or validation in the
browser that stopped the request) is written with `testSuccess="false"`, so that its replay does not expect success.

When recording stops, the server checks each step against the metadata, building the views as the recording user
with the same locators the SmartClient executor uses: a data entry's binding, an action and a tab must be on the
screen's edit or create view, and a row named by text must have shown that text. A step that fails becomes an
`<!-- unconfirmed: ... - why -->` comment instead; it is never dropped.

Anything else is written as an `<!-- unmapped: ... -->` comment where it happened, so the editor sees the gap: a click on
a control that records no step (a list grid tool other than Refresh and Deselect, a link outside a grid that stays in the
window, a header menu item), clearing a field (SAIL
cannot enter an empty value), and typing into a password field (passwords are never recorded). A gesture is never
dropped silently.

### How it works

- SmartClient: `desktop/home.xhtml` adds `skyve/sail/recorder-sc-min.js` after `skyve-min.js` only through
  `#{desktop.sailRecording}`, which checks the gate. The Control Panel's SAIL tab loads the same bundle when Start's
  response says the session records. The bundle is built by `skyve-web/build.xml` (`compress`) from
  `skyve-web/src/js/sail/recorder-sc.js`, and is never merged into `skyve-min.js`.
- Skyve's client code has no recorder checks. The recorder wraps a few of Skyve's SmartClient methods when it loads
  (each is marked in `skyve-web/src/js/desktop` with a comment) and listens to SmartClient's `ValuesManager.itemChanged()`
  and to the DOM in the capture phase. It detaches when recording stops. A date, time or number filter value is
  written with the column type's `shortDisplayFormatter`, as the grid's cells show it.
- It sends its steps as JSON batches, one request at a time through SmartClient's RPC manager, to the `sailrecording`
  endpoint (`org.skyve.impl.sail.recording.SailRecordingServlet`). Each page gets the recording's CSRF token from the
  endpoint (issued once per recording, so the session's tokens do not grow with the pages), and the endpoint checks
  the gate on every request. Steps the page has not seen acknowledged are sent again from a beacon as it
  unloads, and the server adds each step once. `GET sailrecording?from=i&count=n` returns a range of the steps.
- On Stop, `org.skyve.impl.sail.recording.SailRecordingBuilder` builds the `Automation`, with
  `SailRecordingChecks` checking the steps, and writes its XML.
- The bundle is the shared core (`recorder-core.js`: the panel, Check mode, the step model, the requests and uploads)
  followed by the SmartClient adapter (`recorder-sc.js`); the PrimeFaces bundle reuses the core. The core's
  `SKYVE.SailRecorder.mount(container)` puts the panel into a container instead of the page's corner.
- The panel reads the script back in ranges (`GET sailrecording?from=&count=`), and deletes, undoes and stops with the
  same batches the steps go in (`delete`, `undo`, `stop`). `GET sailrecording?format=xml` gives the script as it stands,
  and with `download` as a file.
- `GET sailrecording?roles` returns the signed-in user's effective roles whenever `sail.recording` is on, for any
  signed-in user (no recording or SAILRecorder role needed): the harness uses it (see [The test user](#the-test-user)).
- PrimeFaces: `skyve/sail/recorder-pf-min.js` is the same core followed by the PrimeFaces adapter (`recorder-pf.js`).
  Everything a page adds goes through the same gate, which every page bean inherits from `LocalisableView`, inside a
  `c:if` with no text of its own, so a page whose session is not recording is byte for byte what it was:
  - each theme's `head-resources.xhtml` adds the bundle after `skyve/prime/skyve-min.js` (`#{bean.sailRecording}`);
  - `external/edit.xhtml` and `external/list.xhtml` render, inside the form, a hidden `#sailRecorderMap` span with the
    page's client-id map, so that an ajax update of the form brings the map of what the view now shows; and, after the
    form, a hidden `#sailRecorderState` span with the recording's state: its CSRF token, its counts and its last 50
    entries (`FacesView.getSailRecordingState()`), so that the recorder and its panel start with no request.
  - `org.skyve.impl.sail.recording.PrimeFacesRecordingMap` builds the map: each element's client id (a data table's
    row index left out) and what a step calls it (a binding and widget kind, an action, a tab path, a grid and its
    controls), and each grid's columns (a Vue list grid's projected columns, its date and time columns with their
    converter's format pattern, `format`, in which the recorder writes their filters). It uses the executors' own mock
    render, as the signed-in user, under a mock Faces context. `FacesView.getSailRecordingEditMap()` and
    `getSailRecordingListMap()` build it only while the session records.
  - The adapter listens to the DOM in the capture phase and to PrimeFaces' `pfAjaxSend` and `pfAjaxComplete` events:
    data entry is flushed before each request, and each response's messages, growls, field errors and changed values
    become context. A gesture that loads the next page (a menu item, a list's New or zoom, OK) marks it in
    `sessionStorage`, so the load is not recorded again as a navigation. A button that asks for confirmation holds
    its step until the confirm dialog is answered.
  - Its requests to `sailrecording` go through jQuery, so that a page's own ajax wait (and the PrimeFaces Selenide
    harness's) covers them.
  - Lookups are recorded from their drop-down's items, data table drags and selections from the PrimeFaces ajax request
    they send (`rowReorder`, `rowSelect`), a Vue list grid's filters and sort from the grid's own state each time it
    fetches its rows (`smartlist`), and a data table list grid's filters and sort from its filter and sort requests: its
    column filters as the page shows them, and the sorted column's header id, which the map's columns name (with the
    comparison each column's filter makes).
- Windows, in both renderers: the core gives each browser window an id in its sessionStorage (a new tab starts
  without one), which every step carries; it records a link into a new window on its click (or its `window.open`),
  marks a page load with how it was loaded and whether it is a window's first page, and a window that opened one says
  when it is in front again (`visibilitychange` or `focus`). On Stop, `SailRecordingWindows` turns these into the
  window, back and refresh steps and the context pushes; the panel shows each entry as it will be written.
  - The PrimeFaces Control Panel (`views/external/edit.xml`) has the Recording controls, as the desktop one does.
- Uploads, in both renderers: the upload page (`upload.xhtml`) opens in an iframe, and the core listens to its file
  upload, naming the binding or action from the page's URL. The PrimeFaces recorder does not start in a frame.
- `SmartClientRecorderRoundTripSailIT` and `PrimeFacesRecorderRoundTripSailIT` each record kitchen sink and admin
  contacts workflows, a second kitchen sink workflow of grids, lookups, uploads, list filters, sorting and selection,
  a captured number and server-side questions (`moreGestures`), and checks, notes, a delete and an undo through the
  panel, and replay each recording unedited through their renderer's executor (PrimeFaces as `tablet`). They also
  check that pages are unchanged unless the session records; the PrimeFaces one also checks the sign out warning.

### Limits

- Not recorded, as unmapped comments: what is done inside a tree, map or calendar; PrimeFaces list memberships;
  PrimeFaces inline data grid cells (their inputs are not in the page's map); list grid tools other than SmartClient's
  toolbar Refresh and Deselect (export, chart, tags, snapshots, and the grid's context menu); the browser's Forward
  button; gestures in a window the script has closed or did not see open (SAIL cannot switch windows). A SmartClient
  lookup's pick from the Pick button's pop-up list is recorded as a drop-down pick.
- Steps are written in the order the server receives them, except that a link into a new window is written before its
  window's steps even when its entry reaches the server after the window's first page (each window sends on its own,
  and the link's entry can wait behind an earlier request). A window that records is matched to the link that opened
  it by its URL, or, when it landed elsewhere (a redirect, a link that opens another URL), by its referrer, the page
  the link was on; a link with `rel="noreferrer"` whose window lands elsewhere is not matched, and its window is
  written as opened by its URL. A link's window whose first page with the recorder is not its first page (a PDF or another
  site that led to a Skyve page) is written with its page's `navigateEdit` or `navigateList` instead of a context push.
  Windows are told apart by an id in their sessionStorage; a window opened with `window.open` gets a copy of it, which
  is marked as a copy while the recorder's `window.open` runs.
- Back and Reload are read from the page that loads. Skyve's pages are never kept in the browser's back/forward cache
  (their responses are `Cache-Control: no-store`, and SmartClient has an unload listener), so a Back always loads the
  page; the recorder still handles a page shown from that cache (`pageshow` with `persisted`) as a page load.
- A filter on a date, time or number column is written as the column shows it (its converter's format). A value the
  user types that equals one the application showed but the user did not type is recorded as a capture.
- The kitchen sink has a tree and a calendar menu item (SmartClient only) and a map menu item: the SmartClient round
  trip navigates all three, and the PrimeFaces one the map.
- A PrimeFaces list grid shows its first page of rows, and a replay finds a row only among them: a recording that zooms
  into a row replays only while that row is on the first page.
- A recording starts in the Control Panel. If its first gesture is not a navigation, the script says so in a comment.
- Recorded values are user data. A recording against real data holds real names and values; edit them out before a
  script is committed.
- Replaying a recording finds rows by the text they showed when recorded, so it expects the same data. Records the
  recording created are created again with the same values: put `{run}` into values that must be unique.

### From a recording to a test

A recording replays as downloaded, but it is a first draft. To commit it as a
test:

1. Save it with the other scripts (see [Where scripts go](#where-scripts-go)).
2. Write its test with the base for its `uxui`: SmartClient for `desktop`,
   otherwise PrimeFaces with that device. Paste the header's `viewRoles(...)`
   line, and define the password with `variable("password", ...)`.
3. Put `{run}` into values that must be unique, so the test can run again.
4. Resolve each `unmapped` and `unconfirmed` comment: write the step by hand or
   remove it.
5. Add the checks that were not added with Check while recording.
6. Put each uploaded file under `sail/files/` on the test classpath.
7. Remove real data: names, addresses, amounts.

## Known Limitations

- **No tree or calendar in PF.** The PF UX/UIs have no tree or calendar page,
  so `navigateTree`, `navigateCalendar` and `navigateMenu` to such items fail
  in PF. SC only opens these views; there are no steps that act inside them.
- **Missing gestures.** PF has no gesture for `lookupDescriptionNew`,
  `lookupDescriptionEdit` or `dataGridEdit`. PF select steps work only on
  grids with a `selectedIdBinding`, so the same script can behave differently
  by renderer.
- **`checkMembership`** is rendered by neither PF nor SmartClient, so it
  cannot be tested.
- **No step for**: content signatures, downloads (content, reports, exports),
  opening a collapsed box, or waiting for a job. A report or PDF that opens in a
  new window can be followed with `navigateLink window="new"` and closed, but
  its content is not checked.
- **List grid tools in PF.** `listGridTool` fails in PF, whose list grids have
  no Refresh or Deselect tool. Use `<refresh/>` to reload the page. Only
  `refresh` and `clearSelection` exist; there is no step for export, chart,
  tags or snapshots.
  - The Monitoring Dashboard's Refresh buttons sit in boxes that start
    collapsed, so they are not tested.
  - Jobs are waited for through the completed jobs list, which is why the admin
    suites clear it.
- **Server configuration.** InviteUsers, CreateUsers, SendMail, JobSchedule
  editing and RunJobNow are not covered. They need mail or the job scheduler,
  which the test server runs without.
- **View roles.** Views are built as an every-role user, unless the IT
  registers the signed-in user's roles with `viewRoles`. A script that signs in
  as an unregistered, less privileged user must avoid views that differ for
  them. See [The test user](#the-test-user).
  - `viewRoles` takes module roles and customer roles, not groups. Groups are
    data, which the test JVM cannot read, so list the roles a user gets
    through their groups, or turn `sail.recording` on in the server, and the
    harness asks the server for them (see [The test user](#the-test-user)).
  - An app whose UX/UI selector routes by role can send the every-role user
    somewhere the script does not expect. Biz Hub Apps, for example, routes
    any user with a client role to its support centre UX/UI. Sign in as a
    user with the roles the workflow needs, and register them.
- **Links in rows.** `dataGridLink` clicks links in data grid rows only, not in
  list grid rows.
- **Unused attributes.** `dataEnter@navigateToWidget`,
  `navigateMap@refreshTimeInSeconds`, `navigateMap@showRefreshControls`,
  `navigateCalendar@startBinding` and `navigateCalendar@endBinding` are accepted
  and ignored.
- **Paged lists.** `rowContaining` and `testGridRows` see only the rows the
  browser has, which is the first page of a paged list. Use `listGridFilter`
  and `listGridSort` to bring the row onto it.
- **Restored Vue list grid selections.** A Vue list grid restores its
  selection without running its selected action, by design, so the PF
  executor clicks a row that already shows as selected twice (see
  `listGridSelect`).
- **Equals on a rounded value.** `equals` compares the stored value exactly. A
  value a cell shows rounded (`2.35` for a stored 2.345, `1,235` for 1,234.5) or
  as a magnitude (`Decimal2DollarsAndCentsAbsolute`) is not found by what the
  cell shows, in any renderer; filter by the stored value, or with
  `lessThan`/`greaterThan`.
- **Numbers and the browser's locale.** The Vue list grid formats numbers in the
  browser's locale; the server's converters and SmartClient write English
  numbers (`1,234.50`) whatever the JVM's or browser's locale. The suites run
  the browser in English.
- **Inline cells in PF** are addressed by `dataEnter`, `testValue` and the
  lookup steps; `testWidget` and `captureValue` keep the row but address the
  view's widget.
- **`notEquals` and rows with no value.** In SmartClient's advanced filter a
  `notEquals` clause also matched rows whose column has no value; the Vue list
  grid's did not. The converters scripts give every number column a value in
  every record, so their counts do not depend on it.
- **Numbers beyond a browser's precision.** The server sends numbers to
  SmartClient and the Vue list grid as JSON numbers, so a value beyond 15
  significant digits (a Decimal10 with a large whole part, a Decimal5 above
  10^10 with all its places) or a long beyond 2^53 (9,007,199,254,740,993 shows
  as ...992) is shown as the nearest double there, and a value typed or filtered
  in SmartClient is read as one. PrimeFaces forms and data tables, formatted on
  the server, are exact.
- **A number typed with more places than shown is stored as shown.** A number
  field reads back what it shows when the form is saved again (OK after Save),
  in both renderers: 2.345 typed into a two decimal places field is stored 2.35,
  and -0.001 is stored 0. A value the server sets keeps its places in
  SmartClient while the user does not edit it; PrimeFaces sends every field's
  shown text on every save. This is by design: what is stored is what the user
  saw.

## Runtime Bugs Found

The suites found Skyve runtime bugs that are outside SAIL's scope. All are now
fixed, or were found to be by design. Their reproductions, causes and fixes were
in `docs/sail-runtime-issues.md`, removed on 2026-11-16; see its git history.

## Other Executors

The `Executor` visitor (`org.skyve.metadata.sail.execution.Executor`) has
other implementations, which generate test code instead of driving a browser:

- skyve-ext `WebDriverExecutor` and `SeleneseExecutor`, with their skyve-web
  PrimeFaces subclasses `PrimeFacesInlineWebDriverExecutor` and
  `PrimeFacesSeleneseExecutor`. The Control Panel's SAIL actions use them.
  - They emit a "not supported by this generator" comment for `upload`,
    `testMessage`, `testGridRows`, `testWidget`, `navigateMenu`, `testMenu`,
    `switchMode`, `listGridFilter`, `listGridClearFilter`, `listGridSort`,
    `captureValue`, `captureCell`, `captureTitle`, `dataGridLink`,
    `dataGridMove`, `listMembershipMove`, `navigateBack`, `refresh`,
    `closeWindow` and `listGridTool`, and for `navigateLink window="new"`,
    whose new window the generated code does not follow. `navigateLink
    window="new"` and `closeWindow` still move the contexts as the
    interpreted executors do.
  - The PrimeFaces generators also emit a "not supported" comment, and no
    gesture, for a grid step or `lookupDescriptionPick` with `rowContaining`,
    for a combo or radio `dataEnter` with `match="code"` or `match="label"`,
    for `lookupDescriptionAutoComplete` with `pick`, and for `confirm="false"`.
    The context still follows.
  - They do not substitute `{run}` or `{var:name}`: the generated code has
    them as written.
  - With `confirm` absent or `true`, the generated code answers the action's
    confirmation when the action has confirmation text.
- The SAIL generator (`org.skyve.impl.generate.sail.Generator`) writes menu and
  view scripts. It adds `<switchMode/>` after the generated `<login/>` for
  desktop automations.

## Adding an Op-code

An op-code touches the language, every executor, the harness, the recorder,
the conformance suites and this document. Work in that order, and land each
layer with its tests. The window steps are a worked example: commits
`08afbed6e0` (language), `8782d4d550` (harness) and `68ac09e728` (recorder).

**1. Language (skyve-core).**

| File | Change |
|---|---|
| `org/skyve/metadata/sail/language/step/...` | The step class, in the package for its kind. Annotate it with `@XmlType` and `@XmlRootElement`, both with `namespace = XMLMetaData.SAIL_NAMESPACE`. It implements `Step`: `execute(Executor)` calls `executor.executeYourStep(this)`, and `getIdentifier` labels it in traces. A row step implements `GridRowStep`, and a capture `CaptureStep`. Its Javadoc is its specification, including how it fails. List it in the package's `package-info.java`. |
| `Procedure.java`, `Interaction.java` | `@XmlElementRef(type = YourStep.class)` in the step lists. Otherwise the step does not unmarshal. |
| `org/skyve/metadata/sail/execution/Executor.java` | `executeYourStep(YourStep)`. This breaks executors outside Skyve: add it to [Upgrading](#upgrading). |
| `org/skyve/impl/sail/execution/ContextualExecutor.java` | The context changes, and any rule that is the same for every renderer |
| `sail.xsd` | Regenerate with `XMLMetaData.main`, which writes the schemas from the JAXB classes. Copy it to `skyve-war/src/main/java/schemas/sail.xsd` and `skyve-core/src/test/resources/schemas/sail.xsd`; the two must be identical. |

Rules the XSD cannot express, such as "exactly one of `row` and
`rowContaining`", are checked when the step runs, and fail with a
`MetaDataException`. Test the step class, the XML round trip
(`SailLanguageXmlTest`) and any context change (`ContextualExecutorTest`).

**2. Generators (skyve-ext).** `WebDriverExecutor` and `SeleneseExecutor`
implement the method. Unless they generate code for it, they emit the "not
supported by this generator" comment and move the contexts; see
[Other Executors](#other-executors). Update `SailExecutorTest`.

**3. Harness (skyve-war tests).** `InterpretedWebDriverExecutor` does what is
the same for both renderers: `substituteTokens` on values, checking columns
against the query, and the strategy. It leaves the gesture to a
[renderer hook](#renderer-hooks) that throws "is not implemented yet" until a
renderer overrides it. The PF gesture goes in
`PrimeFacesInterpretedWebDriverExecutor` and `PrimeFacesSelenide`, and the SC
gesture in `SmartClientInterpretedWebDriverExecutor`, `SmartClientSelenide`
and, for a new locator kind, `smartclient-sail.js`. A renderer with no UI for
the step overrides the hook to fail with the reason. Gestures and waits follow
the [Synchronisation Rules](#synchronisation-rules). Unit-test the shared part
in `InterpretedWebDriverExecutorTest`.

**4. Conformance.** Add the step to both kitchen sinks, with a `kitchensink`
fixture if none fits, and add an inline failure test to each kitchen sink IT,
run under Verify, for each way the step should fail. Write each case as a
`@Test` with `assertAll`, not a `@TestFactory`: with reruns on, surefire
reports a failing dynamic test as a flake and the build passes. Run both
kitchen sinks headless and headed against a disposable server, and the admin
suites if the change could affect them.

**5. Recorder (skyve-web).** If users make the gesture, record it:

| File | Change |
|---|---|
| `src/js/sail/recorder-sc.js`, `recorder-pf.js` | Detect the gesture and send its entry. Shared parts go in `recorder-core.js`. |
| `org/skyve/impl/sail/recording/SailRecordingBuilder.java` | Turn the entry into the step |
| `SailRecordingChecks.java` | Check the step against the metadata on Stop |
| `PrimeFacesRecordingMap.java` | PF: name any new element in the page's map |
| The two recorder round-trip ITs and their scripts | Record the gesture, then replay the recording unedited |

Rebuild the bundles with `ant compress` in `skyve-web`, and commit
`recorder-sc-min.js`, `recorder-pf-min.js` and their source maps. Browsers
cache Skyve's scripts, so hard-reload when testing by hand.

**6. Documentation.** In this document:

- the op-code reference row, with its PF and SC support;
- the list of attributes where `{run}` is substituted, if the step takes text;
- the recorder's gesture table;
- [Known Limitations](#known-limitations);
- [Upgrading](#upgrading).

Report any runtime bug the suites find as a Skyve issue. Finally, run
`SailScriptsUnmarshalTest`.

## Upgrading

The SAIL language changes are additive for script authors. Code that
implements `Executor` directly is affected.

- **Breaking:** `Executor` has 20 new abstract methods: `executeUpload`,
  `executeTestMessage`, `executeTestGridRows`, `executeTestWidget`,
  `executeNavigateMenu`, `executeTestMenu`, `executeSwitchMode`,
  `executeListGridFilter`, `executeListGridClearFilter`, `executeListGridSort`,
  `executeCaptureValue`, `executeCaptureCell`, `executeCaptureTitle`,
  `executeDataGridLink`, `executeDataGridMove`, `executeListMembershipMove`,
  `executeNavigateBack`, `executeRefresh`, `executeCloseWindow` and
  `executeListGridTool`. An executor outside Skyve must implement them,
  even if only to fail or to emit a comment. Executors that extend skyve-ext
  `WebDriverExecutor` or `SeleneseExecutor` inherit comment-emitting
  implementations.
- `CaptureValue` implements the new `CaptureStep` (variable and `pattern`);
  `CaptureValue.VARIABLE_NAME_PATTERN` is now declared there and still reachable
  through `CaptureValue`.
- `ContextualExecutor` keeps `{run}` and the variables in a `RunState`.
  `shareRun(RunState)` makes an executor use another's run for every
  automation, which is how a workflow shares them; without it, each automation
  starts its own run, as before.
- `upload`'s new `testSuccess`, the capture steps' `pattern` and
  `navigateLink`'s `exact` and `window` are optional, so existing scripts are
  unchanged. `ContextualExecutor` implements `executeNavigateBack`,
  `executeRefresh` and `executeCloseWindow` for the contexts, and
  `executeNavigateLink` with `window="new"` sets the contexts aside instead of
  needing one (`openWindowContexts()`, `closeWindowContexts()`).
- The six row steps (`dataGridZoom`, `dataGridEdit`, `dataGridRemove`,
  `dataGridSelect`, `listGridZoom` and `listGridSelect`) implement
  `GridRowStep`. Their `row` is no longer required in the XSD, and `getRow()`
  returns `null` when the step uses `rowContaining`. Code that unboxes
  `getRow()` must handle that case. `lookupDescriptionPick` is now a
  `GridRowStep` too, with the same change.
- `confirm` is now also an attribute of `ok`, `save`, `delete`, `remove` and
  `zoomOut`. On `action`, an absent `confirm` now means "answer positively if
  asked". Before, nothing was answered.
- Executors that extend `ContextualExecutor` must pass value attributes
  through `substituteTokens(String)`, which replaces `{run}` and `{var:name}`;
  see [The run token and variables](#the-run-token-and-variables).
- `dataEnter` without `match` still takes an option's position, so existing
  scripts are unchanged.
- `<login/>` does not switch renderers. Scripts for SmartClient follow it with
  `<switchMode/>`.
- An app that runs the PrimeFaces SAIL harness needs Mojarra on its test
  classpath, as skyve-war has: add `org.glassfish:jakarta.faces` with
  `<scope>test</scope>`, at the version Skyve uses. Without it the harness
  fails with `ClassNotFoundException: com.sun.faces.util.Util`.
- `skyve:assemble` now also copies `src/test/resources/util/**`, which holds
  the SmartClient helper. It deletes an app's own files under that folder, as
  it already did under `src/test/java/util`.

## Troubleshooting

| Symptom | Cause and fix |
|---|---|
| `No SAIL base URL: set -Dsail.baseUrl=...` | Name the disposable server; there is no default |
| The test prints a lock file's name and waits | Another SAIL test is using the server ([One test per server](#one-test-per-server)) |
| `ClassNotFoundException: com.sun.faces.util.Util` | Add Mojarra ([Dependencies](#dependencies)) |
| `util/sail/smartclient-sail.js` is not on the test classpath | Assemble the app again with Skyve 10 ([The harness](#the-harness)) |
| Every sign in fails on a new server | The content add-in is missing from `<content directory>/addins/` |
| `admin`'s first sign in goes to the setup page | Set `environment.showSetup` to `false` |
| A PF step finds the wrong or no button | The views were built for another user, or the script's `uxui` is not what the server renders: [The test user](#the-test-user), [UX/UI and user agent](#uxui-and-user-agent) |
| `rowContaining` misses a row you can see | The row is past the first page, or the case differs: filter or sort first |
| A step fails only headed | The window lost the OS focus; the message says so |
| A failing test passes as a flake | Surefire reran it: `-Dsurefire.rerunFailingTestsCount=0` |
| A step "is not implemented yet" | The renderer has no gesture for it; see the op-code's PF and SC columns |

For any failure, `-Dsail.failureDir` saves a screenshot and the page source,
and `-Dsail.snapshotDir` and `-Dsail.videoDir` show the whole run.
