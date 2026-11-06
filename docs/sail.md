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

Without a registration, a script that signs in as a less privileged user must
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

### The run token and variables

`{run}` in a value is replaced with a token unique to the run. The token is the
current time in milliseconds in base 36: 8 or more lower-case letters and
digits. A new token is made for each automation, which means once per script
file. In a multi-file suite, a value created with `{run}` in one file cannot be
found with `{run}` in the next file.

Use `{run}` to make the records a script creates unique, so a script can run
again, or alongside another script, without clashing with leftovers.

`{var:name}` is replaced with the value of a variable. `<captureValue/>` sets
it from what a widget shows. Use it for values the application made, such as a
generated number, that a later step must find again:

```xml
<save />
<captureValue binding="number" variable="number" />
<ok />
<listGridFilter module="kitchensink" document="LookupDescription" column="number" value="{var:number}" operator="equals" />
<testGridRows module="kitchensink" document="LookupDescription" minRows="1" maxRows="1" />
```

- The value is the displayed text, trimmed, as `testValue` reads it.
- A widget that shows no value fails `captureValue`. A blank variable would
  match any row.
- A name is a letter, then letters, digits or underscores.
- Capturing the same variable again replaces its value.
- Variables last for one automation, which means one script file. A suite's
  next file starts with none.
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
- `login@user` and `login@password`, to sign in as a user created earlier in the
  same run;
- `testFailure@message` and `testMessage@contains`;
- every `rowContaining`, including `lookupDescriptionPick@rowContaining`;
- `navigateEdit@bizId`, `navigateLink@href` and `navigateLink@text`.

They are not substituted in attributes that name metadata, such as `module`,
`binding`, `column` or a menu item, nor in numbers, flags, file names,
`comment` text or `execute` scripts.

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
| List membership | The `\|`-separated descriptions of the items to select, for example `Admin\|Basic`. Every other item is deselected. | The members, `\|`-separated |

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
| `navigateLink` | `href` or `text` (exactly one) | With `href`, opens an absolute URL as is, or otherwise a URL relative to the base URL. With `text`, clicks the first displayed link (`<a>`), in page order, whose visible text contains the text (case-sensitive), such as a link in a blurb; a link that opens a new window fails the step, and no match fails it with the page's link texts. The top context, if any, loses its document and view type. Push a context afterwards if later steps need one. | yes | yes; with `text`, if the link leads to another page the step waits for it to load, then for SmartClient to settle |
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
| `dataEnter` | `binding`\*, `value`\*, `navigateToWidget`, `match` | Enters the value into the first displayed, enabled widget for the binding; see [Values](#values). Fails if no widget can take it. `navigateToWidget` is accepted and ignored: the executors always scroll the target into view. | yes | yes; after `dataGridNew`/`dataGridEdit` on an inline grid, bindings are tried as the grid's columns first |
| `dataEnter match` | `index` (default), `code`, `label` | How the value names a combo or radio option: its position, its code or its label. A code or label must name exactly one option. `code` and `label` fail on any other widget. Prefer `code`: labels change with the language, and positions change when options are added. | yes: the option's value or its label as the page shows them (a combo's empty first entry counts) | yes: the option's position is found from the item's options, then it is chosen as with an index |
| `testDataEnter` | `fixture` | Builds a bean with `DataBuilder` (the named fixture, or the `sail` fixture), then enters its values into every scalar widget of the view. Membership widgets are skipped. | yes | yes |
| `upload` | `binding` or `action` (exactly one), `file`\* | Uploads a file from the **test** classpath (for example `sail/files/sail-image.png` from `skyve-war/src/test/resources`). With `binding`, it uses the content widget's upload page, and the widget must hold content afterwards. With `action`, it presses the upload action's button, chooses the file and submits it. When the upload page opens the chosen file in its image editor, as it does for an image field, the step presses the editor's Upload button (`#crop`), which uploads it. Otherwise it presses the file list's Upload button. | yes; in the image editor, it waits for the cropper before pressing Upload | yes |
| `lookupDescriptionAutoComplete` | `binding`\*, `search`\*, `pick` | Types the search text into the lookup and picks the first suggestion. With `pick`, it picks the first suggestion, in the order shown, that contains the text (case-sensitive). Fails if nothing matches. | yes | yes; only the rows the drop-down has fetched are searched |
| `lookupDescriptionPick` | `binding`\*, `row` or `rowContaining` (exactly one) | Opens the lookup's drop-down and picks the zero-based row, or the first row, in the order shown, that contains the text (case-sensitive). | yes; `rowContaining` sees only the rows the drop-down shows | yes; `rowContaining` sees only the rows the drop-down has fetched |
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
| `action` | `name`\*, `confirm`, `testSuccess` | Presses the view's button for the action. | yes; an upload action only opens its upload page, which stays open: use `<upload action="…"/>` | yes; an implicit Upload action fails: use `<upload action="…"/>` |
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
| `dataGridNew` | `binding`\* | Adds a row. For a grid that is not inline, this opens the row's view and pushes its context. | yes; inline grids add in place | yes; inline grids start an inline edit |
| `dataGridZoom` | `binding`\*, `row` or `rowContaining` | Zooms into the row and pushes its context. | yes | yes |
| `dataGridEdit` | `binding`\*, `row` or `rowContaining` | Starts an inline edit of the row. | no: PF grids have no row edit gesture; inline cells are inputs | yes; the following `dataEnter`, `testValue` and `testWidget` steps address the row's columns, and any other step ends the edit |
| `dataGridRemove` | `binding`\*, `row` or `rowContaining` | Removes the row from the collection. The record itself is not deleted. | yes | yes |
| `dataGridSelect` | `binding`\*, `row` or `rowContaining` | Selects the row without zooming. | only on grids with a `selectedIdBinding`; on other grids a selection zooms | yes |
| `listGridNew` | `module`, `document`, `query`, `model`, `createView` | Presses New and pushes the new document's context. | yes; on a Vue list grid, its toolbar's New button, or the floating one a list view shows above its paginator | yes |
| `listGridZoom` | as `listGridNew` without `createView`, plus `row` or `rowContaining` | Opens the row and pushes its context. | yes; on a Vue list grid, the row's View Detail button, or a click on the row when the grid zooms on a row click | yes |
| `listGridSelect` | as `listGridZoom` | Selects the row without opening it. | only on grids with a `selectedIdBinding`. On a Vue list grid, it clicks the row. A row that already shows as selected is clicked twice, to unselect and select it: the grid restores its selection by design, without running the selected action (P7). The step fails if the row does not show as selected afterwards. | yes |
| `listGridFilter` | `module`, `document`, `query`, `model`; `column`\*, `value`, `operator` | Filters a column, as a user does in the grid's column filters. `column` is the column's binding. `operator` is `contains` (the default), `startsWith`, `equals`, `notEquals`, `lessThan` or `greaterThan`. Filters on different columns are ANDed, and a new filter on a column replaces its old one. Without `value`, the step clears that column's filter; `operator` then fails the step. | yes; see [Filtering lists](#filtering-lists) | yes; see [Filtering lists](#filtering-lists) |
| `listGridClearFilter` | `module`, `document`, `query`, `model` | Clears every column filter. | yes: Clear in each filtered column's menu (Vue); in a data table it deletes each typed filter's text, chooses each combo filter's empty option and unsets each check box | yes: the toolbar's Clear filter button, which clears the filter row and the advanced filter |
| `listGridSort` | `module`, `document`, `query`, `model`; `column`\*, `descending` | Sorts by the column alone, ascending unless `descending="true"`. Sort before a `rowContaining` when more than one row could match. | yes: clicks the column title until it alone shows the direction. PrimeFaces ignores a sort click on an empty data table, so that fails | yes: clicks the column header, once for ascending and again to reverse |

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
  `11:11`.

A step whose operator the column cannot take fails and names the operators it
can take. Filter, clear and sort steps fail if an error message shows
afterwards.

| Column type | PF Vue list grid | PF data table | SC list grid |
|---|---|---|---|
| Text | `contains`, `startsWith`, `equals`, `notEquals`; all ignore case | `contains` only, as the server decides (also for a variant domain's description and an association's business key) | `contains` in the filter row (ignores case); the others in the advanced filter (`contains` and `startsWith` ignore case) |
| Number | `equals`, `notEquals`, `lessThan`, `greaterThan` | `equals` only | `equals` in the filter row; the others in the advanced filter |
| Enum, constant domain | `equals`, `notEquals`, from the menu's drop-down | `equals`, from the filter's combo | `equals`, from the filter row's select |
| Boolean | `equals`, `notEquals`, with the three-state check box | `equals`, with the filter's check box | `equals`, with the filter row's three-state check box |
| Date, dateTime, timestamp | `equals`, `notEquals`, `lessThan` (Before), `greaterThan` (After) | `equals` only | `equals` in the filter row; `lessThan` and `greaterThan` in the advanced filter |
| Time | `equals` | `equals` only | `equals` in the filter row when the list shows the column; `equals`, `lessThan` and `greaterThan` in the advanced filter, for a shown or hidden column |
| Association | as text, on its business key | as text | `equals`, by the related record's description, from the filter row's select. Use a column through the association, such as `selection.description`, for `contains` |

**PF details.** The Vue list grid uses each column's filter menu. A new filter
on a column that already has one presses Clear first. The step fails on a
column the grid does not show (show it with the column chooser first) and on a
grid whose toolbar shows Match Any, because its filters are ORed. The Vue
grid's calendar takes `dd/mm/yyyy`, so the harness converts date values. A data
table filters each column one way, chosen by the server; the step opens the
filter row with Toggle filters when it is hidden.

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
| `testSuccess` | | Asserts that no error message, growl or field error is shown. Information messages do not count. Straight after a login, it asserts that the login succeeded. | yes | yes |
| `testFailure` | `message` | Asserts that an error is shown and, if `message` is given, that the error contains it. Only errors count: error messages and growls, field errors, and in SC the Problems dialog. Straight after a login, it asserts that the login was rejected. | yes | yes |
| `testMessage` | `contains`\*, `type` (`info`, `warning`, `error`) | Asserts that a growl or message containing the text, case-sensitively, is shown. With `type`, the message must have that severity. Unlike `testSuccess`/`testFailure`, it sees information and warning messages too. | yes: global messages, growls and field messages | yes: `isc.say` is info, `isc.warn` is warning, the Problems dialog and field errors are error, and growls go by their class |
| `testGridRows` | `binding`, or `module`/`document`/`query`/`model`; `rowContaining`, `minRows`, `maxRows`, `timeoutMillis` | Counts the rows, or only those containing the text, and asserts `minRows ≤ count ≤ maxRows`. An absent `minRows` means 0 and an absent `maxRows` means no maximum. With neither, the step asserts at least one row. `maxRows="0"` asserts absence. `binding` (a data grid) cannot be combined with the list attributes. With `timeoutMillis`, the grid is reloaded from the server and recounted every second until the bounds hold or time runs out. Use it to wait for a server job. A timed step still waits under None. | yes; a Vue list grid refetches its rows as it does after an ajax update, and the step waits for that fetch | yes; only list grids are reloaded |
| `testWidget` | exactly one of `binding`, `action`, `tab`; `visible`, `enabled` | `visible="true"` asserts the target is shown. `visible="false"` asserts it is hidden or not rendered. `enabled` asserts it is shown and is, or is not, enabled. With neither, the step asserts visible. `visible="false"` cannot be combined with `enabled`. `tab` is a tab path as for `tabSelect`. | yes | yes |

`testMenu` is listed under [Navigation](#navigation).

### Other steps

| Element | Content or attributes | Semantics | PF | SC |
|---|---|---|---|---|
| `comment` | text | Written to the trace. Does nothing else. | yes | yes |
| `execute` | JavaScript text | Runs the script in the browser page. Use it sparingly: it bypasses what SAIL is testing. | yes; then waits for any request or page load the script started | yes; checks for open dialogs first, then waits for SmartClient to settle |
| `pause` | `millis`\* | Sleeps. This is a last resort, for when no observable condition exists: see [Synchronisation Rules](#synchronisation-rules). The kitchen sinks pause once, for 100 ms, only to test the op-code. | yes | yes |
| `captureValue` | `binding`\*, `variable`\* | Keeps the value the widget shows in a variable for `{var:name}`; see [The run token and variables](#the-run-token-and-variables). It is not an assertion, so it runs under every strategy. | yes | yes |

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
| `PrimeFacesKitchenSinkSailIT` | `sail/kitchenSink/pf/pf-kitchensink-sail.xml`, plus a second test that switches to SmartClient with PrimeFaces' user menu and checks `<switchMode/>` back | PF, tablet |
| `PrimeFacesSailTestStrategyIT` | inline scripts, 3 tests | PF, tablet |
| `PrimeFacesAdminSuiteSailIT` | `sail/admin/pf/suite/*.xml` | PF, tablet |
| `PrimeFacesAdminSailIT` | `sail/admin/pf/pf-admin-sail.xml` (the older single-file admin script) | PF, tablet |
| `PrimeFacesAdminPhoneSailIT` | `sail/admin/pf/admin-phone-smoke.xml` | PF, phone |
| `SmartClientKitchenSinkSailIT` | `sail/kitchenSink/sc/sc-kitchensink-sail.xml` | SC |
| `SmartClientAdminSailIT` | `sail/admin/sc/sc-admin-sail.xml` | SC |
| `SmartClientAdminSuiteSailIT` | `sail/admin/sc/suite/*.xml` | SC |
| `SmartClientRecorderRoundTripSailIT` | records `sail/kitchenSink/sc/sc-recorder-round-trip.xml` and an admin contacts workflow, and replays the recordings; see [Recording](#recording). Needs `sail.recording` on. | SC |

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
- ActionsFixture: toolbar, form and tab buttons, change events and uploads;
- DialogsFixture: confirmations and server-side questions, answered yes and no;
- EscapingFixture: zoom in and out;
- contexts, links, a map, and `execute`;
- menus;
- switch mode;
- sign out.

`sc-kitchensink-sail.xml` covers the same ground for SmartClient, except
membership and ListGridFixture (`componentBuilderClass` only applies to
PrimeFaces). The only kitchen sink membership widget is on ConditionsFixture,
which never settles in SmartClient (runtime issue SC-4).

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
  pick`.

### Admin: application suite

The admin suites test the admin module as an application, through its menus,
as an administrator would use it. The PF suite is `sail/admin/pf/suite/`, run by
`PrimeFacesAdminSuiteSailIT`:

| File | Role | Covers |
|---|---|---|
| `00-precondition.xml` | set up | No backup and no "SAIL restore marker" data group exist |
| `00-backup.xml` | set up | Clears the completed jobs list. Backs up with no redaction, waits for the backup job, and leaves the restore marker data group. |
| `01-session.xml` | suite | User Dashboard; sign in with a bad password, an unknown user and the right one; sign out and in |
| `02-contacts-communications.xml` | suite | Contact create, find, change and delete, with an image upload and a declined delete; Communication Templates; Communications: every tab, attachments, and the send actions' refusals |
| `03-security-admin.xml` | suite | Data Groups; Groups (roles through list membership); the User wizard; the new user signs in, sees a role-filtered menu and changes their password; Sign In History; Security Log; Configuration tabs; Mail Log |
| `04-devops.xml` | suite | Control Panel (every tab, a query, cache statistics, generated SAIL); Data Maintenance (the backup list, import/export); Document Creator; Monitoring Dashboard |
| `05-reports-misc.xml` | suite | Reports, Audits, System Dashboard, Document Numbers, Jobs |
| `98-destructive.xml` | suite | Configuration saved with safe values (`mailBogusSend` on, so mail is only logged); a mail send that is refused; generate, rerun and delete tagged test data; delete completed jobs (declined, then confirmed) |
| `99-restore.xml` | restore | Restores the backup (declined, then confirmed) and waits for the restore job. Signs in again, checks that the marker has gone, and deletes the backup. |

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
   - PF: `00-precondition.xml`;
   - SC: `00-precondition.xml` and `00-backup.xml`.

   If a required file did not pass, the suite prints that the restore was held
   back. The decision depends on which files passed, not on message text.
4. The test then fails with every outcome: the first file that failed and, if
   it failed too, the restore.

`-Dsail.suite.only=03,04` runs only the suite files with those prefixes. The
set-up files and the restore always run, so the bracket stays intact. The
suites refuse `-Dsail.testStrategy`.

**The backup/restore bracket.** 00-backup backs up the data and 99-restore
restores it, so a suite leaves the server as it found it and can be run again.

**The no-stray-backup precondition.** The backups list shows the oldest backup
first (runtime issue P1), and 99-restore restores row 0. If another backup
existed, it would be restored instead of the suite's own. 00-precondition
refreshes the backup list and asserts
`<testGridRows module="admin" query="BackupsModel" maxRows="0"/>`. It also
asserts that no restore marker is left over. If it fails, delete the stray
backups and marker on the server by hand, check the data, and run again. The
suite asserts rather than deleting them, because a test should not decide to
delete someone's backup. For the same reason, the older `pf-admin-sail.xml`
makes no backup.

**Waiting for jobs.** Backup, restore and test data generation run as jobs.
The suites wait for each job with `testGridRows ... timeoutMillis` on its row
in the completed jobs list (`qCompletedJobs`). To make the rows of this run
distinguishable, 00-backup clears the completed jobs list first, so the
server's earlier job history is lost. No `<pause>` is used.

The admin suites must never lock `admin` out. They run no `testDataEnter` on
Configuration, change passwords only for the user they create, and back up with
data sensitivity "none". A redacted backup cannot be restored safely (runtime
issue P2).

With both suites following the precondition, and each deleting its own backup
in 99-restore, the PF and SC suites can run one after the other, in either
order, against the same server.

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
   - an upload finishing.

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

## Recording

The SAIL recorder writes a script of what a user does in SmartClient. The script is a first draft: an editor (a person
or an AI) adds assertions, puts `{run}` into values that must be unique, and removes steps that do not matter. The plan,
with the phases still to come (context comments, assertions while recording, PrimeFaces, and the remaining gestures), is
[plan_sail_recorder.md](ai/plan_sail_recorder.md).

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

1. Open **DevOps / Control Panel**, select the **SAIL** tab, and press **Start** in the Recording box. The recorder loads
   into the page at once, without a reload.
2. Use the application. Every page of the session loads the recorder until recording stops.
3. Come back to the Control Panel and press **Stop**. The recorder sends its last steps first, then the server builds the
   script. Going back to the Control Panel to stop is left out of the script.
4. Press **Download** for the script, `sail-recording-<yyyyMMdd-HHmm>.xml`. It can be downloaded again until the next
   recording starts or the session ends.

The recording lives only in the server session. It is never written to the database, and signing out or the session
expiring discards it, so download it first.

### What is recorded

The script has `uxui="desktop"` and `userAgentType="desktop"`, one step per line, and each element's attributes in a fixed
order. Its `<before>` signs in as the user who recorded, then switches to SmartClient:

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
| A page loaded with `a=e` or `a=l` in its URL (a link, a typed URL, a reload) | `navigateEdit` (with `bizId` from `i`) or `navigateList` |
| Typing into or changing a field | `dataEnter` with the final value, written before the next step or server response. Combos and radios record the option's code with `match="code"`; check boxes `true` or `false`; dates and times the text as shown; rich text its plain text. New lines are written as `&#10;`. |
| Selecting a tab | `tabSelect`, with `::` for nested tabs. Selecting the tab already shown records nothing. |
| A view's buttons | `ok`, `save`, `cancel`, `delete`, `zoomOut`, `remove`, or `action name="..."` for the view's own actions. A declined confirmation adds `confirm="false"`. |
| A list grid's New, or zooming into a row | `listGridNew`, `listGridZoom`. A row is named with `rowContaining`: its bizKey when a cell shows it, otherwise the cell the fewest loaded rows show, the leftmost on a tie; `row` only when no text finds the row first. The list view's own grid is named by the context; a grid in a view by `module` and `query` (or `document` and `model`). |

Anything else is written as an `<!-- unmapped: ... -->` comment where it happened, so the editor sees the gap: a click on
a control that records no step (a grid tool, a data grid row, a link, a header menu item), clearing a field (SAIL cannot
enter an empty value), and typing into a password field (passwords are never recorded). A gesture is never dropped
silently.

### How it works

- `desktop/home.xhtml` adds `skyve/sail/recorder-sc-min.js` after `skyve-min.js` only through
  `#{desktop.sailRecording}`, which checks the gate. The Control Panel's SAIL tab loads the same bundle when Start's
  response says the session records. The bundle is built by `skyve-web/build.xml` (`compress`) from
  `skyve-web/src/js/sail/recorder-sc.js`, and is never merged into `skyve-min.js`.
- Skyve's client code has no recorder checks. The recorder wraps a few of Skyve's SmartClient methods when it loads
  (each is marked in `skyve-web/src/js/desktop` with a comment) and listens to SmartClient's `ValuesManager.itemChanged()`
  and to the DOM in the capture phase. It detaches when recording stops.
- It sends its steps as JSON batches, one request at a time through SmartClient's RPC manager, to the `sailrecording`
  endpoint (`org.skyve.impl.sail.recording.SailRecordingServlet`). Each page gets a CSRF token from the endpoint, and the
  endpoint checks the gate on every request. Steps the page has not seen acknowledged are sent again from a beacon as it
  unloads, and the server adds each step once. `GET sailrecording?from=i&count=n` returns a range of the steps.
- On Stop, `org.skyve.impl.sail.recording.SailRecordingBuilder` builds the `Automation` and writes its XML.
- `SmartClientRecorderRoundTripSailIT` records two workflows (kitchen sink widgets, actions, tabs and list grids; admin
  contacts) and replays each recording unedited. It also checks that the home page is unchanged unless the session
  records.

### Limits

- SmartClient only. PrimeFaces recording, context comments and server-side checks, assertions added while recording,
  and lookups, data grids, uploads, server-side questions, list filters and sorting, and tree, map and calendar
  navigation come in later phases; until then they are unmapped comments (or, for a field such as a lookup, an unmapped
  comment for its change).
- A recording starts in the Control Panel. If its first gesture is not a navigation, the script says so in a comment.
- Recorded values are user data. A recording against real data holds real names and values; edit them out before a
  script is committed.
- Replaying a recording finds rows by the text they showed when recorded, so it expects the same data. Records the
  recording created are created again with the same values: put `{run}` into values that must be unique.

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
  opening a collapsed box, or waiting for a job.
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
    through their groups.
  - An app whose UX/UI selector routes by role can send the every-role user
    somewhere the script does not expect. Biz Hub Apps, for example, routes
    any user with a client role to its support centre UX/UI. Sign in as a
    user with the roles the workflow needs, and register them.
- **Rejected uploads.** `upload` expects the upload to succeed. A script
  cannot expect an upload to be refused and check its message (such as "file
  already exists").
- **What `captureValue` reads.** It reads a widget in the view by binding. It
  cannot read a list cell or the view's title, so a number that is only shown
  in a list or a title cannot be captured.
- **Links in grid rows.** No step clicks a link in a data grid or list grid
  row (for example a "Revoke Access" link in each row). `navigateLink text`
  clicks the first matching link on the page, which is the wrong one when
  every row has the same link.
- **One automation per renderer.** `{run}` and `{var:name}` belong to one
  automation. A workflow that moves between renderers (a client in the
  support centre, then staff in SmartClient) is several scripts, and the IT
  has to pass the run token and any captured values between them itself.
- **Unused attributes.** `dataEnter@navigateToWidget`,
  `navigateMap@refreshTimeInSeconds`, `navigateMap@showRefreshControls`,
  `navigateCalendar@startBinding` and `navigateCalendar@endBinding` are accepted
  and ignored.
- **Paged lists.** `rowContaining` and `testGridRows` see only the rows the
  browser has, which is the first page of a paged list. Use `listGridFilter`
  and `listGridSort` to bring the row onto it.
- **Runtime bugs.** Update My Details is commented out of both admin scripts
  (runtime issue P4). Other runtime bugs have workarounds in the scripts or the
  executors, such as the second click on a Vue list grid row (P7). SC's missing
  Backup and Restore "started" growls are left out of its green path. See the
  issues list.

## Runtime Bugs Found

The suites found Skyve runtime bugs that are outside SAIL's scope. They are
reported, not fixed, and the scripts avoid or work around them. Each one, with
its reproduction and suspected cause, is in
[SAIL runtime issues](sail-runtime-issues.md).

## Other Executors

The `Executor` visitor (`org.skyve.metadata.sail.execution.Executor`) has
other implementations, which generate test code instead of driving a browser:

- skyve-ext `WebDriverExecutor` and `SeleneseExecutor`, with their skyve-web
  PrimeFaces subclasses `PrimeFacesInlineWebDriverExecutor` and
  `PrimeFacesSeleneseExecutor`. The Control Panel's SAIL actions use them.
  - They emit a "not supported by this generator" comment for `upload`,
    `testMessage`, `testGridRows`, `testWidget`, `navigateMenu`, `testMenu`,
    `switchMode`, `listGridFilter`, `listGridClearFilter`, `listGridSort` and
    `captureValue`.
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

## Upgrading

The SAIL language changes are additive for script authors. Code that
implements `Executor` directly is affected.

- **Breaking:** `Executor` has 11 new abstract methods: `executeUpload`,
  `executeTestMessage`, `executeTestGridRows`, `executeTestWidget`,
  `executeNavigateMenu`, `executeTestMenu`, `executeSwitchMode`,
  `executeListGridFilter`, `executeListGridClearFilter`, `executeListGridSort`
  and `executeCaptureValue`. An executor outside Skyve must implement them,
  even if only to fail or to emit a comment. Executors that extend skyve-ext
  `WebDriverExecutor` or `SeleneseExecutor` inherit comment-emitting
  implementations.
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
