# Server-side Action Availability

From Skyve 10, the server refuses a request that names a view action the view, as last
rendered for the bean, offers the user no way to trigger. A trigger is a control that
leads to the action: a toolbar entry, a button, a link or a `<server>` event handler.
Before Skyve 10, any action the user had the privilege for ran, whether or not the view
offered it. Privileges are still checked as before, and there is no setting to turn the
new check off.

A refused request changes nothing: it is rolled back, and its message is "This action is no
longer available." (the new i18n key `exception.actionUnavailable`, translated in the
bundled languages; [Custom clients](#custom-clients) says how each entry point shows it).
The refusal is the new `org.skyve.domain.messages.ActionUnavailableException`, logged as a
warning.

## What is checked

- SmartClient requests to `/smartedit`: custom actions, the request that prepares a
  Download, OK, Save, Delete, and actions of links in data grid or repeater rows. Flutter
  posts to the same endpoint and gets the same check.
- Preparing a download (the `/smartedit` request, or `ExecuteDownloadAction` in PrimeFaces)
  records a grant for it on the conversation: its action, binding and, for a grid row,
  bizId. `DownloadServlet` serves only the download that matches the grant. It does not
  check the view again, because `prepare()` may disable the action that requested it. HEAD,
  range and resumed requests are served until another download is prepared. A download URL
  opened without being prepared is refused, in both renderers.
- `BizportExportServlet`, the upload action page (`upload.xhtml?_u=action`) and the BizPort
  import page (`bizImport.xhtml`) check their BizExport, Upload and BizImport action.

Each check uses the view the user was shown: the view of the request's UX/UI for the bean's
document, the edit view for a saved bean and the create view for a new one. A row link is
looked for in the form's view, and its action in the row document's edit view.

Not checked: ZoomOut, Cancel, Remove in a zoomed view, Add, New, Edit, Navigate, Report and
Print; requests that name no action, such as rerenders; and PrimeFaces buttons and links,
as JSF already ignores a click on a disabled or unrendered component, silently as before.

## What counts as a trigger

Conditions, named or inline, are evaluated against the bean. An action is available when
at least one of its triggers passes these rules:

- a toolbar entry: `inActionPanel` is not `false`, and the action is visible and enabled;
- a `<button>`: its containers are visible and enabled, and its action is visible and
  enabled (a button has no conditions of its own);
- a `<link>` to an action outside data grids and repeaters: the link and its containers are
  visible, and its action is visible. Whether the action is enabled is ignored, because
  SmartClient does not disable such a link;
- a `<server action="...">` event handler: the widget and its containers are visible.
  Whether they are enabled, and the action's own conditions, are ignored, because the client
  can enable a widget and the event fires regardless. A `disabled` condition therefore does
  not protect an action fired by an event. A data grid or repeater column's input widget
  takes the grid's visibility;
- a link in a data grid or repeater row: the grid with the request's grid binding and its
  containers are visible, the link is visible for that row (its condition is evaluated
  against the row), and its action is declared in the row document's edit view. The row
  action's own conditions are not applied.

A request names a custom or class-based action by its class name, never by its name, so a
request for a class that two actions share passes when either action is available.
Built-in OK, Save and Delete are available only when the view declares them, singly or
through `<defaults/>`; a view with no `<actions>` offers none. Triggers in a `<component>`
count as the containing view's. A view's `refreshAction` is not a trigger.

## Upgrading

Run the report described in [Finding views at risk](#finding-views-at-risk) first. Its
event-only and no-trigger lists are the actions the first item below affects.

- Custom JavaScript, or another client, calling an action that is not available is
  refused: typically `doAction('X')` for an `inActionPanel="false"` action with no button or
  event handler, or for an invisible action. Give the action a real trigger, such as a
  `<server>` event handler or a `<button>`, or move the logic out of the view action into a
  REST endpoint or servlet the application owns. There is no metadata opt-out.
- A client calling a view's `refreshAction`, which `MetaDataServlet` publishes as
  `refreshActionName`, is refused unless the action also has a trigger. No Skyve client
  does this.
- A `<server>` handler on a widget that `<toggleVisibility>` or `<setInvisible>` shows in the
  browser while its `invisible` condition is true on the server is refused. Drive the
  widget's visibility from a server-side condition with `<rerender>`, or put the handler on a
  widget that is visible on the server.
- An action can be available in one UX/UI's view and not another's. For example, admin
  DataMaintenance `CheckContent` has a button in the desktop view and no trigger in the
  default view.
- On a stale screen, where a condition that reads the database, the clock or other users'
  changes has flipped since the view was rendered, the action used to run. Now the user gets
  the message.
- SmartClient requests the built-in OK and Save as `_a=OK` and `_a=Save`, and a custom
  action by its class name, so a custom action classed `OK` or `Save` (as
  `admin.MonitoringDashboard`'s Save is) collides with them: its triggers decide such a
  request, and the built-in action then runs. If the view also has the built-in action, an
  unavailable custom action blocks an enabled built-in Save or OK. A custom action only
  named `OK` or `Save` does not collide. Rename the class, together with its role action
  privileges, which are keyed by class name, and the `<server action>` and `<button action>`
  references to it. SmartClient then runs the class's `execute()` instead of the built-in
  Save or OK, as PrimeFaces already does.
- Flutter evaluates no `disabled` or `invisible` conditions and always uses the edit view, so
  it offers actions that the server refuses, including actions on a new bean whose document
  has a different create view, and it shows no message for the refusal. For each action a
  Flutter app uses, avoid `disabled` or `invisible` conditions, on the action and its
  containers, that Flutter users can meet. When the document has a create view, declare the
  action there the same way too.
- A conversation holds only the grant of the last prepared download, and any other
  download is refused, including a range or resumed request for an earlier one. A download
  still streaming when the next one is prepared restores its own grant when it finishes, so
  a later request for the newer download is refused. A custom client should request each
  download straight after preparing it, and not prepare the next until the previous
  download's response has completed.
- Code that builds a download URL itself, by calling `WebUtil.getDownloadActionUrl(...)` or
  writing `/download?_n=...`, is refused unless it records the grant. After `prepare()`,
  call `recordPreparedDownload(actionName, formBinding, gridBinding, bizId)` on the
  conversation's `org.skyve.impl.web.AbstractWebContext`, with the values passed to
  `getDownloadActionUrl` as `downloadActionName`, `viewBinding`, `dataWidgetBinding` and
  `elementBizId`. The grant is kept with the conversation, so the conversation must be
  cached afterwards, as Skyve's own requests do.
- In PrimeFaces, an error message queued during `prepare()` or `postRender()` stops the
  conversation being cached, so the grant is lost and the download is refused; this
  download was served before Skyve 10. Queue such a message at a lower severity, or throw a
  `ValidationException` (or another `MessageException`) from `prepare()` to stop the
  download with your message; any other exception shows a generic error.

## Known limitations

- SmartClient shows the message only. Its buttons are not refreshed until the next fetch or
  rerender.
- PrimeFaces ignores a disabled `<tabPane>`, so buttons inside it can be clicked and they run,
  as PrimeFaces button clicks are not checked on the server. An Upload button in a disabled
  `<tab>` or `<tabPane>` still opens the upload page, and the upload is then refused. Put
  the `disabled` condition on the actions, or on each `<tab>` (except for Upload), as well
  as on the `<tabPane>`.
- In SmartClient, which saves the form first, BizPort export, BizPort import and upload
  actions are checked against the saved bean. An export is checked when its request arrives;
  an import or upload when the file is submitted, so the refusal shows on the upload page.
- The upload and BizPort import pages take their UX/UI from the UX/UI selector, which can
  differ from the UX/UI of a PrimeFaces page reached through a `<direct>` route, or after the
  UX/UI preference changes in another tab.
- In SmartClient, when an export or download request is refused the browser leaves the app
  for the refusal page and unsaved edits are lost, as with other export and download errors.

## Custom clients

A refused `/smartedit` request gets this response:

```json
{"response":{"status":-51,"data":"This action is no longer available."}}
```

`data` is the user's localised message, HTML-escaped and then JSON-escaped. The transaction
is rolled back, the conversation is not updated and the CSRF token is not replaced: ignore
the response's `X-CSRF-TOKEN` header and send the next request with the token you already
hold.

BizPort export, BizPort import and upload requests are checked against the bean that their
`_b` binding zooms in on, or the conversation bean when there is none. The other entry
points show a refusal as follows:

- `DownloadServlet` and `BizportExportServlet`: an HTML page holding the message, sent with
  HTTP status 200 and content type `text/html` (headers only for a HEAD download request),
  so check the content type rather than the status;
- the upload and BizPort import pages: the message as a global `FacesMessage`.

## Finding views at risk

`ActionAvailabilityAllViewsH2Test` runs the check over every edit and create view of every
customer, document and UX/UI, with all conditions false and then all true, and writes
`target/action-availability-report.txt`. Run it after bumping the Skyve dependency and
before deploying. The report lists actions reachable only through a `<server>` handler,
classes shared by several actions in one view, custom and class-based actions whose class
name is `OK`, `Save`, `ZoomOut` or `Print` (which `/smartedit` takes as the built-in), actions and row links with no trigger for any bean state, views with a
`refreshAction`, `<server>` handlers on widgets revealed by `<toggleVisibility>` or
`<setInvisible>`, and other `<toggleVisibility>` and `<setInvisible>` uses. The report never
fails the build.

`skyve:assemble` does not copy the test, so take it from the Skyve repository at the 10.0
release: `skyve-war/src/test/java/org/skyve/impl/metadata/view/ActionAvailabilityAllViewsH2Test.java`.
Put it in your project's `src/test/java/org/skyve/impl/metadata/view`, replace both the
import and the superclass `modules.test.AbstractSkyveTestDispose` with
`util.AbstractH2TestDispose`, and replace `u.getName()` with `USER`. Then run:

```
mvn test -Dtest=ActionAvailabilityAllViewsH2Test
```

The test asserts that admin DataMaintenance `CheckContent` has no trigger in exactly the
UX/UIs other than `desktop`; remove that assertion if your project overrides the
DataMaintenance views. It also fails when no data grid row link was checked; the standard
admin User create view supplies one.

Try **Kitchen Sink → Actions Fixture**.
