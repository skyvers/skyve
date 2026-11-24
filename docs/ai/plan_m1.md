# Plan: M1 - Refuse actions whose control is unavailable in the view

## Summary

The server will refuse a view action when the user could not have triggered it from the view as it was last rendered. "Could not have triggered" means every control that leads to the action is disabled or invisible, or there is no such control at all.

The check is a new visitor, `ActionAvailability` in `skyve-core`. It walks the view against the bean and decides whether the action has at least one reachable trigger. A trigger is any of:

- a toolbar action;
- a form `<button>`;
- a `<link>` to an action, including one in a data grid row;
- a `<server action="X"/>` event handler.

The check is called from every server entry point where the request names the action: the SmartClient edit servlet, `DownloadServlet`, `BizportExportServlet`, `ContentUploadView` and `BizportImportView`. A refusal throws a new `ActionUnavailableException`, which logs a warning. SmartClient shows the message; refreshing the buttons was dropped in W5 (Mike, 2026-10-21; see Known limitations).

The PrimeFaces button path needs no change. JSF already ignores clicks on disabled or unrendered buttons, and it decides using the right bean state (see [Current state observed](#current-state-observed)).

There is no configuration flag. Enforcement is on from the first release (decision 5 below).

Modules: `skyve-core` (visitor, exception), `skyve-ext` (none expected), `skyve-web` (call sites, SmartClient client), `skyve-war` (i18n, kitchen-sink fixture, H2 tests).

Validation:

- exhaustive tests of the decision for every combination of condition values;
- a test that runs the visitor over every view in the repo and reports risk cases;
- SmartClient H2 tests through the real servlet;
- manual checks in a browser, in both renderers.

## Scope

### Locked decisions (Mike, 2026-10-20)

1. **Full scope.** Custom actions, the class-based built-in actions (BizExport, BizImport, Download, Upload), and the built-in OK, Save and Delete.
2. **Invisible counts as unavailable**, the same as disabled.
3. **Event triggers are lenient.** A `<server>` event handler counts as a trigger when its widget is visible. Whether the widget is enabled is ignored, because `setDisabled` and `toggleDisabled` can change that in the browser.
4. **Refusal UX.** A readable "no longer available" message, a refresh of the view's condition values, and a warning in the log. *As built:* the refresh was dropped in favour of the plan's warning-only fallback (Mike, 2026-10-21).
5. **No flag.** Enforced from the first release, with no log-only phase.

### Decisions made in this plan (confirmed by Mike, 2026-10-20)

6. **No new PrimeFaces check on the button path.**
   - JSF evaluates `disabled` and `rendered` during Apply Request Values, before the posted values reach the bean. That matches what the button showed.
   - A check in `FacesView.action()` would run during Invoke Application, after the values have been applied. It would refuse buttons that looked enabled when an edit changed a condition without a rerender.
7. **ZoomOut, Cancel, Remove (in a zoomed child), Add, New, Edit, Navigate, Report and Print are out of scope.**
   - Cancel and zoomed Remove never reach the server in SmartClient.
   - ZoomOut is the only way out of a zoomed view (comment in `SmartClientEditServlet`), and SmartClient sends it only when the view has changes.
   - Add, New, Edit and Navigate are grid or list operations governed by grid conditions, not view actions.
   - Report and Print open a dialog and never run an action class.
8. **Downloads use a recorded grant, not a re-check.**
   - A download is always two requests. The first, checked, request calls `prepare()`; the second hits `DownloadServlet`.
   - `prepare()` may change the bean, for example marking it exported and so disabling its own button. Re-checking in `DownloadServlet` would then refuse a download the user has just started.
   - Instead, the prepare step records which download it prepared on the conversation's web context, and `DownloadServlet` serves only that download.
   - The grant is not consumed, so range requests and resumed downloads still work.
9. **Permission checks stay where they are.** `ActionAvailability` answers only whether the view offers a reachable trigger. The existing role checks (`canExecuteAction`, create, update, delete) are unchanged.

### Work items

- **W1** `ActionUnavailableException` and its i18n key.
- **W2** The `ActionAvailability` visitor and its unit tests.
- **W3** An H2 test that runs the visitor over every view in the repo and writes a report.
- **W4** A kitchen-sink fixture document, `ActionsFixture`, covering every trigger type.
- **W5** SmartClient enforcement for custom actions, Download, OK, Save, Delete and grid-row link actions, plus the client refresh.
- **W6** The download grant and the `DownloadServlet` check.
- **W7** Checks in `BizportExportServlet`, `ContentUploadView` and `BizportImportView`.
- **W8** Checking that each renderer offers exactly what the visitor allows, plus manual browser checks.
- **W9** Documentation: `docs/skyve-10-changes.md`, and a note in `docs/inline-conditions.md`.

## Affected modules

| Module | Why |
|---|---|
| `skyve-core` | `ActionAvailability` (next to `ViewVisitor`, `org.skyve.impl.metadata.view`) and `ActionUnavailableException` (`org.skyve.domain.messages`). Interpreting view metadata against a bean belongs in core, as `ViewRenderer` already does. |
| `skyve-web` | Call sites in `SmartClientEditServlet`, `DownloadServlet`, `BizportExportServlet`, `ContentUploadView`, `BizportImportView`, `ExecuteDownloadAction`. The grant field on `AbstractWebContext` is in `skyve-core` (`org.skyve.impl.web`). The SmartClient client is `skyve-web/src/js/desktop/editView.js`. |
| `skyve-war` | The i18n key in `resources/i18n*.properties`, the kitchen-sink `ActionsFixture` document and views, generated domain for the fixture, and the H2 tests. |
| `skyve-ext` | No change expected. |

Dependencies point the right way: core defines the check, web calls it, war assembles and tests.

## Dependencies

- No milestone blocks this one.
- The server-side confirmation feature (`d0447f41f7`) is already on `master`. W5 follows its pattern: an `ExceptionUtils.throwableOfType` catch in `SmartClientEditServlet.processRequest`, and a status constant in `editView.js`.
- A refused request is rolled back like any other failure. A confirmation rerun (`_q`) re-evaluates the same conversation state, so the check gives the same answer.
- Work locally in the shared checkout (`/Users/mike/_/skyve`, on `master`), with no branch, no worktree and no commits (Mike, 2026-10-20). Other sessions edit the same checkout, so keep a manifest of the files this milestone touches and never revert changes outside it.

## Backward compatibility

This is a **runtime behaviour change** for downstream apps. Nothing in this repo fails to compile because of it. With no flag (decision 5), apps feel it on upgrade.

**What starts being refused:**

1. **Custom JavaScript, or any other client, calling an action that has no reachable trigger in the view.** Typically a `doAction('X')` for an action declared `inActionPanel="false"` with no button and no event handler, or one declared `invisible`.
   - Migration: give the action a real trigger, such as a `<server>` event handler or a `<button>`. Otherwise move the logic out of the view action, into a REST endpoint or a servlet the app owns.
   - There is no metadata opt-out. If it proves common, add one in a follow-up (see [Better engineering notes](#better-engineering-notes)).
2. **Stale screens.** A condition that reads the database, the clock or other users can change between render and click. Today the action runs; afterwards the user gets the message (SmartClient's buttons refresh on the next fetch or rerender).
3. **Flutter.** `skyve_flutter/lib/util/skyve_rest_client.dart` posts `_a` to `/smartedit`, so Flutter gets the same check. W8 found that Flutter evaluates no `disabled` or `invisible` conditions and always shows the edit view. Its clicks on actions that are unavailable in the view are therefore refused, including actions on a new bean of a document whose create view differs. Flutter shows no message for the refusal.
4. **Download URLs built by app code.** `DownloadServlet` now serves only the download the conversation last prepared. Code that builds a download URL itself, by calling `WebUtil.getDownloadActionUrl(...)` or writing `/download?_n=...`, must call `AbstractWebContext.recordPreparedDownload(...)` after `prepare()`, with the same values it passes to the URL builder. Otherwise the download is refused.
5. **`refreshAction` called by an external client.** `MetaDataServlet` publishes a view's `refreshActionName`, but the refresh action is not a trigger. No client in this repo sends it; a downstream client that polls it would be refused. The W3 report lists views with a `refreshAction`.
6. **Widgets revealed in the browser.** `<toggleVisibility>` and `<setInvisible>` can show a widget whose `invisible` condition is true on the server. A `<server>` handler on that widget is then refused, because decision 3's leniency covers enabled-ness only. The W3 report lists views that use these event actions.
7. **Views that differ by UI type.** The check uses the view for the request's UI type, so it follows what that UI renders. For example, DataMaintenance `CheckContent` is reachable in the desktop view through a `<button>`, and unreachable in the default view, where it is `visible="false"` with no trigger.

**Public API:**

- One new public class, `org.skyve.domain.messages.ActionUnavailableException`.
- `ActionAvailability` lives in an `impl` package.

**Unchanged:** metadata format, generated code and the role privilege model.

## Current state observed

Read on `master` at `ace24df646` on 2026-10-20. Line numbers are approximate.

### SmartClient (and Flutter): no check

- The client sends the action's class name in `_a`.
  - `SmartClientViewRenderer` emits `action.getResourceName()` for custom actions (about line 3592).
  - `ViewRenderer.preProcessAction` (948-1007) emits the built-in name, except BizExport, BizImport, Download and Upload, which use the class name.
- `SmartClientEditServlet.processRequest` (337-367) treats any `_a` other than OK, Save, ZoomOut or Print as a custom action. `apply()` (1097-1130) checks only `user.canExecuteAction`, then finds the class on the request's document with `getServerSideAction` or `getDownloadAction`. The action does not have to be declared in any view.
- `apply()` builds a `ViewJSONManipulator` on the form document's edit or create view (`formBean.isCreated()`, line 1075), visits it, then applies the posted JSON (1086).
  - The manipulator already refuses field values for disabled or invisible widgets (`forApply && parentEnabled && enabled(...)`, for example lines 2016-2018). This milestone is the same rule applied to actions.
  - Checking after `manipulator.visit()` and before `applyJSON` sees the state the client rendered.
- Built-in actions:
  - **OK and Save** arrive as add or update with `_a=OK` or `_a=Save` and go to the implicit branch of `apply()` (1162-1206). They persist only at top level (1193-1205).
  - **Delete** arrives as a `remove` operation with no `_a` (handled at about line 423, `remove()` at about 1352). It uses the conversation bean; no JSON is applied.
  - **ZoomOut as a fetch** is also sent by `rerender()` (editView.js about line 229). It must not be checked.
- **Grid-row link actions:**
  - SmartClient emits `_view.doAction('X',false,prefix,'{bizModule}','{bizDocument}','{bizId}')` (`ViewJSONManipulator` `HrefProcessor.processActionReference`, 164-169).
  - The servlet resolves the row bean through `_g` (266, 320-321).
  - Link visibility is evaluated against the row (`row.evaluateCondition`, about line 1524).
- **Error responses:**
  - The catch at about lines 449-462 sends `QuestionException` to `produceQuestionResponse` (status -50) and everything else to `produceErrorResponse` (520-549).
  - editView.js handles -1 with `isc.warn` only and refreshes nothing (doAction 768-773, saveData 637-638).

### PrimeFaces: already enforced for buttons

- Buttons are components with `disabled` and `rendered` value expressions (`TabularComponentBuilder.actionButton` 4538-4660, `setDisabled` and `setInvisible` in `AbstractFacesBuilder` 158-260).
- A form `<button>` also gets the form's disabled condition (`FacesViewRenderer.renderFormButton`, ORed with the action's in `TabularComponentBuilder.actionButton`).
- Every action runs through a method expression set on the server (`ComponentBuilder.methodExpressionForAction`, 1416-1480). The request carries only the clicked component's client id, never an action name.
- PrimeFaces 13.0.10's `CommandButtonRenderer` and `CommandLinkRenderer` return early from decode when the component is disabled, and JSF never decodes unrendered components. The click is dropped silently.
- `ExecuteActionAction` and `ExecuteDownloadAction` look the action up on the **target (row) document's** view (83-100). Grid-row actions therefore belong to the row document's view, with conditions evaluated against the row bean.

### Entry points that take the action name in both renderers

All four check only `canExecuteAction`:

- `DownloadServlet` (about line 112), which reads the conversation bean;
- `BizportExportServlet` (86);
- `ContentUploadView` (623, `upload.xhtml?_a=`);
- `BizportImportView` (172).

`WebUtil.getDownloadActionUrl` is called only from `SmartClientEditServlet.apply()` (1123) and `ExecuteDownloadAction` (90), both after `prepare()`.

### Visitor facts used by the design

- `visit()` calls `visitContainer(view, true, true)`. For the root view the order is:
  1. `visitView()`;
  2. the widgets;
  3. the sidebar;
  4. `ActionVisitor.visitActions(view)`;
  5. `visitedView()`.
- Toolbar actions are visited without parent visibility flags.
- `<defaults/>` is stored as one action keyed `DEFAULTS`. `ActionVisitor` (264-285) expands it into OK, Save, Delete, Add, ZoomOut, Cancel, Remove and Edit, all sharing its conditions.
  - So `view.getAction("Save")` can be null even though Save is rendered.
  - Built-in actions must be matched on `getImplicitName()` during the visit, not looked up by name.
- `ActionImpl.getName()` is the explicit name, else the class name, else the built-in name. Two view actions can share a class under different names.
- A `Button` has no conditions of its own; it uses its action's (`Button.java:35`, `ViewJSONManipulator.visitButton` 1143-1149).
- Each event handler has a start hook (`visitOn*EventHandler`), the handler's actions (`visitServerSideActionEventAction`), and an end hook (`visitedOn*EventHandler`).
  - The flags passed are the **container's**, not the widget's own (`ViewVisitor` 1576-1579, 1469-1472). The visitor must combine them with the widget's own `visible()`.
- `<component>` fragments are expanded inline with action names mapped to the host view's names (`ComponentViewVisitor.translateAction`). Their conditions evaluate against the host bean.
- Data grid container columns can hold `Link`, `ContentUpload`, `StaticImage`, `DynamicImage`, `Blurb` and `Label`, **not `Button`** (`DataGridContainerColumn.java:38-46`). A row-level trigger is a `<link>` with an action reference.
- `ViewVisitor.visible()` and `enabled()` return true by default. The pattern to copy is `ViewJSONManipulator` 1016-1034: `! bean.evaluateCondition(condition)`.
  - `BindUtil.evaluateCondition` handles `true`, `false`, `{el:...}` and named or `not` conditions.
  - `DynamicBean.evaluateCondition` always returns false.

### Related defect found (fixed separately, 2026-10-20)

`FacesAction` line 90 caught `SecurityException` without importing Skyve's class, so it caught `java.lang.SecurityException`, and Skyve's `SecurityException` fell into the generic error branch. The import was lost in a merge around `e19aded29a` (May 2026). It has been restored, and `FacesActionTest.executeRethrowsSkyveSecurityExceptionAndMarksRollback` covers it.

## Implementation plan

Make each work item its own commit, in this order.

### W1: `ActionUnavailableException`

- Add `skyve-core/src/main/java/org/skyve/domain/messages/ActionUnavailableException.java`, extending `DomainException implements MessageException`, modelled on `OptimisticLockException`.
  - That base means both clients already show its message without a new catch.
  - The exception carries no bindings, so the message shows as a global message, not on a field.
- Constructor: `(User user, String documentName, String actionName)`.
  - The developer message names the user, the document and the action.
  - The user message comes from `Util.nullSafeI18n(MESSAGE_KEY)`.
- Add the key `exception.actionUnavailable` ("This action is not available any more. The screen has been refreshed.", or similar) to `skyve-war/src/main/java/resources/i18n.properties` and to the translated bundles `i18n_ar` (a partial bundle without the `exception.optimisticLock.*` keys), `i18n_de`, `i18n_fa`, `i18n_hi` and `i18n_vi`.
  - `i18n_de` is ISO-8859-1. Search it with `grep -a` and edit it as bytes.

### W2: `ActionAvailability`

Add `skyve-core/src/main/java/org/skyve/impl/metadata/view/ActionAvailability.java`, a `final class extends NoOpViewVisitor`.

**Why a new class** (the change-restraint rule requires the justification):

- It has five production callers (W5, W6, W7). Inlining the walk at each would copy the trigger rules five times.
- `ViewJSONManipulator` can't be reused, because it lives in `skyve-web` and the servlets in W6 and W7 don't build one.
- `ViewRenderer.preProcessAction` can't be reused either. It is private and mixed with label and icon state.

**Entry points.** These are the only public API; the constructor is private.

```java
/** Throws ActionUnavailableException (after logging a warning) unless the view offers a reachable trigger. */
public static void check(@Nonnull User user, @Nonnull Document document, @Nonnull View view, @Nonnull String uxui,
							@Nonnull Bean bean, @Nonnull String actionName);
public static void check(@Nonnull User user, @Nonnull Document document, @Nonnull View view, @Nonnull String uxui,
							@Nonnull Bean bean, @Nonnull ImplicitActionName implicitName);
/** For a link in a data grid row: the link is found in the form view; the action is resolved in the row document's view. */
public static void checkRow(@Nonnull User user, @Nonnull Document formDocument, @Nonnull View formView, @Nonnull String uxui,
							@Nonnull Bean formBean, @Nonnull String gridBinding, @Nonnull Document rowDocument,
							@Nonnull View rowView, @Nonnull Bean rowBean, @Nonnull String actionName);
```

Logging goes inside `check`, so every call site gets it. Use the existing `Category` / `SkyveLoggerFactory` pattern, at warn level, naming the user, `module.document`, the action, the form and grid bindings, and the bean's bizId. Do not log bean values.

**Rules.** Override `visible()` and `enabled()` as in `ViewJSONManipulator`. While walking, record the reachable names in two sets: custom names (both `getName()` and `getResourceName()`) and built-in names. *As built:* a requested name is matched against the action's class (resource) name only, because every server lookup resolves the class from the requested text (Mike, 2026-10-21, after the final PR review).

| Trigger | Reachable when | Records |
|---|---|---|
| Toolbar action (`visitCustomAction`, `visitXxxAction` incl. `DEFAULTS` expansion) | `inActionPanel` is not false, and `visible(action) && enabled(action)` | name + resource name, or built-in name (*as built:* class name only) |
| Form `<button>` (`visitButton`) | `parentVisible && parentEnabled` and the referenced action is visible and enabled | the referenced action's names |
| `<link>` with an action reference (outside grids) | `parentVisible && visible(link)` and the referenced action is visible and enabled | the referenced action's names |
| `<server action>` event handler | the widget is reachable: `parentVisible && visible(widget)` when the widget is `Invisible`. For grid events, the grid's own visibility. Enabled-ness is ignored (decision 3). Track it on a stack: push in `visitOn*EventHandler`, pop in `visitedOn*EventHandler`. | the target action's names (look up `view.getAction(name)`; the action's own conditions are **not** applied, because the event fires regardless) |
| Grid-row link (`checkRow` only) | The grid with `binding == gridBinding` is visible, and a link in its container columns references the action and is visible against **`rowBean`**. Then the action is declared in the row document's view. The row action's own `invisible` and `disabled` conditions are **not** applied: SmartClient, the only caller, renders row links from the link's own `invisible` condition alone (`ViewJSONManipulator.visitLink`), so applying them would refuse a live link (Mike, 2026-10-20, replacing the PrimeFaces `ExecuteActionAction` semantics first planned here). | — |

`check` passes when the requested name is in the matching set.

As built (accepted at W2 review, 2026-10-20):

- The walk keeps one `reachable` flag rather than two name sets, and one event-source slot rather than a stack: event actions hold no widgets, so handlers never nest.
- The warning names no form binding, because the approved signatures carry none.
- A view's `refreshAction` is not a trigger. Only `MetaDataServlet` publishes `refreshActionName`, and no client in this repo (SmartClient, PrimeFaces, Flutter, Vue) sends it.
- Events of a data grid or repeater column's input widget need only the data widget (and its containers) to be visible: SmartClient does not render a column widget's `invisible` condition. Such events reach the server without `_g`, so they are checked with `check`, not `checkRow`. W8 checks PrimeFaces against this rule.
- Links through an `ImplicitActionReference` are not triggers (SmartClient renders only a client-side grid Remove for them; PrimeFaces a placeholder label). A `<component>`'s own `invisible` condition is not applied, matching `ViewVisitor.visitComponent` and both renderers.

Notes for the executor:

- Built-in Save, OK and Delete match on `getImplicitName()`. A view with no `<actions>` element offers no built-in actions, so they are refused. That matches the renderers, which render none.
- The `DEFAULTS` expansion calls the per-type visit methods with the defaults `ActionImpl`, so they share its conditions.
- Verify the "link to a disabled action" rule against both renderers in W8. If a renderer shows such a link as enabled, change the rule to drop the action's own `enabled` and keep only visibility.
- Do not touch the bean beyond `evaluateCondition`.

**Tests.** Add `skyve-core/src/test/java/org/skyve/impl/metadata/view/ActionAvailabilityTest.java`, with Mockito `CustomerImpl` and `ModuleImpl` as in `ViewVisitorTest`, and hand-built `ViewImpl`s.

- Mock the bean so that `evaluateCondition` answers from a `Map<String, Boolean>` truth table, handling the `not` prefix and the `true`/`false` literals the way `BindUtil` does.
- **Exhaustive cases.** For each view below, enumerate every truth assignment of its conditions. Compare `check` against an expected formula written independently in the test as plain boolean code. Use one `@Test` with `assertAll(Stream<Executable>)`, never `@TestFactory`: surefire reports a failing dynamic test as a flake, and the build passes.

  The views:
  - a toolbar action with `disabled` and `invisible`;
  - an action with `inActionPanel="false"` plus a `<button>` in a form with a `disabled` condition;
  - a button in a disabled tab;
  - an event handler targeting a `disabled` action on a visible field;
  - an event handler on an invisible field;
  - two actions sharing a class with different conditions;
  - `<defaults/>` with a `disabled` condition, checked for Save, OK and Delete;
  - a view with no actions;
  - a link with an action reference;
  - grid-row links via `checkRow`;
  - an action referenced only from a `<component>` fragment;
  - inline `{role:...}` conditions with a mocked user. `skyve-web` has no EL on its test classpath; core may not either.
- A test that a refusal throws `ActionUnavailableException` with the i18n message.

### W3: every view in the repo

Add `skyve-war/src/test/java/org/skyve/impl/metadata/view/ActionAvailabilityAllViewsH2Test.java`, extending `AbstractSkyveTestDispose`. For every module, document, view type and UI type in the test customer, plus customer overrides (for example `customers/test/...`):

- Run the visitor with the all-true and all-false truth tables and assert that it never throws.
- Write a report to `target/action-availability-report.txt`, listing:
  - actions reachable only through an event handler;
  - class names shared by several named actions;
  - actions with no trigger under any assignment.

  The report does not fail the build. It is the pre-release risk list, and it should be run over Basix Cloud's views too.

As built (2026-10-20):

- It walks every customer, module, owned document and router UX/UI, and the **edit and create** views only. List, pick and params views carry none of the actions the servlets check.
- Truth tables: the literals `true` and `false` evaluate as written. Every other condition, inline `{el:...}` and `{role:...}` included, takes the table value. Every rule needs its conditions false, so all-false is the most permissive assignment and "refused under all false" means "refused for every bean state".
- The gate fails on any exception other than `ActionUnavailableException`, on an empty walk, on a check that passes under all true but is refused under all false, and when DataMaintenance `CheckContent` stops being unreachable in exactly the non-desktop UX/UIs.
- Extra report sections: grid-row links refused under all false; custom and class-based actions whose class collides with a built-in name that `/smartedit` takes as built-in (OK, Save, ZoomOut, Print); views with a `refreshAction`; `<server>` handlers on widgets revealed by `<toggleVisibility>` or `<setInvisible>`, and the other reveal uses.
- The test lives in `skyve-war/src/test/java`, which `skyve:assemble` does not copy, so running it over Basix Cloud's views needs the class copied into that project.
- Result in this repo: the 13 actions with no trigger are all DataMaintenance actions declared in the default view and reachable only in the desktop view (Backward compatibility item 7). `admin.MonitoringDashboard` declares a custom action named `Save` (class `Save`), which collides with the built-in Save that SmartClient's `_a=Save` selects; W5 handles it.

### W4: `kitchensink.ActionsFixture`

Add a new persistent document with child `ActionsFixtureRow`, following the `DialogsFixture` and `ConditionsFixture` layout. The view needs:

- a custom action with a `disabled` condition bound to a boolean field;
- an `inActionPanel="false"` action targeted only by a `<server>` handler on a field;
- an `invisible` action with no trigger;
- a `<button>` in a form with a `disabled` condition;
- a `<button>` in a tab with a `disabled` condition;
- two actions sharing one class with different conditions;
- `<defaults/>` with a `disabled` condition on Save;
- a Delete with a condition;
- a data grid of rows with a container-column `<link>` to a row action. Give the row action a condition on the row and declare it in the row document's view;
- a download action whose `prepare()` flips the condition that disables it;
- a BizExport and an Upload action with conditions.

To add the document:

1. Register the document, its actions and its privileges in `kitchensink.xml`.
2. Generate domain by running `DomainGenerator` directly in the mirror. See the generation recipe in the validation plan, including the "new document" bootstrap: generate without the views first, compile, restore, generate again.
3. Copy the generated classes back.

As built (2026-10-20):

- OK, Save, Delete and Cancel are declared explicitly (`<ok disabled="saveDisabled"/>`, `<save disabled="saveDisabled"/>`, `<delete disabled="deleteDisabled"/>`, `<cancel/>`), not through `<defaults/>`. `<defaults/>` gives every built-in one shared condition, so Save and Delete could not be refused independently. Refusal through the `<defaults/>` expansion is covered by the W2 unit tests only.
- The grid-row link carries `invisible="rowLinkHidden"`, evaluated against the row (SmartClient and `checkRow`). The row action `RunFromRow` is declared in the row view with `inActionPanel="false"` and its own `disabled` condition, which PrimeFaces applies to the row link.
- `ActionsFixture` also declares `rowLinkHidden`, as a constant-false stub. `ViewValidator` checks a container-column link's condition against the form document, although both renderers evaluate it against the row (see Better engineering notes).
- A BizImport action (`ActionsImport`) is added for W7, alongside BizExport and Upload.
- The row view also offers a BizExport (`ExportFromRow`), so that W7 can show a zoomed-in export being allowed by the row's own view (Mike's `_b` request, 2026-10-21).

### W5: SmartClient enforcement

All changes are in `skyve-web/.../smartclient/SmartClientEditServlet.java`.

1. **Custom actions and Download, in `apply()`.**
   - After `manipulator.visit()` and before `applyJSON`, when `mutableCustomActionName != null`: call `ActionAvailability.check(user, formDocument, formView, uxui, formBean, name)`.
   - When `gridBinding != null`, call `checkRow` instead, with `processDocument`'s view and `processBean` as the row.
   - Keep the existing `canExecuteAction` check after it.
   - Skip push requests (`PUSH_ACTION_NAME`). They bypass the manipulator already.
   - Keep the `formView` that the manipulator is built with in a local variable and reuse it for the check.
2. **OK and Save, in `apply()`.** At the same point, when `implicitAction` is OK or Save, call `check(..., implicitAction)`. Do not check ZoomOut (decision 7) or a null implicit action, which is a plain update.
3. **Delete, in the `remove` operation.** Before `remove(...)`, call `check(..., processBean, ImplicitActionName.Delete)` against the bean's own document. Use that document's edit view, or its create view when the bean is not yet created (as `apply()` and the fetch do), for the request's UI type. A remove with no conversation bean is not checked, because it deletes nothing.
4. **Refusal response.**
   - In the `processRequest` catch, next to the `QuestionException` branch, add `ExceptionUtils.throwableOfType(t, ActionUnavailableException.class)`. It writes status `-51` with the message and a `conditions` map. *As built:* the message only (warning-only fallback, Mike 2026-10-21).
   - The map holds the form view's condition names mapped to their values, evaluated against the conversation bean. That is the state after rollback, so it is what the client should show.
   - Produce the map from the existing manipulator's condition collection (the same `addCondition` values a fetch returns) rather than a new walk, if it can be reached cleanly.
5. **`editView.js`.**
   - Add `_UNAVAILABLE: -51` next to `_QUESTION`.
   - In the `doAction`, `saveData` and `removeData` callbacks, on -51: `isc.warn(message)`, then merge only the returned condition values into the view's values. Do not refetch, because a fetch would discard the user's unsaved edits.
   - Then re-run whatever re-evaluates button `disabled` and visibility from those values.
   - *As built:* the spike found the client refresh feasible, but there was no clean server source for the condition values, so the warning-only fallback was used (Mike, 2026-10-21).
   - **Spike this first.** Find how `BizButton` reads condition values (editView.js about 1052, 1163-1184). If merging condition values can't cleanly refresh the buttons, fall back to a warning only, and record that in AI validation results.
6. **Tests.** Add `skyve-war/src/test/java/modules/kitchensink/ActionsFixture/ActionsFixtureSmartClientH2Test.java`, modelled on `DialogsFixtureSmartClientH2Test`: a real session, webId, csrf token and fetch-then-apply. For each fixture trigger, assert both outcomes:
   - an available action runs;
   - an unavailable action gets -51, the action has not run, and nothing was saved.

   Also assert:
   - an event-handler target runs even though it has no button;
   - the action with no trigger is refused;
   - a shared class runs when either named action is available;
   - OK and Save are refused while `saveDisabled` is true;
   - Delete is refused when its condition is true;
   - a grid-row link action is refused when its row condition is true;
   - a `rerender` fetch with `_a=ZoomOut` is never refused.
   - *Added at the final review:* a request naming an action by a name that differs from its class is refused, and a custom action merely *named* Save (class `AskOnce`) does not take over the built-in Save (`aCustomActionMerelyNamedSaveDoesNotTakeOverTheBuiltInSave`).

### W6: download grant

1. Add a serialisable field to `AbstractWebContext` (`skyve-core/.../impl/web/AbstractWebContext.java`) recording the last prepared download: action name, form binding, grid binding and bizId. Give it a setter and a matcher named for their effect, for example `recordPreparedDownload(...)` and `isPreparedDownload(...)`.
2. Set it right after `prepare()` in `SmartClientEditServlet.apply()` and in `ExecuteDownloadAction.callback()`, using the same values passed to `WebUtil.getDownloadActionUrl`.
3. In `DownloadServlet.processDownload`, after the `canExecuteAction` check, refuse with `ActionUnavailableException` unless `webContext.isPreparedDownload(...)` matches the request parameters. Do not clear the grant (range requests).
4. Tests:
   - an H2 test with `MockHttpServletRequest`, as in `SmartClientEditServletH2Test`: a download without prepare is refused, and one after prepare is served;
   - the fixture download whose `prepare()` disables its own button is still served;
   - a HEAD request followed by a GET is served.

### W7: other entry points that take the action name

- **`BizportExportServlet`** (about line 86): after `canExecuteAction`, `check(...)` against the conversation bean and the edit or create view of its document.
- **`ContentUploadView`** (about line 623) and **`BizportImportView`** (about line 172): the same, when the page resolves the action and before running it.
- These run after the client's `saveInstance`, so the bean already holds the posted values. Record this as a known limitation in Better engineering notes; it is low risk because these are buttons the user just clicked.
- Tests: H2 tests driving each entry point for an available action and an unavailable one.

### W8: renderer checks and manual verification

1. **SmartClient.** In a throwaway H2 test, generate the fixture view's SmartClient code and JSON for several truth assignments. Confirm that the buttons the client enables are exactly the triggers `ActionAvailability` allows. Answer:
   - Does SmartClient disable a `<button>` inside a disabled form? Inside a disabled tab?
   - How does it render a `<link>` to a disabled action?

   Adjust the W2 rules to match, and record the answers in the plan.
2. **PrimeFaces.** Confirm by reading `TabularComponentBuilder` that every form `<button>` and action link gets `setDisabled` and `setInvisible` from its action's conditions and its container's conditions, including links in grid rows. List any gaps as follow-ups.
3. **Flutter.** Read the Flutter client's action rendering in `skyve_flutter/lib` and record whether it honours `disabled` and `invisible`. Do not change Flutter in this milestone.
4. **Browser.** Do the user validation steps below in both renderers, using devtools to resend a refused request.

Results of the automated parts (2026-10-21; evidence in the M1 ledger):

- **SmartClient matches `ActionAvailability` for the fixture.** A throwaway H2 probe generated the `ActionsFixture` code and fetch JSON for 66 truth assignments, and replayed `editView.js`'s condition logic over it. The triggers the client enables equal what the server allows for every request the client can send. `node --check` passes on the `ActionsFixture` and `ActionsFixtureRow` scripts.
- **(a) A `<button>` in a disabled form or tab: SmartClient disables it.** A form's `disabledCondition` reaches the button through its `CanvasItem`. `TabSet.setTabDisabled` disables the pane too (`disablePaneWithTab`), and a disabled `<tabPane>` is honoured. The W2 button rule matches.
- **(b) A `<link>` to an action.**
  - **Form level.** SmartClient renders the link as a blurb carrying only the link's own `invisibleCondition`, so a link to a disabled action looks enabled. The pre-authorised rule change was therefore applied: a link no longer needs its action to be enabled, only visible. These links are broken today anyway. `HrefProcessor` sends `_g='null'`, which fails at `getElementInCollection` with -1 before the check; the link also sends the action's name where the server looks up a class. (Since PR #363 on 2026-10-28 the link sends the class; `_g='null'` remains.) Links outside forms (vbox, hbox, tab) are not rendered at all (`renderLink` is a TODO), and each one shifts the manipulator's anonymous format numbers by one.
  - **Grid rows.** Links are hidden per row by the link's own condition, which matches `checkRow`.
- **(c) Toolbar.** SmartClient honours `inActionPanel`, `invisible` and `disabled`. The client also hides some built-ins the server allows (ZoomOut and Remove at top level, Delete on an unpersisted bean, and so on), so the server is more lenient there, which is harmless.
- **(d) A `<server>` handler on a grid column's input widget renders invalid JavaScript.** `changed:function(){...}` is written inside `_fields:[` (`node --check` fails). No view in the repo has one. The W2 column-widget rule is lenient for these events.
- **PrimeFaces audit (reading).**
  - Form `<button>`s get the form's and the action's conditions.
  - Grid-row links get the row action's conditions and drop the link's own.
  - Gaps:
    - A disabled `<tabPane>` is ignored. Buttons inside it can be clicked and run unchecked, which weakens decision 6's premise, and an Upload inside it is now refused by `ContentUploadView`.
    - A button in a disabled `<tab>` has no `disabled` of its own. Its click is dropped by `Tab.processDecodes`, but Upload is client-side and still opens.
    - Action links outside grids probably fail while rendering (`obtainActionForActionReference` with a null data-widget binding).
    - BizExport and BizImport bind to `#{skyve.bizexport}` / `#{skyve.bizimport}`, which no bean defines.
    - Rendering a `widgetId` fragment loses its parents' conditions.
- **Flutter (reading).**
  - It honours only `inActionPanel`. It evaluates no `disabled` or `invisible` conditions on actions or containers, and always uses the edit view.
  - Every button except Cancel posts `_a=<name>` with `_operationType=update` (Delete included). Links, grids and `<server>` events are not implemented, so `_g` is never sent.
  - After this milestone, Flutter clicks on actions whose own or container conditions make them unavailable are refused, and so are actions on new beans of documents whose create view differs from the edit view. Flutter only `debugPrint`s the refusal.

### W9: documentation

- Add a `docs/skyve-10-changes.md` entry covering:
  - what is refused;
  - what counts as a trigger;
  - the migration advice from [Backward compatibility](#backward-compatibility);
  - the -51 response for custom clients.
- Add one paragraph to `docs/inline-conditions.md`: action `visible` and `enabled` conditions are now enforced on the server, not only shown.
- *As built:* the entry became its own guide, `docs/server-side-action-availability.md`, linked from the `docs/skyve-10-changes.md` index (Mike, 2026-10-21). The `inline-conditions.md` addition is a short "Enforcement on the Server" section.
- Fill in the [AI validation results](#ai-validation-results) and [Acceptance checklist](#acceptance-checklist) sections.

## Acceptance checklist

- [x] W1: `ActionUnavailableException` exists. The message key is in all six bundles, with `i18n_de` checked with `grep -a`.
- [x] W2: `ActionAvailability.check` and `checkRow` are implemented as specified. The exhaustive tests pass and fail when a rule is deliberately broken (red/green).
- [x] W3: the all-views test passes and the report is produced. The report has been reviewed, with any surprising unreachable action explained.
- [x] W4: `ActionsFixture` generates, compiles and renders in both renderers.
- [x] W5: SmartClient refuses unavailable custom, Download, OK, Save, Delete and grid-row link actions with -51. Event-handler targets and rerender fetches still work. The client shows the message and refreshed buttons, or the fallback is documented.
- [x] W6: `DownloadServlet` serves only prepared downloads. A self-disabling download still works. Range and HEAD requests work.
- [x] W7: `BizportExportServlet`, `ContentUploadView` and `BizportImportView` refuse unavailable actions.
- [x] W8: the renderer questions are answered and recorded, and the W2 rules match the renderers. The Flutter behaviour is recorded.
- [x] W9: the docs are updated.
- [ ] Every touched Java file is free of diagnostics. *Changed lines are clean (ECJ, SonarQube and VS Code). Touched files keep pre-existing warnings on unchanged lines, which were left alone by rule.*
- [ ] `mvn -B package` passes in the mirror. *Every module passes except 3 pre-existing `ContentServletH2Test` failures, reproduced on the baseline.*

## Better engineering notes

**Follow-ups (not in this milestone):**

- **SmartClient `_doc` hardening.** The action class is looked up on the request's `_doc`, which is never checked against `processBean`'s own document. Requiring them to match is a separate hardening change, and it needs care with polymorphic beans. The same applies to the other entry points. `DownloadServlet` and `BizportExportServlet` resolve the action class from `_doc`. The SmartClient export's `_b` (added in W7) picks the bean that is checked, while the export still runs against the conversation. A forged `_b` naming a child whose view declares an export of the same class name could therefore pass the check. The download grant does not bind `_doc` or the bean suffix of `_c`.
- **PrimeFaces drops refused clicks silently.** A click whose condition flipped since render does nothing, with no message. Making it visible would need a decode-time hook; consider it only if users report it.
- **Metadata opt-out.** If downstream apps prove to rely on calling unreachable actions from custom JavaScript, a per-action metadata opt-out is the narrow escape hatch.
- **Grid conditions.** Add, edit and remove on grids (`disableAdd`, `disableEdit`, `disableRemove`) and on lookups are the same class of problem, and are not covered here.
- **OK and Save with a form binding.** SmartClient never sends these, and a forged one runs only `preExecute`. They are checked like any OK or Save, with no extra context rule.

- **Validator and container-column link conditions.** `ViewValidator.visitLink` validates a container-column link's `invisible`/`visible` condition against the form document, while SmartClient (`resolveRowInvisibleConditions`) and `ActionAvailability.checkRow` evaluate it against the row, and PrimeFaces ignores it. A row-only condition therefore needs a stub on the form document (`ActionsFixture.rowLinkHidden`), and `ConditionsFixture`'s grid link `visible="shownByExpression"` names a condition that `ConditionsFixtureRow` does not define. The validator should resolve these conditions against the row document, as `validateMessageExpressions` already does; the stub can then go.
- **Renderers disagree on grid-row link conditions.** SmartClient applies only the link's own condition, per row; PrimeFaces applies only the row action's conditions and drops the link's. W8 records the detail.

- **Refresh after a SmartClient refusal.** The client side is feasible (merge the values with `_vm.setValue`, then re-run `_processWidgets` and `_manageActionPanel` in the `_askable` wrapper). But `ViewJSONManipulator` keeps conditions in private state, mixed in with the field bindings, so the server has no clean way to produce them. A follow-up could record them through a protected `addCondition` override, or through a package-private accessor.

- **PrimeFaces gaps found in W8.**
  - Honour a disabled `<tabPane>`, and disable Upload and other buttons inside a disabled `<tab>` or `<tabPane>`.
  - Fix BizExport and BizImport binding to non-existent `#{skyve.bizexport}` / `#{skyve.bizimport}`.
  - Fix action links outside grids.
- **SmartClient gaps found in W8.**
  - Form-level action links send `_g='null'`, which fails at `getElementInCollection` with -1 before the check.
  - *Done 2026-10-28:* links send the action's class, its client validation and its confirmation (PR #363, merged as `89614845bd`, with fix-up `8f4698bab8` for escaping the confirmation in the href). `ActionsFixtureSmartClientH2Test.aRowLinkRendersACallThatTheServerAccepts` sends the call a rendered row link makes; the fixture's row action is now named `runFromRowLink`, apart from its class `RunFromRow`.
  - Links outside forms are not rendered, and each one shifts the anonymous format numbers.
  - `<server>` handlers on grid column widgets produce invalid JavaScript.
  - Fixing form-level links must also render the action's `invisible` condition, which the link rule requires.
- **Flutter.** Evaluate action and container conditions, use the create view for new beans, and show refusals.

**Known limitations:**

- SmartClient BizExport, BizImport and Upload are checked against the bean *after* the preceding `saveInstance` (W7). A saved edit that changes the action's condition therefore decides the check, even if the button was rendered before it. PrimeFaces opens the upload page without saving.
- Event triggers ignore whether the widget is enabled (decision 3).
- A SmartClient refusal (-51) shows the message only. The buttons are not refreshed until the next fetch or rerender. This is the W5 fallback, chosen by Mike on 2026-10-21 over adding a construct that exposes the manipulator's conditions.
- A download grant is recorded on the conversation's web context, one per conversation. Preparing download B before A's request arrives refuses A. The grant does not bind the request's `_doc` or the bean suffix of `_c` (see the `_doc` hardening follow-up).
- In PrimeFaces, `SkyveFacesPhaseListener` does not cache the conversation when an ERROR-severity FacesMessage is queued, but `ExecuteDownloadAction` still runs its download script. If `prepare()` or `postRender` queues an error message, the grant is lost and the download is refused. Before M1 it was served from the previously cached conversation.
- A refused SmartClient export or download returns a readable HTML page. The client opened the URL with `window.location.assign`, so the browser leaves the app and unsaved edits are lost, as other export and download errors already do (Mike, 2026-10-21).
- The upload and Bizport import pages are always requested at their own paths (`/upload.xhtml`, `/bizImport.xhtml`), so their UX/UI comes from the selector. A PrimeFaces page that took its UX/UI from a `<direct>` route, or a UX/UI preference changed in another tab, can therefore be checked against a different UX/UI's view. Recording the rendered UX/UI on the web context would remove this but is a new construct.
- When SmartClient sends `_a=OK` or `_a=Save`, and the form view declares a custom action whose class is `OK` or `Save` (for example `admin.MonitoringDashboard`), the custom action's triggers decide, and the built-in branch runs as it did before. A view with both a built-in Save (or OK, or `<defaults/>`) and a custom action classed Save would be judged by the custom action alone, because both send `_a=Save`. An unavailable custom action would then block the enabled built-in Save. No view in this repo has both, and the W3 report's collision section lists such views. Mike kept this as a known limitation on 2026-10-21.
- Grid-row links ignore the row action's own `invisible` and `disabled` conditions, because SmartClient renders them from the link's condition alone. Making SmartClient honour the row action's conditions per row (the existing `ROW_CONDITION_DELIMITER` mechanism) would let `checkRow` apply them; that is a follow-up.
- Event-handler widgets revealed client-side by `<toggleVisibility>` or `<setInvisible>` are refused (see Backward compatibility item 6).

**Risks:**

- With no flag (decision 5), the W3 report is the only way to find risky views before release. Run it over Basix Cloud's views as well as this repo's before shipping.
- The W5 client refresh depended on the spike; the fallback (warning only) was used.

## AI validation plan

Edit in the shared checkout. Build and test in an rsync'd mirror in the session scratchpad, because VS Code's compiler overwrites `target/classes` in the checkout.

- Exclude `target`, `.git`, `node_modules`, `deployments`, `.settings`, `.classpath`, `.project`, `skyve_flutter` and `skyve-vue`.
- Always use `-o` and `-am`.

1. Core:

   ```
   mvn -o -pl skyve-core test -Dtest=ActionAvailabilityTest
   ```

   Red/green: break one rule, for example ignore `parentEnabled` for buttons, and confirm the test fails with `BUILD FAILURE` and no "Flakes" line.
2. Domain generation for W4: run `org.skyve.impl.generate.DomainGenerator` directly from `skyve-war/`.
   - First `rsync -a --exclude '*.java' src/main/java/ target/classes/`, because the generator reads metadata from `target/classes`.
   - Re-generate after every sync.
   - Copy only the new generated files back.
3. One skyve-war test class:

   ```
   mvn -o -pl skyve-war -am test -Dtest=<Class> -DskipUnitTests=false -DskipIntegrationTests=true -Dsurefire.failIfNoSpecifiedTests=false
   ```

   Run this for `ActionAvailabilityAllViewsH2Test`, `ActionsFixtureSmartClientH2Test` and the W6 and W7 tests. Read the results from `target/surefire-reports/TEST-*.xml`.
4. Regression: `DialogsFixtureSmartClientH2Test`, `DialogsFixturePrimeFacesH2Test`, `SmartClientEditServletH2Test`, `SmartClientEditServletErrorResponseTest`, `ConditionsFixtureH2Test` and `ViewJSONManipulatorTest`.
5. Fixture view script: write the SmartClient code for `ActionsFixture` to a file from a throwaway H2 test and run `node --check` on it.
6. Diagnostics: `get_errors` on every touched Java file.
7. Baseline: `mvn -B package` in the mirror. It runs about 21,000 tests.

## AI validation results

Executed 2026-10-20 to 2026-10-21 by an orchestrating agent with subagents, in the shared checkout. Nothing is committed. All builds ran in rsync'd mirrors of the checkout. Changed lines were measured against a snapshot taken before W1, not against `HEAD`. The per-item ledger, with every review finding and how it was resolved, is in the session scratchpad (`m1/ledger.md`).

**Per item.** Each item passed its gates, then had a rubric review, a code-slop review and a plan/Javadoc/tests review. Reviews ran up to three rounds; fixes were re-gated after each round.

- **W1.**
  - `DomainMessagesTest`: 74 tests pass.
  - The key is in all six bundles (`i18n_de` checked as Latin-1 with `grep -a`), and also in the skyve-core test bundle.
- **W2.**
  - `ActionAvailabilityTest`: 16 tests. Each is exhaustive over every truth assignment, and the largest covers 1,024.
  - Red/green: 34+ deliberate rule breaks each failed the build. None was reported as a flake.
- **W3.**
  - The report covers 1,395 view combinations and 6,074 action checks, plus 25 row-link checks.
  - The gate fails on:
    - any exception other than `ActionUnavailableException`;
    - an empty walk;
    - a monotonicity anomaly;
    - a drift in the DataMaintenance `CheckContent` result.
  - Report findings, all explained under W3 "As built":
    - 14 actions with no trigger: the DataMaintenance default-view actions (Backward compatibility item 7), plus the fixture's `RunWithoutTrigger`;
    - 2 class collisions with a built-in, both on MonitoringDashboard;
    - no refused row links.
- **W4.**
  - Generated with the two-pass bootstrap.
  - Renders in both renderers: `node --check` on the SmartClient code, and the PrimeFaces component tree built in a throwaway H2 test.
- **W5.**
  - `ActionsFixtureSmartClientH2Test`: 24 tests through the real servlet.
  - Plan regression list, all passing: `DialogsFixtureSmartClientH2Test` 15, `DialogsFixturePrimeFacesH2Test` 26, `SmartClientEditServletH2Test` 11, `SmartClientEditServletErrorResponseTest` 42, `ConditionsFixtureH2Test` 11, `ViewJSONManipulatorTest` 101.
  - The client refresh spike succeeded on the client side. The fallback (warning only) was used, by Mike's choice.
- **W6.**
  - Grant unit tests, a matrix of 21 cases.
  - `WebUtilTest` ties the URL the grant expects to `getDownloadActionUrl`.
  - H2 tests of the SmartClient and PrimeFaces paths: self-disabling download, HEAD, ranged GET, and refusals.
- **W7.**
  - `ActionsFixtureBizPortAndUploadH2Test`: 12 tests, covering both outcomes for each entry point, the create view, and a zoomed export allowed and refused via `_b`.
- **W8.** See the results under W8.
- **W9.** Reviewed for accuracy against the code over three rounds.

**Final milestone gate (2026-10-21).**

- **Full `mvn -o -B package`** in a fresh mirror: 21,999 tests.
  - The only failures were 3 in `ContentServletH2Test`: 404 where 200 or 206 was expected.
  - These fail identically on a mirror of the pre-milestone baseline, so they are pre-existing and not caused by M1. Their cause was not investigated.
  - No flaky entries.
  - `ConverterRegistrationTest` needs `skyve-vue/src` in the mirror, because the standard mirror excludes it.
- **Aggregate JaCoCo** over the milestone's tests and the regression list (509 tests):
  - changed production lines: 239/239 (100%) of lines and 130/132 (98.5%) of branches;
  - `ActionAvailability`: 100% of lines;
  - the two partial branches cannot be reached with today's widgets and metadata: a non-`Invisible` event source, and an action whose class only is Save;
  - re-measured after the class-only change: `ActionAvailability` 133/133 lines, 73/74 branches; `SmartClientEditServlet` 39/39 lines, 24/24 branches.
- **Batch ECJ:** 0 problems on changed lines. There are pre-existing warnings on unchanged lines of touched files: 7 in `SmartClientEditServlet`, 2 in `ContentUploadView`, 1 in `BizportExportServlet` and 1 in `DownloadServlet`.
- **SonarQube** (local, Sonar way): 0 issues on changed lines.
- **Production suppressions**, both commented:
  - `java:S110` on `ActionUnavailableException`, the sibling exceptions' pattern;
  - `java:S107` on `ActionAvailability.checkRow`, whose approved signature has 10 parameters.
- No `@TestFactory` is used.
- Formatting was done by hand. Spotless was not run, per Mike's standing instruction.
- **Whole-milestone review** (a rubric review and a PR review): no blocking finding was left open.
  - Fixed: matching by class name only (Mike), the download-URL backward-compatibility item, and stale plan text.
  - Waived under Change Restraint: shared helpers for the repeated view-selection, refusal-page and collision-matching code.

**Not verified:**

- Browser checks in either renderer (see User validation suggestions).
- The Flutter behaviour beyond reading the code.
- A 206 response through `DownloadServlet`: the fixture download is bytes-backed.
- The downstream recipe for running the W3 report.

## User validation suggestions

Deploy and open `kitchensink.ActionsFixture` in **SmartClient** (desktop) and **PrimeFaces** (external). In each renderer:

1. Tick the field that disables the custom action. The button greys out.
   - In SmartClient devtools, resend the last `doAction` request with that action. Expect the "not available" message and nothing changed (the buttons refresh only on the next fetch or rerender).
   - In PrimeFaces, nothing happens, as before.
2. Change the field with the `<server>` handler. Its hidden target action runs.
3. Disable the form and the tab that hold buttons. Their buttons can't be clicked, and a resent request is refused.
4. With the Save `disabled` condition true, a resent Save request is refused and the record is unchanged. Do the same for Delete.
5. Click the self-disabling download. The file downloads, and the button is then disabled. Open the download URL directly in a new tab without clicking: it is refused.
6. In a grid row whose row condition disables the row link, resend the row link's request. It is refused.
7. Run the W3 report against Basix Cloud and look over the "event-only" and "unreachable" lists for actions that custom JavaScript calls.
