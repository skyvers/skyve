# Skyve Server-Side Confirmation: Feature Spec

## Summary

Server code in Skyve cannot ask the user a question today. The only confirmation is the static `confirm` text on a view action, which is shown before the request is sent and cannot depend on data or server logic.

This spec adds **server-side questions**, using the smallest mechanism that covers the cases in [What it is for](#what-it-is-for). Server code calls `EXT.getDialogs().ask(...)` where it needs a decision. When the question has no answer yet:

1. `ask` throws.
2. Skyve rolls the request back. Neither renderer saves the conversation after a failed request, so the conversation stays as it was before the request.
3. The client shows the question.
4. The client sends the same request again, adding the answers given so far in a `_q` request parameter.
5. On the rerun, `ask` finds its answer in `_q` and returns it, and the code carries on past the question.

A request can ask several questions. Each rerun replays the answers so far and stops at the next unanswered question.

The new parts are:

- an exception;
- a request parameter;
- a request attribute that marks the requests that can ask;
- a catch block in each renderer;
- a flag that stops PrimeFaces saving the conversation;
- a small client wrapper in each renderer.

Replay is done on the client in both renderers. In PrimeFaces this depends on PrimeFaces' client internals, so a spike comes first ([Spike first](#spike-first)).

Code references are to the local Skyve `master` at `7ab462fb6f`, read on 2026-10-19. Line numbers are approximate.

## What it is for

A static `confirm` covers a question with fixed text that is always asked, where Cancel aborts. These need server-side questions:

- **Text built from data**, or a question asked after an earlier dialog.
- **Conditional questions**, where server logic decides whether to ask. For example, an action asks whether to charge a fee only when the record's settings say one applies.
- **Questions whose answer picks a path**, not only continue or cancel. For example, OK runs an extra step, and Cancel skips it while the action still completes.
- **Questions after a server-side check**, asked in the order the logic runs rather than before the action starts.
- **Questions from a field change**, which is not an action. For example, a reference number fails a check-digit test, and the user is asked whether to keep it.

Validation is not a question: an "invalid, re-enter" check becomes a warning beside the field (Mike, 2026-10-09).

The design holds no transaction or lock while the user decides, so a question needs no timeout.

## How Skyve behaves today

- **Static confirm only.** `confirm` and `escapeConfirm` are attributes of `abstractAction` (`view.xsd:1851`). `ViewRenderer.preProcessAction` resolves them with `Util.i18n` only; there are no bindings.
  - SmartClient shows them with `isc.ask` in `BizButton.initWidget` (`editView.js:1636`).
  - PrimeFaces shows them with a `ConfirmBehavior` and the global `<p:confirmDialog>` in each template.
- **No channel for questions.** Nothing in `skyve-core`, `skyve-ext` or `skyve-web` lets server code make the client ask something. `ServerSideActionResult` carries only the bean. `PushMessage` is asynchronous and addressed to a user, not to a request.
- **A failed request leaves no trace on the server, in either renderer.** This is what makes the rerun cheap.
  - **SmartClient.** `SmartClientEditServlet.processRequest` catches `Throwable`, calls `persistence.rollback()` and writes an error response. `StateUtil.commitAndCacheConversation` is reached only from `pumpOutResponse` on success. The next request deserialises the last good conversation, including its Hibernate session and the bean's optimistic-lock version. The client keeps the user's edits and resends the whole bean.
  - **PrimeFaces.** `FacesAction.execute` sets rollback-only on any `Throwable`. `SkyveFacesPhaseListener.afterResponseRendered` saves the conversation only when the highest `FacesMessage` severity is below `ERROR`. The form is not re-rendered, so its inputs are re-posted with `@form` on the next request.
- **Every server-side path goes through one place per renderer.** In SmartClient, custom actions, OK, Save, ZoomOut, Delete and rerenders all run in `SmartClientEditServlet.processRequest`. In PrimeFaces, `SaveAction`, `DeleteAction`, `ExecuteActionAction`, `RerenderAction` and the others all extend `FacesAction`.
- **Field changes.** Skyve has no `onPropertyChanged`. A `<rerender>` on change calls `Bizlet.preRerender` (SmartClient `apply()` with `_s`; PrimeFaces `RerenderAction`). Rerenders share the transaction and rollback behaviour of actions.
- **Reaching the request.** `preSave` and `preDelete` get no `WebContext`. `EXT.getHttpServletRequest()` works anywhere in a request: it is backed by the `WebContainer` thread-local that `RequestLoggingAndStatisticsFilter` sets for `/*`. `SmartClientEditServlet.collectRequestParameters` drops parameters whose names start with `_` from its map, but `request.getParameter` still reads them.

## Requirements

### R1. Server API, in `skyve-ext`

```java
public interface Dialogs {
	String ask(Question question);       // the chosen button's code
	boolean confirm(Question question);  // true for OK or Yes
	boolean canAsk();                    // false outside an interactive request
	// Later phases: choose(...) for custom buttons, input(...) for value answers and popup forms
}

// In EXT, alongside getMailService(), getReporting() and the other services.
public static Dialogs getDialogs();

Question q = Question.okCancel("Payment.reference#checkDigit", "Confirmation", message)
		.defaultButton(Question.Button.OK)
		.cancelAborts();
```

- **R1.1** The API lives in `skyve-ext`, because it reads the request through `EXT.getHttpServletRequest()`. It needs no `WebContext`, so `preSave`, `preDelete`, interceptors and helper code can ask.
  - `EXT.getDialogs()` returns a static singleton, as `EXT.getMailService()` does, with `set(...)` and `setDefault()` (R7.1).
  - The default implementation holds no state. Answers live on the request (R2.2).
  - It is not a `WebContext` method. `WebContext` is in `skyve-core`, which cannot read the request. `preSave`, `preDelete` and their interceptor hooks get no `WebContext`. A `ViewBackgroundTask` holds a `WebContext` with no request behind it.
  - `Dialogs` covers dialogs as a whole, so the later phases add methods to it rather than new top-level classes. A question is one kind of dialog, so `Question` and `QuestionException` keep their names (Mike, 2026-10-19).
  - **Packages and classes** (Mike, 2026-10-19):
    - `org.skyve.dialog` holds `Dialogs`, `Question` and `QuestionException`. This matches `org.skyve.report`, `org.skyve.tag` and `org.skyve.cache` in `skyve-ext`.
    - The button type is the nested enum `Question.Button`, not a top-level `Button`. Core already has `org.skyve.impl.metadata.view.widget.Button`.
    - `org.skyve.impl.dialog` holds `DefaultDialogs` (named like `DefaultTagManager` and `DefaultReporting`) and `DialogsStaticSingleton`, and `DialogsProtocol`, which holds what the renderers, their clients and any `Dialogs` implementation share: the askable attribute with `markAskable()` and `unmarkAskable()`, the `_q` parameter name, the SmartClient question status and the question JSON. The singleton has `get()`, `set(...)` and `setDefault()`, like `MailServiceStaticSingleton`.
- **R1.2** A `Question` has:
  - a **key**, supplied by the caller and stable across reruns;
  - a **title** and a **message**, as plain text that is always escaped;
  - a **preset**, OK/Cancel or Yes/No, with the button codes `OK`, `CANCEL`, `YES` and `NO`;
  - a **default button**. If none is set, the negative button (Cancel or No) is the default;
  - an **aborts** flag on the negative button, set by `cancelAborts()`.
- **R1.3** When the request has no matching answer, `ask` throws `QuestionException`. It extends `SkyveException` and carries the question and its id. It does not implement `MessageException`, so the existing error paths never show it as an error.
- **R1.4** `ask` never returns a button that aborts. Choosing one ends the gesture on the client (R4.3).
- **R1.5** `canAsk()` is true only when the current HTTP request carries the **askable** request attribute (`DialogsProtocol.ASKABLE_ATTRIBUTE_NAME`). Each renderer sets it at the start of the paths in R5 that can ask (R3.2, R3.3). Otherwise `ask` throws `IllegalStateException` naming the question. That covers a job, a background task, no request at all, and a request on a path that cannot ask. A test implementation installed for R7.1 does not have this restriction.
  - A request attribute is used because the request already exists on every path, and the container discards it when the request ends. No thread-local is needed and nothing has to be cleared.
  - The paths that cannot ask share code with the ones that can. SmartClient `fetch` goes through `processRequest`, and PrimeFaces `EditAction` and `PreRenderColdHitAction` extend `FacesAction`. So the attribute is set per path, not per servlet or base class.

### R2. Answers

- **R2.1** A question's **id** is a hash of its key, title, message and button codes. It is computed by `ask` and sent to the client with the question. It is not a secret.
  - It is `BindUtil.identifier(...)`: the SHA-1 of the text, written as 27 letters and digits. That method exists for exactly this case, where a client and the server have to agree on a name for the same thing without sharing state (Mike, 2026-10-19).
  - The text passed to it puts each field's length before the field, as `<length>:<field>`, in the order key, title, message, then each button code. Simply joining the fields would let key `a|b` with title `c` give the same id as key `a` with title `b|c`.
- **R2.2** The client sends answers as one request parameter, `_q`: a JSON array of `{"id": "...", "code": "OK"}`. `ask` parses it once per request, keeps the parsed map as a request attribute, and looks up its own id.
  - It is parsed with `JSON.unmarshall(q, Answer[].class)`, where `Answer` is a private record `(String id, String code)`. `JSONReader` builds records and arrays without a `"class"` property (Mike, 2026-10-19).
  - If `_q` is malformed (bad JSON, not an array, or an entry missing `id` or `code`), it is ignored as a whole. `ask` logs a warning once per request and treats the request as having no answers, so the question is asked again. `_q` comes from the client, so bad input must not become a server error (Mike, 2026-10-19).
- **R2.3** An answer counts only when its id matches and its code is one of the question's buttons that does not abort. Anything else is ignored and the question is asked again.
- **R2.4** A question whose text changes between runs gets a new id, so a stale answer is never applied to a changed question. This happens when data changes between runs, such as another user's edit or the clock. Answers are matched to the question, not only to its position.
- **R2.5** **Limit.** When `_q` already holds 20 answers (a constant, not a setting; Mike, 2026-10-19), `ask` throws an `IllegalStateException` naming the question's key instead of a question. Each renderer reports it as an unexpected error. Without this, a question whose text changes on every run, such as one that includes the time, would be asked forever. The limit is enforced on the server, so it applies to every client.
- **R2.6** The same question, meaning the same key and text, asked twice in one run gets one answer for both. There are no re-prompt loops (Mike, 2026-10-09).
- **R2.7** Answers are not an authorisation control. A user who sends an answer for a question they were not shown gains nothing they could not get by clicking.

### R3. Catching the question

- **R3.1** Each renderer looks for `QuestionException` by walking the cause chain of whatever it catches, so a question wrapped by an interceptor or by Skyve code is still a question.
- **R3.2 SmartClient.** In `SmartClientEditServlet.processRequest`:
  - Set the askable attribute (R1.5) for every operation except `fetch`. `fetch` is form load (New, Edit, ZoomIn). `add`, `update` and `remove` cover OK, Save, ZoomOut, custom actions, rerenders and Delete.
  - Because `fetch` is not askable, a question there fails `ask` with `IllegalStateException` and reaches the generic handler as a `-1` error. The fetch callbacks in `editView.js` act only on `-1`, so a `-50` there would not be shown at all.
  - Before the generic handler, for a `QuestionException`, call `persistence.rollback()`.
  - Do not call `StateUtil.replaceToken`, as on any failure. The token sent in this response's `X-CSRF-TOKEN` header is never registered, so the client must not use it (R4.7).
  - Write HTTP 200 with status `-50` (Mike, 2026-10-19). `data` is a plain-text fallback for clients without question support, such as Flutter.
    - The status is negative, so every existing `status >= 0` check treats it as a failure.
    - It is outside SmartClient's own `RPCResponse.errorCodes`, which use `0`, `1`, `-1`, `-3` to `-13`, `-15`, `-16`, `-90` to `-92`, and `-100`. `-10` is SmartClient's `STATUS_TRANSACTION_FAILED`.
    - In this repository only `editView.js` calls `/smartedit`. A client elsewhere shows `data` until it supports questions.

  ```json
  {"response": {
    "status": -50,
    "data": "This action needs a confirmation that this app cannot show: <message>",
    "question": {
      "id": "...", "title": "...", "message": "...",
      "buttons": [
        {"code": "OK", "label": "OK", "aborts": false, "default": true},
        {"code": "CANCEL", "label": "Cancel", "aborts": true, "default": false}
      ]
    }
  }}
  ```

- **R3.3 PrimeFaces.**
  - Set the askable attribute (R1.5) in the actions that can ask: `SaveAction`, `DeleteAction`, `ZoomOutAction`, `ExecuteActionAction`, `RerenderAction` and `RemoveAction`. The other `FacesAction` subclasses do not set it. These are the load actions (`EditAction`, `AddAction`, `ZoomInAction`, `PreRenderColdHitAction`), the lookups (`CompleteAction`, `GetSelectItemsAction`, `GetBeansAction`) and the content, chart and download actions. A cold hit is a plain page load, not an AJAX request, so it has no request to replay.
  - In `FacesAction.execute`, before the generic handler, for a `QuestionException`:
    - set rollback-only;
    - remove every `FacesMessage` the run added;
    - clear the partial render ids, so nothing is rendered and the client keeps its inputs;
    - set a flag that stops the conversation being saved;
    - run `executeScript("SKYVE.PF.ask(<question JSON>)")`.
- **R3.4** When the flag is set, `SkyveFacesPhaseListener.afterResponseRendered`:
  - does not call `postRender` or its interceptors. `postRender` runs before the save check today, and on this path it would run against rolled-back state and could add growls;
  - does not save the conversation. It cannot rely on message severity, because R3.3 adds no error message;
  - still calls `view.dehydrate()`, as on any failure (R6).
- **R3.5** Button labels come from Skyve's i18n resources, under new keys (Mike, 2026-10-19):
  - `ui.dialog.ok`, `ui.dialog.cancel`, `ui.dialog.yes` and `ui.dialog.no` for the button labels;
  - `ui.dialog.unsupported` for the SmartClient fallback text in `data` (R3.2), with the message as `{0}`.

  The existing `resources.implicitActionName.ok` and `.cancel` are not reused. They label Skyve's implicit actions ("save and close"), and translations made for that meaning may not suit a dialog button. The keys go in all six bundles in `skyve-war/src/main/java/resources/`. `i18n_de` is Latin-1.
- **R3.6 The PrimeFaces global confirm dialog.** The global `<p:confirmDialog>` in the `diamond`, `ecuador`, `editorial`, `external` and `ultima` templates has its buttons written as `value="Cancel"` and `value="OK"`. Change them to `#{bean.i18n['ui.dialog.cancel']}` and `#{bean.i18n['ui.dialog.ok']}`, through `LocalisableView.getI18n()`. This is in v1, so that a static `confirm` and a question show the same translated labels (Mike, 2026-10-19).

### R4. Client behaviour

- **R4.1** The client shows the question in a modal dialog: the title, the message and the buttons in order, with the default button focused. Escape and the close box choose the button that aborts. If no button aborts, the user must choose one.
- **R4.2** Choosing a button that does not abort adds `{id, code}` to the answers for this gesture and resends the original request with all of them in `_q`.
- **R4.3** Choosing a button that aborts ends the gesture. Nothing is sent. The form keeps what the user entered, and the server conversation is as it was before the gesture.
- **R4.4** Answers last for one gesture: a click or one field change, plus its reruns. The client discards them when the gesture completes, fails or is aborted.
- **R4.5** A resend repeats the original request exactly: the same action, source, validate flag and form values. A static `confirm` on the action is shown only before the first request, not on resends.
- **R4.6 Field changes.** When a field-change question is aborted, the field keeps the new value and the rest of the change logic does not run. The value reaches the server bean with the next request, as any edit does.
- **R4.7 SmartClient.** `editView.js` has three mutating calls, each with its own inline callback:
  - `saveInstance` covers OK, Save, ZoomOut and rerender (`rerenderAction` calls it), through `_vm.saveData`;
  - `deleteInstance` uses `_DATA_SOURCE.removeData`;
  - `doAction` covers custom actions and `_doBlurryAction`, through `_vm.saveData`.

  One shared helper handles status -50 in all three callbacks, before the existing `status === -1` check. It shows the dialog. On a button that does not abort, it reissues the same DataSource call with the same callback and a copy of the `params` the first call built, with `_q` added. On a button that aborts, it calls the method's `completedCallback` with `success` false, as a failure does today.
  - Replaying means resending the captured `params`, not calling the method again. `saveInstance` clears `this._source` before it sends, so a second call would lose `_s`. A second call would also run `gather` again and show the static `confirm` again (R4.5).
  - The resend keeps the `_csrf` the first request sent. The callbacks take a new token from `x-csrf-token` only when `status >= 0`, and R3.2 does not register the token sent with a question. This already happens, provided the -50 branch does not update `this._csrf`.
  - The question reaches the callback as `dsResponse.question` if `isc.RestDataSource` passes extra response properties through. The spike found that it does not, so the edit data source copies it in `transformResponse` (see [Open decisions](#open-decisions)).
  - Rebuild `skyve-min.js`.
- **R4.8 PrimeFaces.** `pf.js` remembers the configuration of the most recent PrimeFaces AJAX request for the form. Command buttons and `p:ajax` change behaviours both go through PrimeFaces' AJAX request handling. `SKYVE.PF.ask` shows the dialog and re-issues that request with `_q` added as a request parameter.
  - Save, Delete, ZoomOut, custom actions and rerenders are covered without recording anything on the server.
  - The resend's screen updates are those of the original request.

### R5. Where questions work

| Request | v1 |
| --- | --- |
| Custom `ServerSideAction.execute` | Yes |
| OK and Save, including `preExecute`, `preSave`, `postSave` and interceptors | Yes |
| ZoomOut, including `preExecute` | Yes |
| Delete from the form, including `preExecute` and `preDelete` | Yes |
| Rerender, including `preRerender` and interceptors | Yes |
| A `<server action>` in an `onChanged` or `onBlur` handler | Yes |
| Remove | PrimeFaces only. SmartClient Remove runs on the client only |
| Form load (New, Edit, Add, ZoomIn), list grid operations, downloads, uploads, REST | No. A question shows as a failure |
| Rendering the response, such as getters, conditions and the title | No. The actions are done, so the request is unmarked as askable before rendering and `ask` throws `IllegalStateException`. In SmartClient the conversation is committed after rendering, so a rerun would apply the changes twice |
| `postRender`, including its interceptors | No. It runs after the response is sent, so the request is unmarked as askable first and `ask` throws `IllegalStateException` |
| Jobs, background tasks, push | No. `canAsk()` is false |

### R6. Documented limits

These are documented on `Dialogs.ask` rather than enforced in code:

- **Don't ask after a commit in the same request.** That includes `webContext.cacheConversationAndCycleTransaction()`, `background()` and an explicit `persistence.commit`. The committed work is not rolled back and runs again on the rerun.
- **Don't ask after a side effect outside the transaction.** Each rerun repeats it. That includes content and attachment writes (`closeContent` runs even on rollback), `EXT.push`, email, jobs, external HTTP calls and static state.
- **PrimeFaces:** don't ask after changing the conversation's current bean. `dehydrate()` records the new bean's web id even on failure, and the next request cannot find it in the old conversation.
- **PrimeFaces:** `executeScript` calls queued before the question still run.
- **SmartClient:** a click queued behind a blur-triggered rerender (`_blurry`) is dropped when that rerender asks. The user clicks again after answering.

### R7. Testing support

- **R7.1** Tests supply answers by key, outside a request, by installing a test implementation, for example `DialogsStaticSingleton.set(new AnsweringDialogs(Map.of("key", "OK")))`, and restoring it with `setDefault()` afterwards.
  - A question with no answer in the map throws `QuestionException`, as in a request.
  - The singleton is shared across the JVM. `skyve-war` runs tests in separate forks (`forkCount` 2), not in parallel threads. A module that adds in-JVM parallel tests needs a per-thread implementation instead.
- **R7.2** `QuestionException.getQuestion()` exposes the key, the resolved text and the buttons, so a test can check what was asked.
- **R7.3 Unit tests**, in the module that owns the code:
  - the question id: stable for the same key, text and buttons; changed by any one of them; and the length prefix keeps fields apart (R2.1);
  - `_q` parsing and matching: wrong id, a code that aborts, an unknown code, malformed `_q` ignored as a whole, and the 20-answer limit;
  - `canAsk()` with and without the askable attribute, and with no request;
  - finding a `QuestionException` through the cause chain (R3.1);
  - the SmartClient `-50` response's JSON shape and its escaping.
- **R7.4 Kitchen sink fixture.** Add a `DialogsFixture` document to the `kitchensink` module in `skyve-war`, next to `ConditionsFixture` (Mike, 2026-10-19). Its Bizlet, actions and view ask from each path in R5:
  - a custom action that asks once;
  - a custom action that asks twice, where the second question depends on the first answer;
  - a custom action whose question text includes a counter that changes on every run, for the 20-answer limit;
  - `preSave`, `preDelete` and a `preRerender` from a field change;
  - an OK/Cancel question with `cancelAborts()`, and a Yes/No question whose No returns `false` and carries on.

  The fixture also serves the manual browser checks.
- **R7.5 Integration tests** against the fixture on H2, following `ConditionsFixtureH2Test` and `SmartClientEditServletH2Test`:
  - **SmartClient**, through `SmartClientEditServlet.doPost`. Each path in R7.4 returns `-50` with the expected question. The rerun with `_q` carries on. After an abort nothing is written and the cached conversation is unchanged. A save, then a question, then OK writes the record once, with one optimistic-lock increment. A stale answer is asked again. The limit fails as an unexpected error. A question during `fetch` returns `-1`. The CSRF token is not replaced on a question.
  - **PrimeFaces**, following `FacesActionsTest`. The actions in R3.3 roll back, clear their messages, set the flag and queue `SKYVE.PF.ask`. The phase listener skips `postRender` and the save when the flag is set. An action outside R3.3 that asks reports an error.
  - **Background job.** `ask` throws `IllegalStateException` and `canAsk()` is false.

  The browser-only acceptance criteria are checked by hand in both renderers, on the fixture. These are the dialog, default focus, client replay, keeping the user's edits and which growls are shown. A SAIL step for questions (Later phases) would automate them.

## Spike first

The spike settles the PrimeFaces client replay (R4.8) on one custom action and one `p:ajax` rerender. Confirm that the re-issued request:

- posts the same `@form` values, with a valid view state;
- processes and updates the same components as the original;
- carries `_q` as a request parameter that `request.getParameter` reads;
- does not trigger the button's static `ConfirmBehavior` again;
- uses PrimeFaces API that is stable across the PrimeFaces version Skyve ships.

If it fails, the fallback is to replay on the server: `FacesView` records the method and arguments it was invoking (`action`, `save`, `ok`, `delete`, `zoomout` or `rerender`), and a single `#{skyve.answer}` remote command replays it with `_q`. That needs a replay entry for each `FacesView` method.

The same spike also checks one SmartClient point (R4.7): does a `-50` response's `question` property reach the `saveData` and `removeData` callbacks on the edit `isc.RestDataSource`? If it doesn't, use one of these instead:
- override `transformResponse` on `isc.EditView._DATA_SOURCE` to copy `question` from the raw response onto `dsResponse`;
- carry the question in `errors`.

`data` stays the plain-text fallback either way.

## Acceptance criteria

- [ ] In both renderers, `ask` from each of these shows the question, and the rerun continues past it with the answer:
  - a custom action;
  - Save;
  - Delete;
  - ZoomOut;
  - `preSave`;
  - `preDelete`;
  - a rerender.
- [ ] After an aborting Cancel:
  - nothing is written to the database;
  - the server conversation equals its state before the gesture;
  - the screen keeps the user's edits.
- [ ] A run that saves, then asks, then is answered OK writes the record once, with one optimistic-lock increment.
- [ ] Two questions in one action are asked in order, and both answers reach the code on the final run.
- [ ] A question whose text changes between runs is asked again, not answered from the stale answer.
- [ ] A question whose text changes on every run stops with an error after 20 answers.
- [ ] Growls and messages from runs that ended in a question are not shown; those from the completing run are.
- [ ] A `QuestionException` wrapped in another exception is still treated as a question.
- [ ] With no default button set, focus is on Cancel or No.
- [ ] `ask` in a job throws `IllegalStateException`, and `canAsk()` is false there.
- [ ] A client without question support shows the fallback text.
- [ ] The PrimeFaces global confirm dialog and the question dialogs show translated button labels in every bundle's language.

## Open decisions

| Decision | Options | Recommendation |
| --- | --- | --- |
| PrimeFaces dialog | A dialog component in each template, filled by script, or a dialog built in script | **Decided in the spike (2026-10-19): a dialog built in script** by `SKYVE.PF.ask`, styled with PrimeFaces' dialog classes, with no template change. It is modal (a mask), sets its text as text, focuses the default button, keeps the focus inside, and only a question with an aborting button can be closed by Escape or the close box. The replay wraps `PrimeFaces.ajax.Request.send` and re-issues a copy of the recorded configuration through `PrimeFaces.ajax.Request.handle`. See `docs/skyve-server-side-confirmation-notes.md` |
| SmartClient question property | `dsResponse.question`, a `transformResponse` override, or `errors` | **Decided in the spike:** `isc.RestDataSource.transformResponse` copies only the standard properties, so `isc.EditView._DATA_SOURCE` overrides it to copy `question` onto the `DSResponse` |

## Later phases

Deferred from v1 on purpose. None needs a change to the `_q` protocol.

- **Custom coded buttons.**
- **A details area** that can be collapsed, instead of appending details to the message.
- **i18n keys with parameters** in the title and message.
- **Value answers bound to the bean.** A simple input becomes a typed transient attribute, a popup form a transient document, and a grid input a collection. The answer is data, so it belongs on the bean rather than in `_q`.
- **Guards for the R6 limits:**
  - `ask` refuses after a commit in the request;
  - PrimeFaces `dehydrate()` records the web id the request started with;
  - queued `executeScript` calls are discarded.
- **Questions on form load** and from list grids.
- **Questions from background jobs** through `EXT.push`. A job, unlike a request, can wait for an answer.
- **A SAIL step** that answers a question.

## Alternatives considered

| Alternative | Why not |
| --- | --- |
| **A thread-local for answers** | The request parameter needs no new state. `EXT.getHttpServletRequest()` is already request-scoped and cleaned up by the filter, and no request means `canAsk()` is false. The test hook (R7.1) replaces the singleton instead, so the design has no thread-local of its own |
| **`ask` as a `WebContext` method** | `WebContext` is in `skyve-core`, which has no servlet dependency, so the question classes would move to core and the implementation would go in `skyve-web`. `preSave`, `preDelete` and their interceptor hooks get no `WebContext`, and nothing returns the current one statically. A `ViewBackgroundTask` holds a `WebContext` with no request, so having one does not mean the user can be asked. A new method also breaks other implementations of the interface |
| **Pre-flight check**: a `getQuestion(bean)` hook called before `execute` | It is simpler inside Skyve, with no rollback and no replay. But it needs a hook per action type, Bizlet implicit action and rerender, and costs two requests per click. Data can change between the check and `execute`. It handles one question at a time, so a question that depends on an earlier answer or follows a write does not fit. Code would have to be split at each of its questions, and an action that asks several questions would need a round trip and a check for each |
| **Push and wait**: send the question by `EXT.push` and block for the answer | Holds the thread, connection, transaction and locks while a person decides. Locks taken by database triggers or by the request's own queries make that worse. Proxy timeouts would kill the request. The answer needs a new endpoint routed back to the waiting thread, across nodes in a cluster. The PrimeFaces AJAX queue would deadlock behind the waiting request. Push is addressed to a user, not a tab, and is lost when the websocket is down |
| **Push and continue**: the answer starts a second action | Splits the logic into callbacks, so the code no longer reads top to bottom. Work before the question is already committed, so an aborting Cancel would need code to undo it. The form stays live between the question and the answer |
| **Answers as transient bean attributes** | Needs one attribute per question per document, including for questions asked from shared code. Each attribute must also be in the view to be posted. An answer not cleared after its gesture confirms the next one silently, and the check that the question's text has not changed is lost. Kept for value answers in a later phase, where the answer is data |
| **Static `confirm` with bindings and a condition** | Needs no server round trip, but only works when the question is known before the action runs. It cannot handle a conditional question, a question after a server-side check, or a field change. Worth doing separately for questions whose text comes from data |
| **Server-side replay in PrimeFaces** | Owned by Skyve rather than PrimeFaces internals, but needs a replay entry for each `FacesView` method. Kept as the fallback if the spike fails |
