# Plan: the cockpit

## Summary

The cockpit is one panel, on every signed-in page, that brings together the things that come to a user rather than the
things a user goes to find. It has four tabs:

| Tab | What it holds |
|---|---|
| **Assistant** | AI chat that can act on the view the user has open |
| **Messages** | Direct messages, channels and record threads (the discussion about one record) |
| **Inbox** | Items waiting for the user to act on, such as flow activities |
| **Activity** | Notifications and reminders |

A button next to the avatar opens the panel, and its badge shows how many unread items there are across the tabs. SSE
push keeps the badge current.

The panel is written once, in Vue, and mounted in both SmartClient and PrimeFaces. skyve-vue first becomes an ES-module
build: each page loads a small entry module for what it shows, and heavier parts (the list grid, later the Forms and
Flows designers) load as separate modules only where they are used, all sharing one Vue runtime. Messages are written to
the database and moved by the existing archive pipeline into flat files indexed by Lucene, so the volume can grow
without limit on one server.

There are five phases. C0 makes skyve-vue an ES-module build; the list grid stays on PrimeVue 3 with the trim already
done, and the cockpit is plain Vue. Messaging, notifications and the inbox do not depend on the agent work in
[plan_agent_mcp.md](plan_agent_mcp.md), so C0 to C3 can start now. Only the Assistant (C4) waits for it.

The work branches off `skyve12`: ee11 (the Jakarta EE 11 upgrade) plus the list grid trim (decision 35, commit
`9ea2b1f516`).

| Phase | Deliverable | Depends on | Size |
|---|---|---|---|
| C0 | skyve-vue as ES modules: entry modules, lazy chunks, one shared Vue runtime; the list grid moved onto it; gzip for script and style | Nothing | Medium |
| C1 | The panel, the badge, the Activity tab: notifications, reminders, `EXT` API, push and count refresh | C0 | Medium |
| C2 | Messages: direct messages, channels, record threads, mentions, unread tracking, archiving | C1 | Large |
| C3 | Inbox: the provider interface and the panel tab; the Skyve Flows provider when Flows is on Skyve 10 | C1; Flows for its provider | Small |
| C4 | Assistant: chat on the user's open view, guidance, save as a task | C1, agent phases 0 to 2 (3 for tasks) | Large |

C0 and C1 are specified closely enough to start. C2 is designed here and gets its own detailed plan when it is
scheduled. C3 and C4 are outlined.

## Scope

### Direction from Mike (discussion, 26 November 2026)

1. **The cockpit's parts**: a badge on the avatar updated by SSE; an agent chat like the one in VS Code; in-app contextual
   chat with mentions (`@this` for the open edit view, `@<user>` to notify a person); the workflow inbox.
2. **Messages will be enormous in number.** Testing of the audit archive showed one commodity server can hold large
   amounts of data with Lucene and the archive files.
3. **The badge placeholders exist** in the PrimeFaces template and in the SmartClient header.

### Decisions confirmed by Mike (26 November 2026)

1. **The panel slides out on the right and docks**, pushing the page aside on wide screens. It is part of the page frame,
   not the view, and is separate from the view `<sidebar>`.
2. **Channels are their own thing.** No `@group` mentions, and no use of Skyve security groups as teams.
3. **Anyone who can read a record can read its record thread**, checked on every read.
4. **Multi-node push is left for now.** Push stays in memory on one server.
5. **The database takes the writes and holds recent messages, and the archive pipeline holds older ones.** Messages are
   not written straight to Lucene: posting needs one transaction across the message, its mentions, notifications and
   unread counters, and messages change for a while after they are sent.
6. **Messages can be edited for 15 minutes and are fixed after that.** A correction after the window is a new message.
7. **Messages are kept forever.**
8. **Deleting a message after the window hides it** ("message deleted") and keeps the content. Within the window a
   delete removes the row, because it has not been archived yet.
9. **Erasure (destroying the content everywhere) is deferred.** See [Deferred](#deferred).
10. **When a record is deleted its thread is kept**, marked as belonging to a deleted record, as audits outlive their
    records.

### Decisions confirmed by Mike (26 November 2026, second round)

11. **The panel is a Vue component in skyve-vue**, mounted by both renderers. (Revised by decision 33: skyve-vue becomes
    an ES-module build, and the panel is its own entry module, not part of the list grid's bundle.)
12. **The cockpit is off by default, switched on like text search**: by a `<cockpitRoles>` list of feature roles in the
    customer XML, next to `<textSearchRoles>`, `<flagRoles>` and `<switchModeRoles>`, with `User.canUseCockpit()`
    alongside `canTextSearch()`. A customer with no cockpit roles has no panel, badge, push type or working endpoint,
    and behaves exactly as before. The admin tables are still created, as Communication's are. Installation-wide
    settings (such as the notification retention) stay in the JSON configuration.
13. **The document names are `MessageThread` and `MessageThreadMember`**, not `Thread` (which would shadow
    `java.lang.Thread` in generated code) and not `Conversation` (Skyve's name for an edit view's server state).
14. **Push carries no content.** A cockpit push item says only what changed (activity, messages and which thread, or
    inbox). The client then fetches counts or new messages over HTTP. A dropped push costs freshness, never
    correctness, and content never sits in the push queue.
15. **Pushes are sent after the transaction commits.** Add a post-commit callback to the `Persistence` interface (a
    default method), implemented where `commit` and `rollback` are, in skyve-ext's `AbstractHibernatePersistence`, and
    delegated by `PersistenceInjectable`. It runs after a successful commit and is dropped on rollback. Without it, a user can fetch counts before the row is visible and the
    badge lags until the next event.
16. **The archive gains a per-document rule for when a row may be exported**, as a default method on the existing
    `DocumentConverter` interface. Audit and mail logs keep exporting every row at the next run.
17. **Unread messages are emailed as a catch-up digest**, as Slack sends, to people who are not online. One email
    covers everything unread since the last one. See [Email digest](#email-digest).
18. **CSRF for the cockpit's endpoints is left for now.** See [Open decisions](#open-decisions).
19. **Backing up archive files is deferred.** See [Deferred](#deferred).

### Decisions confirmed by Mike (26 November 2026, third round)

20. **The documents live in the admin module**, next to Communication and Audit, with read-only interfaces in
    `org.skyve.domain.app.admin` in skyve-core, as `MailLog` has, so that skyve-ext can use them.
21. **The cockpit has its own button in the top bar, just before the avatar, and the badge moves onto it.** One click
    opens or closes the panel, and the button shows as pressed while the panel is open. The avatar opens the user menu
    as now, with no badge. The existing placeholders (the commented-out badge in `header.js` and the hidden
    `<p:badge>` in the editorial template) move to the new button. Reasons: the panel is opened many times a day, so it
    should be one click, not two; a badge on the avatar promises something that the user menu does not show; and a
    docked panel needs a visible toggle. A keyboard shortcut opens it too.
22. **The client fetches counts whenever the SSE stream opens or reopens**, because a reconnect does not replay missed
    pushes.
23. **The user directory for `@` mentions is the poster's customer**, and the poster's data group when they have one,
    limited to users who can use the cockpit. It does not need permission on `admin.User`, which most users do not
    have.
24. **Read notifications are purged after 30 days**, configurable. They are not archived: the message or record they
    point at is the lasting record.
25. **Fired reminders become notifications** in the Activity tab. The Inbox holds only provider items.
26. **Channels are private in C2**: the owner adds and removes members. Open channels that anyone in the customer can
    join can follow.
27. **Message text is plain text with a small markdown subset** (emphasis, code, lists, links). The server stores what
    was typed. The client escapes it and renders the subset with a sanitising renderer. Links are `http`, `https` or a
    Skyve record link only. Messages are limited to 8,000 characters.
28. **A deleted record is detected when its thread is read**, not through an interceptor on every delete: when the
    record cannot be found, the thread is shown as belonging to a deleted record, readable by its members and by
    message auditors (decision 29).
29. **Message content is readable only by a separate auditor role, and every read is logged.** Administrators can list
    channels and their members but not read messages. A new admin role, `MessageAuditor`, not part of any existing
    role or group by default, can search and read messages across the customer. Every search and every thread opened
    is logged. See [Auditing messages](#auditing-messages).
30. **Which messages notify a person, and so reach their digest**: for a direct message, every other member; for a
    channel or record thread, the people mentioned and members who have chosen "notify me of every message" for that
    thread. Muted threads notify nobody. A user can turn the digest off for themselves in the panel. Without this, a
    busy channel would fill every member's Activity tab and digest.

### Decisions confirmed by Mike (26 November 2026, fourth round)

31. **The cockpit's endpoint is a servlet declared in `web.xml`, not JAX-RS.** Every other Skyve service is a servlet
    that an installation can remove by commenting it out of `web.xml`. A JAX-RS application registered by
    `@ApplicationPath` is always deployed and can only be blocked by a filter such as `ForbiddenFilter`, which leaves
    the endpoint live behind one filter mapping. See [Server endpoints](#server-endpoints).
32. **The cockpit service is not injected.** It is reached through `EXT.getCockpit()`, backed by a
    `CockpitStaticSingleton` set in `SkyveContextListener`, as Skyve's mail, SMS and dialogs services are. See
    [Notifications and reminders](#notifications-and-reminders-c1).

### Decisions confirmed by Mike (26 November 2026, fifth round)

33. **skyve-vue becomes an ES-module build**, not one IIFE bundle. Each page loads a small entry module for what it
    needs, larger parts load as separate modules on first use, and they all share one Vue runtime. The list grid moves
    onto it too (it stays PrimeFaces-only and opt-in). See [The skyve-vue build](#the-skyve-vue-build-c0).
34. **A rewrite of Skyve Forms and Skyve Flows is coming, with drag and drop and fast data entry.** Their designers are
    full-page views, not the side panel. They will be modules of the same skyve-vue build, loaded only where they are
    used. Their design is not part of this plan; C0 only makes room for them.

### Decisions confirmed by Mike (26 November 2026, sixth round)

35. **Trim the list grid's download**, as measured in [The skyve-vue build](#the-skyve-vue-build-c0): drop PrimeFlex
    (318 KB of its 549 KB stylesheet), drop the duplicate primeicons stylesheet, and replace luxon (66 KB of script, for
    one parse and format) with a small formatter. **Done** on `skyve12` in `9ea2b1f516` (26 November 2026), ahead of
    C0; still to be checked against the list grid's SAIL tests on the editorial template, which C0's exit covers.
36. **The cockpit uses PrimeVue in unstyled mode**: PrimeVue's behaviour (popups, the mention list, keyboard handling,
    accessibility) without its theme CSS. *Replaced by decision 43:* the cockpit uses no PrimeVue.
37. **Skyve compresses its responses with gzip**, in C0. Undertow's `compress` handler (gzip and deflate) is switched on
    in skyve-war's `WEB-INF/undertow-handlers.conf`, the file that already sets the SameSite cookie attribute, so it
    ships inside the WAR, needs no WildFly change and can be commented out. `MavenSkyveProject` already copies that file
    into Skyve projects, so applications get it when they take the release. See [Gzip](#gzip).

### Decisions confirmed by Mike (26 November 2026, seventh round)

38. *Reversed by decision 42.* **skyve-vue moves to PrimeVue 4 before the cockpit work**, as its own phase (V4) before C0. The cockpit's components
    are then written once, on the version they keep; PrimeVue 4 loads each component's styles only where it is used,
    which C0's per-entry styles are designed around; and the Forms and Flows rewrite starts on the current line. Kept
    apart from C0 so that a list grid failure points at either the component upgrade or the module loading, not both.
    See [Appendix: PrimeVue 4 and Tailwind](#appendix-primevue-4-and-tailwind-deferred).
39. **The cockpit work branches off `skyve12`**, which is ee11 plus the list grid trim. ee11 stays as it is for testing
    the Jakarta EE 11 upgrade.

### Decisions confirmed by Mike (26 November 2026, eighth round)

40. *Reversed by decision 42.* **skyve-vue is styled with Tailwind over unstyled PrimeVue 4**, in a phase of its own (VT) straight after V4 and
    before the cockpit, so the list grid, the cockpit and later Forms and Flows share one way of styling. V4 keeps Lara
    (as PrimeVue 4's Lara preset), so the upgrade is checked for behaviour alone and the restyle for looks alone.
    Tailwind's theme values are CSS variables set from the template's variables, so the components follow the page's
    theme and light or dark mode live. See [Appendix: PrimeVue 4 and Tailwind](#appendix-primevue-4-and-tailwind-deferred).
41. **The look to match is the editorial template's only.** The cockpit follows editorial's variables; on SmartClient's
    home page it uses editorial's default (light) values, as fallbacks. (Decision 45 leaves the other PrimeFaces
    templates without a cockpit.)

### Decisions confirmed by Mike (26 November 2026, ninth round)

42. **The list grid stays on PrimeVue 3.52 with the trim** (decision 35); no PrimeVue 4 and no Tailwind for now
    (reverses 38 and 40). Measured, PrimeVue 3 with the trim is the smallest option (913 KB, 198 KB gzipped, against
    203 to 225 KB gzipped for every PrimeVue 4 build); the download savings come from gzip and C0's entry splitting.
    The styling stays as it is (Lara with editorial's `vue.css` and `main.css` overrides). The PrimeVue 4 and Tailwind
    design, its steps and the measurements are kept in the
    [appendix](#appendix-primevue-4-and-tailwind-deferred) for when PrimeVue 3 has to be left.
43. **The cockpit is plain Vue, with no PrimeVue**, and one small stylesheet of its own. Its parts are simple (tabs,
    lists, a text area, the mention popup, a date and time input for reminders), so it adds only its own code to the
    Vue runtime it shares with the grid, and has nothing to migrate when the grid leaves PrimeVue 3. Its keyboard
    handling and accessibility (focus, roles, the popup) are written in the cockpit.
44. **The cockpit is deliberately neutral: its own design, editorial's colours and font.** Its layout, spacing, type
    sizes, corner radius, shadows and icons are its own and the same everywhere, so it reads as one tool on top of
    either renderer and looks like neither SmartClient nor PrimeFaces. Its colours and its font come from editorial's
    `--skyve-editorial-*` variables, so on the editorial template it follows the theme (blue, emerald, indigo, custom)
    and dark mode with no extra rules. On SmartClient's home page those variables do not exist and it uses editorial's
    default light values, as fallbacks written into each `var()` (decision 41). The
    font is Montserrat, which skyve-web already hosts for editorial; see [The panel](#the-panel).

### Decisions confirmed by Mike (26 November 2026, tenth round)

45. **PrimeFaces development is for the editorial template only.** The cockpit is built into editorial (and
    SmartClient); ecuador, diamond and ultima get no cockpit button or panel and behave as before. Development, SAIL
    gates and checks run on editorial, which is already the default PrimeFaces template for the `tablet` and `phone`
    UX/UIs (`router/UxUis.java`), so SAIL's PrimeFaces runs use it. C0 still swaps the Vue list grid's script tags in
    all four `view.xhtml` files, because it removes the files the old tags load; only editorial is tested in the
    browser, and `ThemeFaceletsStructureTest` covers the other three.

### Open decisions

- **CSRF for the cockpit's endpoints (left for now, Mike, 26 November 2026).** Today:
  - PrimeFaces checks CSRF. `CSRFForm` puts a hidden `csrfToken` in each form, and `FacesView.setCsrfToken` compares
    it and issues a new one on every postback.
  - SmartClient's servlets (`SmartClientSnapServlet`, `SmartClientListServlet`, the edit and tag servlets) send a new
    `X-CSRF-TOKEN` on every mutating request, but `SmartClientListServlet.checkCsrfToken`'s body is commented out
    ("async problems in SC"), so SmartClient checks nothing.

  The cockpit's servlet is not a Faces postback, so neither covers it as it is. Chat makes quick concurrent posts
  likely, and a token that rotates on each post rejects the second of two, which is the problem SmartClient hit. The
  choice is between a per-session token for the cockpit, sent as a header and checked on every `POST`, and a rotating
  one with posts queued client-side. It must be settled before C2's posting, and before the endpoints are used outside
  development.

### Out of scope

- Push across several servers.
- Erasure, presence ("online"), typing indicators, reactions, replies within a thread, attachments on messages,
  `@group` mentions.
- A language model inside Skyve. As in the agent plan, the model is an external service.

## Affected modules

| Module | Changes |
|---|---|
| `skyve-core` | Domain interfaces for the new admin documents; `<cockpitRoles>` in `CustomerMetaData` and `CustomerImpl`, `User.canUseCockpit()` and both copies of `customer.xsd` (`skyve-core/src/test/resources/schemas`, `skyve-war/src/main/java/schemas`); the installation-wide cockpit settings in `UtilImpl`; the post-commit callback as a default method on the `Persistence` interface; `canUseCockpit()` in `UserImpl` and `SuperUser` |
| `skyve-ext` | The post-commit callback in `AbstractHibernatePersistence` and `PersistenceInjectable`; `canUseCockpit()` in `UserInjectable`; the `Cockpit` interface, its implementation and `CockpitStaticSingleton` (notifications, reminders, counts; messages in C2), reached through `EXT.getCockpit()`; the cockpit push item on `PushMessage`; the export rule on `DocumentConverter` and its use in `ExportDocumentsToArchiveJob`; the message converter and the merged read; `InboxProvider` (C3) |
| `skyve-web` | `CockpitServlet` at `/cockpit/*` (declared in skyve-war's `web.xml`); an open hook on `sse/connect.xhtml`; the `SKYVE.listgrid`/`SKYVE.cockpit` call queue in `common/util.js` (C0) and the cockpit item in `desktop/util.js` and `prime/pf.js`; the SmartClient docked panel and badge in `header.js`; the renderers' cockpit flag in `DesktopView` and `HarnessView`; the PrimeFaces `<cockpit:panel/>` composite component; the rebuilt `skyve-min.js` bundles |
| `skyve-vue` | The ES-module build (C0): entry modules, lazy chunks, the list grid moved onto it; `build.xml` copying the whole output; the cockpit entry module and `window.SKYVE.cockpit` (C1) |
| `skyve-war` | Admin documents, generated domain and the reminder job; `CockpitServlet`'s declaration in `web.xml` and its matcher in `SpringSecurityConfig`; the editorial template (and the Vue list grid's tags in all four `view.xhtml`) and `desktop/home.xhtml`; i18n keys; the sample `skyve.json`; H2 and SAIL tests |

## Dependencies

- C1 needs nothing new.
- C2 needs the archive enabled for `admin.Message` in the JSON configuration of any installation that wants messages
  archived. Without it, messages simply stay in the database.
- C3's Flows provider needs Skyve Flows on Skyve 10 (agent plan, phase P1).
- C4 needs the browserless conversation engine and the service layer (agent plan, phases 0 to 2), and tasks (phase 3)
  for "save as a task".

## Backward compatibility

- Off by default. A customer with no `<cockpitRoles>`, which is every existing customer, has unchanged pages and push,
  and the endpoints refuse its users. `<cockpitRoles>` is optional in `customer.xsd`, so existing customer files stay
  valid.
- The new push item type is ignored by any client that does not know it: both push handlers skip unknown types.
- The `DocumentConverter` export rule is a default method that adds no restriction, so existing converters, and
  applications' own converters, behave as before.
- New admin tables are created by the normal schema update.
- **Existing applications must re-copy the PrimeFaces templates (C0).** `MavenSkyveProject` copies `WEB-INF/pages`
  into Skyve projects, so an application's own `view.xhtml` files still load `vue/skyve.js` and `vue/skyve.css` as
  Faces resources, and C0 removes those files. Applications that use the Vue list grid re-copy the templates when they
  take the release. This, gzip (decision 37) and the cockpit's switch go in `docs/skyve-12-changes.md`, a new notes
  file in the form of `docs/skyve-10-changes.md`.
- **skyve-vue's built output is committed** (`skyve-web/src/main/resources/META-INF/resources/skyve/vue/`), as the
  `skyve-min.js` bundles are. With hashed chunk names each build replaces files, so a commit of a rebuild includes the
  removed old chunks (`git add -A` on that folder).

## Current state observed

Read on 26 November 2026 at `21317dcb3e`.

### Push

- `PushMessage` (`skyve-ext`) holds items of four types: growl `g`, message `m`, rerender `r` and execute JavaScript
  `j`. `EXT.push` sends one to the receivers of the users it names.
- `SseClientHandler` (`skyve-web`, `/sse/stream`) registers one receiver per open stream in `PushMessage.RECEIVERS`,
  which is in memory. Its queue is bounded and drops the oldest message when full. A reconnect's `Last-Event-ID` keeps
  event ids increasing but does not replay missed messages. There are caps per user (the oldest stream is closed) and in
  total.
- The client is `sse/connect.xhtml`, a composite component that opens an `EventSource` and passes each message to
  `isc.BizUtil.onPushMessage` (`skyve-web/src/js/desktop/util.js`) or `SKYVE.PF.onPushMessage`
  (`skyve-web/src/js/prime/pf.js`). It has no open or error hook.
- Rerender `r` rerenders the current view in every tab the user has open. There is no way to target one conversation.

### The badge

- SmartClient: `skyve-web/src/js/desktop/header.js` has a commented-out badge on the 32-pixel avatar ("uncomment this
  when its time in cockpit"), showing a fixed 4. Clicking the avatar opens the user menu.
- PrimeFaces: three templates have a hidden placeholder, `<p:badge value="4" visible="false">` around the avatar:
  `editorial/template.xhtml` (line 68), `ecuador/topbar.xhtml` (31) and `ultima/topbar.xhtml` (41). `diamond` has none.
  The templates are not uniform: ecuador, ultima and diamond have a `topbar.xhtml`, editorial has its top bar in
  `template.xhtml`. A fifth template, `iframe`, has no page frame and gets no cockpit.
- Every template's `template.xhtml` (and SmartClient's `desktop/home.xhtml`) uses `sse/connect.xhtml`, five places
  in all.

### Vue

- skyve-vue builds an IIFE bundle (`skyve/vue/skyve.js`, `skyve.css`) that exposes `window.SKYVE.listgrid`. The four
  PrimeFaces `view.xhtml` templates load it with `h:outputScript`. SmartClient's `home.xhtml` does not. It holds the list
  grid, the two calendar components, the image component and the snapshot picker, all in one file.
- An IIFE build has one entry and cannot split into chunks; it was chosen so that minified top-level names stay private
  from other page scripts (`vite.config.js`). `build.xml` copies only `skyve.js` and `skyve.css` into skyve-web.
- skyve-vue has no test runner configured.
- Sizes, measured on 26 November 2026 with a source-mapped build of the current source (Vue 3.5.43, PrimeVue 3.52.0).
  The files then committed in skyve-web were a little larger (744 KB and 563 KB), and those are the "before" figures in
  the trim results below:
  - `skyve.js`: 726 KB raw (187 KB gzipped). **The script is tree-shaken**: PrimeVue components are imported one by
    one. The largest parts are DataTable 155 KB, Calendar 69 KB, luxon 66 KB, PrimeVue's SVG icons 42 KB, Vue 63 KB,
    MultiSelect 38 KB, Dropdown 33 KB, PrimeVue utilities 30 KB, `ListGrid.vue` itself 25 KB, InputNumber 24 KB,
    TieredMenu and ContextMenu 20 KB each.
  - `skyve.css`: 549 KB raw (59 KB gzipped). **The stylesheet is not tree-shaken**: it is the whole Lara theme (every
    PrimeVue component, 231 KB once PrimeFlex is gone), the whole of PrimeFlex (318 KB) and primeicons (12 KB).
  - PrimeFlex is used for three classes in the components (`gap-3`, `gap-5`, `justify-content-center`). The `diamond`
    and `ultima` templates load Skyve's own, older PrimeFlex (`skyve/css/primeflex.min.css`, the `p-grid` generation),
    so those pages carry two PrimeFlex versions.
  - The primeicons stylesheet repeats PrimeFaces' own; the build already points its font URLs at PrimeFaces' copy.
  - luxon is used once, in `support/Converters.js`, to parse an ISO date and format it with a converter's pattern.
  - Neither the WildFly configuration nor `web.xml` turns on gzip, so these are served raw.

### Archive

- Configured per installation in the JSON configuration (`archive`): a schedule (by default every 20 minutes, run as
  one customer's user), a batch size, and per document a directory and `retainDeletedDocumentsDays`.
- `ExportDocumentsToArchiveJob` exports **every** row whose `archiveTimestamp` is null. It has no rule for age. It
  writes each row as one JSON line to `content/archive/<directory>/<document>-<yyyy-MM-dd>.archive`, then marks the row
  with `archiveTimestamp` and `archiveFilename`. Marked rows are hard deleted after the retention days.
- `IndexArchivesJob` indexes the files in Lucene, storing each row's file, byte offset and length. `ArchiveRetriever`
  reads a row by seeking to its offset.
- `DocumentConverter` (found through CDI by `handles(module, document)`) builds the Lucene document and has
  `prepareForArchive`.
- `ArchivedDocumentListModel` does no customer or data scope filtering. That is acceptable for admin-only audits, not
  for messages.
- `AuditService` merges archived and database rows, preferring the archived row where both have the same id.

### Other

- Skyve's services are servlets declared in `web.xml`, so an installation can remove one by commenting it out. The two
  exceptions are JAX-RS applications registered by `@ApplicationPath`, which are always deployed: `/rest`
  (`JaxRsActivator`), switched off only by `ForbiddenFilter` mapped in front of it, and `/sse` (`SseApplication`), which
  takes the user from the session with `WebUtil.getValidatedSessionUser`.
- `SmartClientSnapServlet` is a session-level servlet (not tied to one conversation) that takes an action per request,
  calls `persistence.begin()` and then `WebUtil.processUserPrincipalForRequest(...)`, which sets the persistence's
  user, and runs its own `commit`. It sends a new `X-CSRF-TOKEN` for mutating actions; the check
  (`SmartClientListServlet.checkCsrfToken`) is commented out.
- `SpringSecurityConfig` (skyve-war) names every servlet path. After them, any other `GET` is permitted to anyone, any
  other `POST` needs a signed-in user, and every other method is denied. `SpringSecurityConfigAccessTest`,
  `FacesThemeSensitiveEndpointPropagationTest` and `docs/theme-resolution.md` list the servlets too.
- `EXT.push` broadcasts to every connected user when the message names no users.
- `ConverterRegistrationTest` (skyve-core) reads `skyve-vue/src/main.js` for the 12-hour locale (`am: 'AM', pm: 'PM'`)
  and `Converters.js` for the formatters. `ThemeFaceletsStructureTest` (skyve-war) asserts the exact
  `h:outputScript`/`h:outputStylesheet` tags for `vue/skyve.js` and `vue/skyve.css` in all four `view.xhtml` files.
- The archive job is scheduled from configuration in `QuartzJobScheduler.scheduleArchiveJob`, not from a `JobSchedule`
  row.
- Persistence `save` does not check document privileges (those are checked by the UI layer), so code can write admin
  rows for any user in its own transaction. New beans take the current user's `bizUserId` and `bizDataGroupId`.
- Admin jobs are declared in `admin.xml` (`<job name=... className=.../>`).
- Features are switched on per customer by lists of feature roles in the customer XML: `<textSearchRoles>`,
  `<flagRoles>` and `<switchModeRoles>` (for example `customers/demo/demo.xml`), read by `CustomerMetaData` into
  `CustomerImpl` and asked through `User.canTextSearch()`, `canFlag()` and `canSwitchMode()`.
- `org.skyve.domain.app.admin.MailLog` in skyve-core is the precedent for an admin document that skyve-ext uses through
  an interface.
- `admin.Group` is a security bundle of roles with users. It is not used by this plan.

## Design

### The skyve-vue build (C0)

**From one IIFE bundle to ES modules** (decision 33).

- **Entry modules.** Vite builds several entries in one ES-module build:
  - `cockpit.js`: the panel. Loaded on every signed-in page in both renderers, so it stays small.
  - `listgrid.js`: the list grid, as today's `window.SKYVE.listgrid`. Loaded by PrimeFaces views that have a Vue list
    grid, and only when the Vue list grid is turned on.
  - The calendars, the image component and the snapshot picker are used only inside the grid, so they stay in
    `listgrid.js` (lazy where a first render does not need them). Nothing on a PrimeFaces page calls them directly.
  - Later, the Forms and Flows designers and the form filler (decision 34), each loaded only where it is used.
- **Shared chunks.** Vite puts what entries share (Vue, PrimeVue's base, Skyve's support code) into chunks that every
  entry imports, so one Vue runtime serves them all and the browser downloads it once. Inside an entry, rarely used
  parts load with `import()` on first use: the snapshot picker, the column filter calendar, a dialog.
- **Privacy.** Module scope keeps minified names away from SmartClient and PrimeFaces scripts, which is what the IIFE
  was for. Entries still publish their functions on `window.SKYVE` (`SKYVE.listgrid`, `SKYVE.cockpit`), as now.
- **Loading.** A plain `<script type="module" src="skyve/vue/<entry>.js?v=...">`, not `h:outputScript`. A module's
  `import`s resolve relative to its own URL, so the chunks must be served from the plain static path `skyve/vue/`
  (skyve-web's `META-INF/resources`), as SmartClient's `home.xhtml` loads `skyve-min.js`. A Faces resource URL
  (`jakarta.faces.resource/...xhtml?ln=skyve`) would break every chunk import. Module scripts run after parsing, like
  `defer`, so they run after inline scripts: `VueListGridScript` writes an inline `SKYVE.listgrid(...)` call into the
  page, which would run before the entry. So `skyve-web/src/js/common/util.js`, which is in both renderers'
  `skyve-min.js` (loaded as a classic script in the head) and already creates `window.SKYVE`, defines `SKYVE.listgrid`
  and `SKYVE.cockpit` as stubs that queue their calls. Each entry replaces its stub with the real function and replays
  the queue when it runs.
- **Dev page.** `index.html` stays the dev page for `npm run dev`, importing the `listgrid.js` entry; the build's
  inputs are the entry files, not `index.html`.
- **CSS per entry.** Each entry imports its own CSS; shared modules import none, so that each entry's CSS file holds
  all of its styles.
- **Caching.** Vite names chunks by content hash, so a browser keeps them until they change. Entry files keep fixed
  names and the `?v=#{bean.webResourceFileVersion}` version, which also clears the 7-day caching that has caught us
  before. A version change must reach the chunks too: the entry imports its chunks by hashed name, so a new entry
  fetches new chunks.
- **Styles.** Each entry's CSS is its own file (`listgrid.css`, `cockpit.css`), loaded with the entry. The list grid
  keeps PrimeVue's Lara theme in its cascade layer, as `theme.css` does now, and `src/primeflex.css` from the trim.
  The cockpit uses no PrimeVue (decision 43); its stylesheet is built on editorial's variables.
- **`build.xml`** copies the whole output directory into skyve-web's `skyve/vue/`, not two named files, and clears old
  hashed chunks first.

**Trimming the list grid** (decision 35) is done, on `skyve12` in `9ea2b1f516`:

| Change | Measured result |
|---|---|
| PrimeFlex replaced by the rules used: `flex`, `flex-row`, `flex-column`, `gap-2`, `gap-3`, `gap-5`, `justify-content-center` (ListGrid and SnapshotPicker) and `.field` (top-labelled form fields, from `ResponsiveLayoutBuilder`), in `src/primeflex.css` | CSS 563 KB to 237 KB |
| No primeicons stylesheet: PrimeFaces adds its own to every page, with the same codepoints for the five icons used; the dev page gets it from a serve-only Vite plugin | CSS to 225 KB |
| luxon replaced by `src/support/DateFormat.js`, which parses the server's `yyyy-MM-dd`, `HH:mm:ss` and UTC timestamp strings and formats the converters' patterns. It matched luxon in 11.4 million cases across ten time zones, daylight saving gaps and pre-1900 local mean time included | Script 744 KB to 688 KB |

Altogether: `skyve.js` 744 KB to 688 KB (187 to 171 KB gzipped), `skyve.css` 563 KB to 225 KB (61 to 27 KB gzipped).
Splitting the entries (C0) then means a view with only a date field no longer loads DataTable (155 KB) and the rest.
Most of the CSS left is the Lara theme for every PrimeVue component. Moving to PrimeVue 4 would remove it, but its
larger script takes back more than that saves, so the grid stays on PrimeVue 3 (decision 42; measurements in the
[appendix](#appendix-primevue-4-and-tailwind-deferred)).

#### Gzip

Gzip (decision 37) is the biggest single saving, and it helps every page, not only Vue ones: SmartClient's home page
goes from about 7 MB to 1.7 MB, and the list grid from 1.3 MB to about 250 KB before trimming. Nothing is compressed
today.

It goes in skyve-war's `WEB-INF/undertow-handlers.conf`, which Undertow reads from the deployment, as one rule ahead of
the two SameSite rules:

```
path-suffix('.js', '.mjs', '.css', '.svg', '.json', '.map') or path-prefix('/jakarta.faces.resource/') -> compress
```

(One line: the file takes one rule per line. This rule parses with WildFly 42's Undertow 2.4.4, checked 26 November
2026.) The suffixes cover the static files, SmartClient's under `isomorphic130/` included; the prefix covers Faces
resources, whose URLs end in `.xhtml` (`primefaces.js.xhtml?ln=primefaces`).

- **Static script, style and SVG only, by path.** These are the bulk of every first load and the same for every user.
  Pages, Faces partial responses and the JSON from the list and snap servlets are left uncompressed for now: they carry
  session data next to the rotating CSRF token, which is the case the BREACH attack on compressed HTTPS needs. They can
  be added later, with that weighed.
- **Never the SSE stream.** Compression buffers output, which would hold back pushes; `/sse` matches no rule, and C0
  step 8 checks it is sent uncompressed.
- **Images, fonts in WOFF2 and zipped downloads** are already compressed and match no rule.
- **The `NeverExpiresFilter` and `PublicCacheControlFilter` headers are unchanged**; Undertow adds `Content-Encoding`
  and `Vary: Accept-Encoding`, so caches keep both versions apart.
- The exact predicate and the paths that SmartClient and Faces resources are served from are checked against a running
  server in C0 step 8 before the rule is final.

### The panel

- **Placement.** On screens 960 pixels wide and up, the panel docks on the right and the page content narrows to make
  room. Below that, it opens as a full-screen sheet over the page. Its width can be dragged, and the width and whether
  it is open are kept in `localStorage`, so it stays open across navigation.
- **SmartClient.** A canvas on the right of the desktop layout holds the element that Vue mounts into. `header.js`
  gains the cockpit button before the avatar, carrying the live badge (decision 21). `home.xhtml` loads the `cockpit.js`
  entry module and `cockpit.css`, not the list grid.
- **PrimeFaces.** `<cockpit:panel/>` is a composite component, as `<sse:connect/>` is, placed in the editorial template
  only (decision 45). The panel is a fixed element. Docking adds a class to the body that editorial's style sheet uses
  to narrow its content wrapper. Editorial gets the cockpit button and its badge before the avatar. PrimeFaces pages are full page loads, so the panel reopens from
  `localStorage` on each page.
- **Style.** Plain Vue and the cockpit's own stylesheet (decisions 43 and 44). Its rules are scoped to the panel's
  root and its class names prefixed (`skyve-cockpit-`), so that it clashes with neither SmartClient skins nor
  PrimeFaces themes. The stylesheet maps editorial's variables to its own once, at the panel's root, and uses only its
  own names below that:

  | Cockpit variable | From | Default (editorial's) | Used for |
  |---|---|---|---|
  | `--skyve-cockpit-accent` | `--skyve-editorial-accent` | `rgb(59 130 246)` | Badge, active tab, primary button, links, unread dot |
  | `--skyve-cockpit-accent-hover` | `--skyve-editorial-accent-hover` | `#2563eb` | Hover on the above |
  | `--skyve-cockpit-accent-soft` | `--skyve-editorial-accent-soft` | `rgb(59 130 246 / .1)` | Unread rows, selected item |
  | `--skyve-cockpit-focus` | `--skyve-editorial-focus-ring` | `rgb(59 130 246 / .28)` | Focus ring |
  | `--skyve-cockpit-bg` | `--skyve-editorial-panel-bg` | `#fff` | Panel background |
  | `--skyve-cockpit-header-bg` | `--skyve-editorial-panel-header-bg` | `#f3f6fb` | Panel header, tab strip |
  | `--skyve-cockpit-page-bg` | `--skyve-editorial-page-bg` | `#f8fafc` | Composer, quoted record |
  | `--skyve-cockpit-border` | `--skyve-editorial-panel-border` | `#d2d7d9d9` | Borders, dividers |
  | `--skyve-cockpit-text` | `--skyve-editorial-menu-text` | `#444c56` | Text |
  | `--skyve-cockpit-text-muted` | `--skyve-editorial-button-secondary-fg` | `#64748b` | Times, secondary text |
  | `--skyve-cockpit-success`, `-warning`, `-danger`, `-info` (and `-soft`) | The editorial variables of the same names | `#16a34a`, `#d97706`, `#dc2626`, `#0284c7` | Inbox states, reminders due, errors |
  | `--skyve-cockpit-font` | `--skyve-editorial-font` | `"Montserrat", -apple-system, "Segoe UI", Roboto, sans-serif` | All text |

  For example `--skyve-cockpit-accent: var(--skyve-editorial-accent, rgb(59 130 246));`. Spacing, type sizes, radius,
  shadows and the panel's width are fixed values in the stylesheet, not taken from editorial.
- **Font.** Montserrat is hosted in skyve-web (`skyve/editorial/assets/fonts/montserrat/montserrat.css`, a variable
  font split by character set, so a page with only English text loads the 35 KB Latin file). The editorial template
  loads it already; the cockpit's entry loads the same stylesheet on the other templates and SmartClient's home page,
  so the font is the same everywhere and the browser caches one copy.
- **Dark mode.** Follows editorial's (system setting or forced) through the variables. On other templates and in
  SmartClient the cockpit is light, as they give it no dark values.
- **Mock-up.** A look to start from is the mock-up at https://claude.ai/artifact/74SV885JWdYonAu4hi1arW.
- **Strings.** All text comes from Skyve's i18n bundles, passed to the component when it mounts.
- **Context.** Both renderers tell the panel which view is open, through `SKYVE.cockpit.context({module, document,
  bizId, bizKey, webId})` on every render, and clear it on a list or the home page. C1 records it. C2 uses it for `@this`
  and record threads, and C4 for the Assistant.

### Server endpoints

A servlet, `CockpitServlet`, declared in `web.xml` and mapped to `/cockpit/*`, like Skyve's other services
(decision 31). Commenting out its `<servlet>` and `<servlet-mapping>` removes the endpoint entirely. The
`<cockpitRoles>` feature roles stay the normal per-customer switch, and the `web.xml` entry is the hard off switch for
an installation.

It follows `SmartClientSnapServlet`:
- it calls `persistence.begin()` and then `WebUtil.processUserPrincipalForRequest(...)`, which sets the persistence's
  user (`WebUtil.getValidatedSessionUser`, which `/sse` uses, does not), and refuses public users and users without
  `canUseCockpit()`;
- it reads the path after `/cockpit/` and the HTTP method to choose the call;
- it begins and commits its own persistence per request, and runs the post-commit pushes after the commit;
- it answers JSON written with Skyve's `JSON` utility;
- mutating calls are to be CSRF-checked once that is settled (see Open decisions);
- it uses only `GET` and `POST`. `SpringSecurityConfig` gets `.requestMatchers("/cockpit/**").authenticated()` with
  the other servlets, before the catch-all `GET` rule, so no call is open to an anonymous request; any other method
  would be denied by the configuration's final rule.

C1:

| Call | Does |
|---|---|
| `GET /cockpit/counts` | `{activity, messages, inbox}` for the user |
| `GET /cockpit/notifications?before=&limit=` | A page of notifications, newest first |
| `POST /cockpit/notifications/{id}/read`, `POST /cockpit/notifications/read` | Mark one, or all, read |
| `GET /cockpit/reminders` | The user's reminders, due first |
| `POST /cockpit/reminders` | Create or update a reminder (with `id` to update) |
| `POST /cockpit/reminders/{id}/delete` | Delete a reminder |

The paths and methods are the servlet's own routing; no JAX-RS is involved.

The JSON shapes, so that the client and the servlet can be built at the same time:

```
counts:        {"activity": 2, "messages": 0, "inbox": 0}
notification:  {"id", "kind", "title", "text", "link": {"module", "document", "bizId"} | {"thread"} | null,
                "from": {"id", "name"} | null, "created": ISO timestamp, "read": ISO timestamp | null}
notifications: {"items": [notification...], "more": true | false}
reminder:      {"id", "due": ISO timestamp, "text", "about": {"module", "document", "bizId", "bizKey"} | null,
                "fired": ISO timestamp | null}
reminders:     {"items": [reminder...]}
error:         {"error": "message"} with a 4xx or 5xx status
```

C2 adds threads, messages, members and the user directory.

### Push

- `PushMessage.cockpit(what)` adds an item of type `c` with `what` = `activity`, `messages` (with the thread id) or
  `inbox`. Both push handlers pass it to `SKYVE.cockpit.changed(item)`.
- On `activity` or `inbox` the client fetches counts. On `messages` it fetches counts, and the thread's new messages if
  that thread is open.
- `sse/connect.xhtml` gains an optional `onOpen` attribute (empty by default, and only called when set, so the four
  templates and `home.xhtml` that do not pass it stay valid). The client fetches counts each time the stream opens.
- The push handlers call `SKYVE.cockpit.changed(item)`; before the cockpit entry has loaded, the stub from
  `common/util.js` queues it.
- **A cockpit push always names its users.** `EXT.push` broadcasts when a message names none, so the cockpit's push
  helper refuses an empty user list.

### Notifications and reminders (C1)

- **`Notification`**: the user it is for, kind (`mention`, `message`, `inbox`, `reminder`, `system`), title, short
  text, a link (a record as module, document and bizId; or a thread), who or what caused it, created and read times,
  and (from C2) when it went out in an email digest.
- **`Reminder`**: the user, the due time, text, optionally a record it is about, and when it fired.
- **The API**, for Skyve and for applications:

  ```java
  EXT.getCockpit().notify(user).title("Timesheet approved").text("Week ending 22 November").link(timesheet).send();
  EXT.getCockpit().remind(user).at(due).text("Chase the invoice").about(invoice).save();
  ```

  `send()` and `save()` write in the caller's transaction and push after it commits. Persistence `save` does not check
  document privileges, so any caller can write them. A `Notification` or `Reminder` takes the *recipient's*
  `bizUserId` and `bizDataGroupId`, not the caller's (a new bean takes the current user's), so the recipient's
  data-group-scoped queries find it. No role gets privileges on either document: users reach them only through
  `/cockpit`, which reads them by the user's id.
- **How it is reached.** Not by injection. `Cockpit` is an interface in skyve-ext, and its implementation is held by
  `CockpitStaticSingleton`, set at startup in `SkyveContextListener`, as `MailServiceStaticSingleton` is.
  `EXT.getCockpit()` returns it, alongside `EXT.getMailService()`. `CockpitServlet`, the reminder and digest job, and
  application code all use `EXT.getCockpit()`, and tests can set a replacement on the singleton (decision 32).
- **Reminders fire** from a job that Skyve schedules every minute when any customer has cockpit roles, from
  `QuartzJobScheduler` as `scheduleArchiveJob` is, rather than from a `JobSchedule` row. A job runs as one user of one
  customer, so this one loops over the customers that have cockpit roles and works on each as that customer's
  super user. A fired reminder creates a `reminder` notification.
  The same job purges notifications read more than the configured number of days ago.

### Messages (C2)

**Documents.**

| Document | Holds | Store |
|---|---|---|
| `MessageThread` | Kind (`direct`, `channel`, `record`, and in C4 `assistant`); a title for a channel; the owner; for a record thread, the record's module, document and bizId | Database |
| `MessageThreadMember` | The thread, the user, the last read time, an unread counter, muted | Database |
| `Message` | The thread, the author, the text, the mentioned users, an optional record link with its bizKey at the time, created and edited times; `ArchiveableBean` | Database, then archive |
| `MessageDeletion` | The message, who deleted it, when | Database, kept forever |

The user's own cockpit preferences (in C2, only whether they get the email digest) are an embedded association
`cockpit` on `admin.User`, so they are columns of `ADM_SecurityUser` rather than a table of their own. They change
rarely. The panel changes them through `/cockpit`, which updates just those columns, because most users cannot edit
`admin.User` and a full save would run `UserBizlet`. Nothing written on a schedule goes on the user row: that would bump
its `bizVersion` (an administrator editing the user would hit an optimistic lock failure) and add audit rows.

**Posting** runs in one transaction: insert the message; add 1 to the unread counter of every other member
(`update ... set unreadCount = unreadCount + 1 where thread = ? and user <> ?`); create notifications for the people the
message notifies (decision 30). After commit, push `messages` to the members.

**Reading.** The thread pages newest first from the database. When it runs past the oldest row there it carries on from
Lucene by thread and created time. Rows stay in the database for the retention days after export, so the two overlap;
the overlap is removed by bizId, as `AuditService` does. Each page is checked against `MessageDeletion` with one query
over its ids. Reading a thread sets the member's last read time, sets the counter to zero and marks the user's
notifications for that thread read, then pushes `messages` to the user's other tabs.

**Unread counts** are the counters, so the badge never reads the archive.

**Editing and deleting.** The server allows an edit or delete while `now < created + 15 minutes`. A delete in the window
removes the row. After the window, a delete writes a `MessageDeletion` and the message shows as "message deleted".

**Archiving.**

- The export rule for `Message` is `created < now - 20 minutes`: the 15-minute window plus a margin for an edit in
  flight. Every other document keeps exporting everything not yet exported.
- The message converter indexes the bizId, customer, thread, author, created time (for sorting) and the text.
- The index is never read through `ArchivedDocumentListModel`. Every read goes through `EXT.getCockpit()`, which first
  checks that the user may read the thread and then always restricts the Lucene query to that thread and customer.
- One file per day for messages. At about 1 KB a message, 200 users posting 100 messages a day make about 7 GB a year.

**Record threads.**

- Created on the first message about a record. Its members are the people who posted, were mentioned or chose to follow
  it.
- Reading it needs read access to the record, checked by retrieving the record as the user, with document permissions
  and data scope.
- When the record cannot be found, the thread is shown as belonging to a deleted record, readable by its members and by
  message auditors (decisions 28 and 29).
- With an edit view open, the Messages tab shows that record's thread first.

**Mentions.**

- `@` opens the user directory (decision 23), searched by name.
- A mentioned user gets a `mention` notification. In a record thread, a mention makes them a member. If they cannot read
  the record, the poster is warned before the post is sent and the mention is dropped.
- In a channel, mentioning someone who is not a member is refused with a message saying so; the owner can add them.
- `@this` inserts a link to the open record (module, document, bizId and its bizKey). On a record that has not been
  saved, it warns and inserts nothing. Opening the link goes through normal security.

### Auditing messages

Decision 29. This is for answering "what did X say to Y" in a complaint or dispute, without letting ordinary
administrators read people's direct messages.

- **The role.** `admin.MessageAuditor`, declared in `admin.xml`. Skyve does not give it to any existing role or group,
  so a customer has to grant it on purpose.
- **What it can do.** Admin views to search messages across the customer, by author, member, thread, date range and
  text, over the database and the archive (through `EXT.getCockpit()`, restricted to the customer), and to open any
  thread. Auditors see deleted messages too, marked as deleted with who deleted them and when, because the content is
  kept for this purpose (decision 8). Auditors can read only; they cannot post, edit or delete.
- **The log.** A new admin document, `MessageAuditLog`: the auditor, when, what they did (search or open a thread), the
  search criteria or the thread, and how many messages it returned. It is written in the same request as the read, so a
  read that is not logged does not happen. It cannot be edited or deleted through the UI. Users with the security
  administrator role and auditors can list it. It is immutable from the start, so it can be archived as audits are.
- **Data groups.** An auditor sees the whole customer. Data groups do not limit auditing, because a dispute can cross
  them.
- **Not included.** A required reason for each read, and grants limited to one person or thread (option C in the
  discussion). Either can be added later on top of the log.

### Email digest

C2 sends a catch-up digest by email, not one email per message (decision 17).

- **What goes in.** A user's unread `message` and `mention` notifications (decision 30 says which messages create
  them) that are at least 30 minutes old and have no digested time. Each notification points at a message, so the
  digest is built from notifications, not by scanning threads.
- **When.** The reminder job runs it every 15 minutes. A user gets a digest only when they have something to include
  and have no open SSE stream at that moment; with push on one server (decision 4), that is a lookup in
  `PushMessage.RECEIVERS`. A user reading in the app clears the notifications first, so they are never emailed about
  what they have already seen.
- **What it looks like.** "You have 7 unread messages in 3 conversations", then for each thread its name, how many
  unread, and the latest three messages with author, time and text (plain text, markdown left as typed), and a link
  that opens the thread in the panel (and the record, for a record thread). Sent with `EXT.sendMail`.
- **Bookkeeping.** Sending a digest sets the digested time on the notifications it included, in the same transaction
  as the `MailLog`. There is no per-user record of digests.
- **Not sent** to users with no email address, or who have turned the digest off (the embedded `cockpit` preference on
  `admin.User`). Muted threads create no
  notifications, so they never appear.
- **Settings.** `cockpit.digestIntervalMinutes` (default 15) and `cockpit.digestDelayMinutes` (default 30) in the JSON
  configuration.

### Inbox (C3)

- `InboxProvider`, found through CDI as `DocumentConverter` is: `count(user)` and `items(user, page)`, where an item has a
  title, subtitle, due time, the provider's name and a link to the view where it is actioned.
- The Inbox tab lists items from every provider, grouped by provider. Opening an item opens its view in the page, and
  the action is taken there as for any view.
- A provider calls `EXT.getCockpit().inboxChanged(user)` when a user's items change. That pushes `inbox`.
- Skyve Flows becomes a provider when it is on Skyve 10. Applications can add their own, such as "timesheets to
  approve".

### Assistant (C4)

Outline only. It is designed with the agent plan's phase 2.

- A provider is configured in the JSON configuration; the model is external.
- The panel's chat posts to a server endpoint that talks to the model, which calls Skyve's agent service in-process as
  the user (no more than the user's permissions).
- Replies stream on a response of their own, not through the shared SSE queue, which drops when full.
- `@this` hands the agent the open conversation's `webId`. The agent fills the view and the user presses Save (agent
  plan, direction 27). This needs a rerender aimed at one conversation: the push item names the `webId`, and only the
  tab showing it rerenders.
- Deletes and external effects ask the user to confirm in the panel, whatever the agent decides.
- The transcript is an `assistant` thread, private to the user, stored and archived as messages are. It keeps the user's
  and the assistant's text; tool calls go to the audit, not the transcript.
- Guidance and "save as a task" follow the agent plan.

### Security

- The cockpit is for signed-in users only. Public users get no panel, stream or endpoint.
- Every read of a thread checks membership, or read access to the record for a record thread, on every call. The
  archive is never queried without that check and the thread restriction.
- The one exception is the `MessageAuditor` role, which can read across the customer, and every one of its reads is
  logged in `MessageAuditLog` (decision 29).
- The user directory reveals only users in the poster's customer and data group.
- Message text is never rendered as HTML from the server. The client escapes it and renders the markdown subset with a
  sanitiser.
- Push carries no content.
- The email digest does: message text leaves Skyve in it (decision 17). A user can turn the digest off, and muted
  threads never appear in it.
- An agent in C4 acts with the user's permissions or fewer.

## Implementation plan

### Running it: parallel streams, with Q&A alongside

C0 and C1 are built by sub-agents in parallel streams, each in its own worktree. While they build, the main session
and Mike settle the next phases (Q&A track below), so that C2 and C3 are ready to start when C1 lands.

**Branches.**

- `cockpit`: the integration branch, off `skyve12`, in its own worktree (`~/skyve-cockpit`). Mike's checkout keeps
  `skyve12`; he merges `cockpit` into it when a phase is accepted.
- `cockpit-<stream>`: one per stream, off `cockpit`, each in its own worktree (`~/skyve-cockpit-<stream>`). The
  orchestrator merges a stream into `cockpit` after its gate passes, and merges `cockpit` into streams that need
  what landed.
- The plan is changed only on `cockpit`, by the orchestrator. A Q&A answer that changes C0 or C1 after a stream has
  started goes to that stream as a change request, not only into the plan.

**Rules for every stream.**

- Maven runs in the stream's own rsync'd mirror; SAIL and WildFly on the stream's own port offset.
- Streams touch only the files they own (table below). A stream that needs a change elsewhere asks the orchestrator.
- Built artefacts are not committed by streams: both `skyve-min.js` bundles, skyve-vue's output in skyve-web and
  `WEB_RESOURCE_FILE_VERSION` are rebuilt and bumped by the orchestrator after merging, once per phase. The generated
  domain is the exception: S3 owns it and commits it.
- Each stream's gate: its unit tests and the tests named in its steps, in its mirror; a review; the fixes; the gate
  run again after the fixes.
- Product-code streams run on Opus; reviews on Sonnet at high effort, with one Opus correctness review per phase.

**Contracts fixed here, so streams do not wait for each other.**

- Entries and files: `skyve/vue/listgrid.js`, `listgrid.css`, `cockpit.js`, `cockpit.css`.
- The call queue: `common/util.js` defines `SKYVE.listgrid(config)` and the `SKYVE.cockpit` methods below as stubs
  that queue; each entry replaces them and replays the queue.
- `SKYVE.cockpit`: `mount(element, {strings, user: {id, name}, renderer: "sc" | "pf"})`; `attachButton(element)` (the
  cockpit keeps the badge inside it current and toggles the panel on click); `context({module, document, bizId,
  bizKey, webId} | null)`; `changed(item)`; `open()`, `close()`.
- The renderers' flag: `isc.BizUtil.canUseCockpit` (SmartClient) and `HarnessView.isCanUseCockpit()` (PrimeFaces).
- The post-commit callback: `Persistence.afterCommit(Runnable)`.
- The `/cockpit` calls and JSON shapes in [Server endpoints](#server-endpoints).

**Streams.**

| Stream | Builds | Owns | Waits for |
|---|---|---|---|
| S1 `c0-vue` | C0 steps 1 to 4, 6, 7 | `skyve-vue/` (except `src/cockpit/`), `skyve-web/src/js/common/util.js`, `ConverterRegistrationTest` | Nothing |
| S2 `c0-web` | C0 steps 5, 8, 9 | The four `view.xhtml`, `ThemeFaceletsStructureTest`, `undertow-handlers.conf`, `docs/skyve-12-changes.md` | Nothing (the entry names are fixed above); its browser checks run once S1 is merged |
| S3 `c1-switch` | C1 steps 1 and 3 | Customer XML and schemas, `CustomerMetaData`, `CustomerImpl`, `User` and its implementations, `UserTest`, `DesktopView`, `HarnessView`, `skyve.json`, the admin documents, generated domain | Nothing |
| S4 `c1-commit` | C1 step 2 | `Persistence`, `AbstractHibernatePersistence`, `PersistenceInjectable`, their tests | Nothing |
| S5 `c1-panel` | C1 step 7, against the JSON shapes with a stand-in server on the dev page | `skyve-vue/src/cockpit/` and the `cockpit.js` entry's body | Nothing; merged after S1 |
| S6 `c1-server` | C1 steps 4, 5 (the push item), 6, 10 | skyve-ext's cockpit package, `EXT`, `PushMessage`, `CockpitServlet`, `web.xml`, `SpringSecurityConfig` and the tests that list servlets, `QuartzJobScheduler`, `docs/theme-resolution.md` | S3, S4 |
| S7 `c1-wiring` | C1 steps 5 (the client side), 8, 9 | `sse/connect.xhtml`, `desktop/util.js`, `prime/pf.js`, `header.js`, `desktop/home.xhtml`, editorial's `template.xhtml` and `head-resources.xhtml`, the `<cockpit:panel/>` composite, the template structure tests | S1, S3 (flags), S5 (API); `view.xhtml` stays S2's |

**Order.**

1. Wave 1: S1, S2, S3, S4 and S5 start together.
2. C0 gate once S1 and S2 are merged: the orchestrator rebuilds the bundles, bumps the version and runs C0's exit. It
   runs while wave 2 builds.
3. Wave 2: S6 once S3 and S4 are merged; S7 once S1, S3 and S5 are merged.
4. C1 gate once S6 and S7 are merged: C1 step 11, then C1's exit and acceptance checklist.

**Q&A track** (the main session with Mike, while the waves run):

- **For C2:** the CSRF approach (needed before posting); then the detailed `docs/ai/plan_c2.md`: channel creation and
  who may add members, the mention list's source and limits, the digest email's content and template, the Messages
  tab's layout, the archive export rule's tests.
- **For C3:** how inbox providers are found. Decision 32 keeps the cockpit off injection, so registration at startup
  (`EXT.getCockpit().addInboxProvider(...)`, from a customer `<observer>`) is the proposal, in place of the CDI lookup
  written in [Inbox (C3)](#inbox-c3). Also whether a `CockpitInjectable` wrapper is wanted, as `MailServiceInjectable`
  is.
- **For C4:** nothing until the agent plan's phases 0 to 2 are scheduled.


### C0: skyve-vue as ES modules

**Goal.** skyve-vue builds entry modules and shared chunks instead of one IIFE file; the list grid, the calendars and
the image component work exactly as before on every PrimeFaces template, with a smaller download; an empty `cockpit.js`
entry exists for C1.

**Steps.**

1. `vite.config.js`: ES-module output with the entries `listgrid.js` and `cockpit.js` (empty until C1), hashed chunk
   names, one CSS file per entry (`listgrid.css`, `cockpit.css`). Keep `__VUE_PROD_HYDRATION_MISMATCH_DETAILS__`, the
   dev proxy and the serve-only primeicons plugin; `index.html` stays the dev page, importing `listgrid.js`.
2. Split `main.js` into the entries. `listgrid.js` gets the list grid's code, including the 12-hour `LOCALE`; repoint
   `ConverterRegistrationTest`'s `VUE_MAIN` to the file that now holds it. Each entry replaces its stub (step 3) and
   replays queued calls.
3. The call queue: in `skyve-web/src/js/common/util.js`, stubs for `SKYVE.listgrid` and `SKYVE.cockpit` that queue
   their calls until the entry replaces them; rebuild both `skyve-min.js` bundles with skyve-web's `build.xml`
   (`compress`). `VueListGridScript` and its inline call stay as they are; check `ListGridStaticTest` still passes.
4. `build.xml` (skyve-vue): copy the whole output into skyve-web's `skyve/vue/`, removing the old files first.
5. The four PrimeFaces `view.xhtml` templates: replace the `h:outputScript`/`h:outputStylesheet` pair with `<script
   type="module">` and `<link>` tags for `listgrid.js` and `listgrid.css` on the static `skyve/vue/` path, with
   `?v=#{bean.webResourceFileVersion}`, still only where the Vue list grid is on. Update `ThemeFaceletsStructureTest`
   to the new tags (it covers all four). Bump `WEB_RESOURCE_FILE_VERSION` and check on editorial in the browser that the
   new entry and its chunks are fetched (decision 45: the other three are not checked in the browser).
6. (Done before C0.) The trim (decision 35) is on `skyve12` in `9ea2b1f516`; C0 keeps `src/primeflex.css` with the
   list grid's entry.
7. Lazy parts with `import()`: the snapshot picker, and other parts a first render does not need. Record the sizes
   before and after, per entry and chunk, raw and gzipped.
8. Gzip (decision 37): add the `compress` rule to skyve-war's `undertow-handlers.conf`. With `curl -H 'Accept-Encoding:
   gzip'`, check a Vue entry and chunk, a PrimeFaces resource, a SmartClient script and a stylesheet come back
   `Content-Encoding: gzip`; and that a page, a list servlet response and `/sse/stream` do not; with `curl -N` on
   `/sse/stream`, a push (a growl from a kitchen-sink action) arrives at once. Drop `.mjs` from the rule if no output
   uses it.
9. `docs/skyve-12-changes.md` (new, in the form of `docs/skyve-10-changes.md`): re-copying the PrimeFaces templates for
   the Vue list grid, and gzip with how to comment it out.
**Exit.** The `kitchensink` and `converters` SAIL suites pass on the editorial template and in SmartClient; `ConverterRegistrationTest`, `ThemeFaceletsStructureTest`
and `ListGridStaticTest` pass; script and style are served gzipped; the sizes are recorded.

### C1: the panel, notifications and reminders

**Goal.** A signed-in user in either renderer sees a live badge and a panel whose Activity tab shows notifications and
reminders; Skyve and applications can notify and remind users from code.

**Steps.**

1. The switch, following `<textSearchRoles>` everywhere it appears:
   - `<cockpitRoles>` in both copies of `customer.xsd`, in the position that matches `CustomerMetaData`'s `propOrder`;
   - `CustomerMetaData`: the field, its `propOrder` entry, `populateFeatureRoles` in `convert`, and the
     `XMLMetaData.removeEmptyChildElements` list; `CustomerImpl`;
   - `User.canUseCockpit()` (abstract, as `canTextSearch()` is), in `UserImpl`, `SuperUser` (false: the super user
     runs jobs and has no browser) and skyve-ext's `UserInjectable`, with `UserTest`;
   - the renderers learn the flag: `DesktopView` writes `isc.BizUtil.canUseCockpit=` beside `canSwitchMode`, and
     `HarnessView` gets `isCanUseCockpit()` for the PrimeFaces templates;
   - the `demo` customer (which the kitchen-sink SAIL suite signs in to) given a cockpit role;
   - the installation-wide setting `cockpit.notificationRetentionDays` (default 30), read into `UtilImpl` and
     documented in the sample `skyve.json`.
2. The post-commit callback, `Persistence.afterCommit(Runnable)`: a default method on `Persistence`, implemented in `AbstractHibernatePersistence` (where
   `commit` and `rollback` are, both `final`) and delegated by `PersistenceInjectable`. Callbacks registered in a
   transaction run once, in order, after the commit that ends it, and are dropped by `rollback`; `commit(false)`
   (commit and carry on) runs and clears them; a callback registered while callbacks run waits for the next commit;
   an exception in one is logged and does not stop the rest. Tests for each.
3. Admin documents `Notification` and `Reminder` in `admin.xml` (no role privileges, see [Notifications and
   reminders](#notifications-and-reminders-c1)), their interfaces in skyve-core; generate the domain with
   `DomainGenerator` and commit `src/generated` and `src/generatedTest`.
4. The `Cockpit` interface, its implementation, `CockpitStaticSingleton` (set in `SkyveContextListener`) and `EXT.getCockpit()`: notify, remind, counts, mark read, reminder firing and the purge.
5. The cockpit push item (refusing an empty user list), the optional `onOpen` hook on `sse/connect.xhtml`, and both push
   handlers (`desktop/util.js`, `prime/pf.js`) passing it to `SKYVE.cockpit.changed`.
6. `CockpitServlet` and its `web.xml` declaration at `/cockpit/*`, following `SmartClientSnapServlet` as described in
   [Server endpoints](#server-endpoints), with the JSON shapes there; its matcher in `SpringSecurityConfig`; add it to
   `SpringSecurityConfigAccessTest`, `FacesThemeSensitiveEndpointPropagationTest` and `docs/theme-resolution.md`. CSRF
   is left for now (decision 18).
7. The `cockpit.js` entry module (from C0), plain Vue with its own stylesheet (decision 43): panel, four tabs (Messages, Inbox and
   Assistant show "coming" until their phases), Activity tab, counts, `context()`, `changed()`.
8. SmartClient: when `isc.BizUtil.canUseCockpit`, load the `cockpit.js` module, `cockpit.css` and Montserrat's
   stylesheet in `home.xhtml`; the docked canvas; the cockpit button with its live badge before the avatar in
   `header.js`; `SKYVE.cockpit.context(...)` called on each view change and cleared on a list or the home page.
9. PrimeFaces, editorial only (decision 45): when `isCanUseCockpit()`, load `cockpit.js` and `cockpit.css` from
   editorial's `head-resources.xhtml` (Montserrat is already loaded there), so every signed-in page has them, not only
   views; `<cockpit:panel/>` in editorial's `template.xhtml`; the cockpit button with its badge before the avatar,
   replacing the hidden `<p:badge>` placeholder in `template.xhtml`; editorial's docking styles;
   `SKYVE.cockpit.context(...)` on each view render. Ecuador's and ultima's placeholders are left as they are. Check
   `ThemeRoutingStructureTest`, `PrimeFacesHeadSailRecorderTest` and `ThemeFaceletsStructureTest`.
10. The reminder and purge job, scheduled from `QuartzJobScheduler` when any customer has cockpit roles, looping over
    those customers (see [Notifications and reminders](#notifications-and-reminders-c1)).
11. Rebuild both `skyve-min.js` bundles and skyve-vue's output, bump `WEB_RESOURCE_FILE_VERSION` once for the phase,
    and add the cockpit's switch to `docs/skyve-12-changes.md`.

**Exit.** The acceptance checklist for C1 passes in both renderers on the `demo` customer's kitchen sink.

### C2: messages

Its own `docs/ai/plan_c2.md` when scheduled. First steps: add the export rule to `DocumentConverter` and
`ExportDocumentsToArchiveJob` with tests that audit export is unchanged; then the documents, posting, the merged read,
record threads, mentions, the email digest and the panel's Messages tab.

### C3: inbox

Small. It can follow C1 directly, with a kitchen-sink provider standing in until Flows has one.

### C4: assistant

Its own plan, written with the agent plan's phase 2.

## Deferred

- **Erasure.** When it is needed, the design is: overwrite the message's archive line in place with an "erased" line of
  the same length, so no offsets move and nothing is reindexed; delete its Lucene document and merge deletions; clear or
  delete the database row; and keep an erasure record (id, who, when, no content) in the database, so that erasure is
  reapplied after a restore or a reindex. The same tool would serve audits. Nothing in C2 needs to change to allow it.
- **Backing up archive files** (decision 19). `BackupJob` backs up the tables and the content repository. It does not
  appear to include `content/archive/`, so archived audits are backed up today only if something else copies that
  directory. "Keep forever" for messages makes this matter more; when it is taken up, it should cover audits and mail
  logs too.
- **PrimeVue 4 and Tailwind** for the list grid (decision 42); the design, steps, checklists and measurements are in
  the [appendix](#appendix-primevue-4-and-tailwind-deferred).
- **The cockpit in ecuador, diamond and ultima** (decision 45): the button, the composite and docking styles in each,
  and a variable bridge from each layout's own CSS variables to the `--skyve-editorial-*` names the cockpit reads.
- Push across several servers.
- Presence, typing indicators, reactions, replies within a thread, attachments, open channels, `@group`.

## Acceptance checklist

C0:

- [ ] skyve-vue's build writes ES-module entries (`listgrid.js`, `cockpit.js`) and hashed shared chunks, and
      `build.xml` copies them all.
- [ ] The Vue list grid, with its calendars, image component and snapshot picker, works on the editorial template; the
      `kitchensink` and `converters` SAIL suites pass; `ThemeFaceletsStructureTest` passes for all four `view.xhtml`.
- [ ] A `SKYVE.listgrid(...)` call made inline before the entry has loaded is replayed when it loads.
- [ ] A page loads the chunks only from `skyve/vue/`, none through a Faces resource URL.
- [ ] A change of `webResourceFileVersion` makes the browser fetch the new entries and chunks.
- [ ] `docs/skyve-12-changes.md` says to re-copy the templates and describes gzip.
- [x] No PrimeFlex, primeicons stylesheet or luxon is left in the output (done on `skyve12`, `9ea2b1f516`; the
      formatter was checked against luxon, not with a committed test).
- [ ] Before-and-after sizes are recorded per entry.
- [ ] Script, style and SVG, including SmartClient's and PrimeFaces' resources, come back gzipped to a client that
      accepts it, and uncompressed to one that does not.
- [ ] Pages, servlet JSON and `/sse/stream` are not compressed, and a push still reaches the browser at once.
- [ ] Commenting out the rule in `undertow-handlers.conf` turns compression off and changes nothing else.

C1:

- [ ] A customer with no `<cockpitRoles>` gets no panel, badge or push item, its users are refused by `/cockpit`, and
      the existing tests pass unchanged.
- [ ] On the editorial template the panel takes each theme's accent and goes dark in dark mode (system and forced);
      in SmartClient it shows editorial's default colours; Montserrat is the font in both; no cockpit rule changes
      anything outside the panel.
- [ ] In a customer with cockpit roles, a user without one gets no panel and is refused by `/cockpit`.
- [ ] `EXT.getCockpit().notify(...)` in a transaction that rolls back creates no notification and sends no push.
- [ ] A notification sent to a signed-in user updates the badge in SmartClient and in PrimeFaces without a page load.
- [ ] After the stream drops and reconnects, the badge shows the right count without any new event.
- [ ] Marking a notification read in one tab updates the badge in the user's other tabs.
- [ ] A reminder fires within a minute of its due time and appears in the Activity tab.
- [ ] Notifications read more than the retention days ago are purged, and unread ones are not.
- [ ] One user cannot read or mark another user's notifications through `/cockpit`, and a public user gets nothing.
- [ ] The panel docks on a wide screen and becomes a full-screen sheet on a narrow one, on the editorial template and in
      SmartClient.
- [ ] Ecuador, diamond and ultima show no cockpit button or panel and look as before.
- [ ] The panel stays open across navigation.
- [ ] A PrimeFaces view with a Vue list grid loads `cockpit.js` and `listgrid.js` and downloads Vue once.
- [ ] `/cockpit` refuses an anonymous request for every call, and no call uses a method other than `GET` or `POST`.
- [ ] A notification sent by a user in one data group to a user in another appears for the recipient.

C2 (refined in its own plan):

- [ ] Audit and mail log export are unchanged by the export rule.
- [ ] A message is not exported until 20 minutes after it was created, and cannot be edited after 15.
- [ ] A thread with archived and unarchived messages reads in order, with no gaps and no duplicates.
- [ ] A user who is not a member, or who cannot read the record, gets nothing from a thread, from the database or from
      the archive.
- [ ] A message deleted after archiving never appears again, from either store.
- [ ] A record thread survives the record's deletion and is readable only by its members and message auditors.
- [ ] An administrator without `MessageAuditor` can list channels and members but cannot read a message by any route.
- [ ] Every auditor search and thread opened writes a `MessageAuditLog` row, and a failure to write it fails the read.
- [ ] A digest goes to a user who is offline with unread notifications at least 30 minutes old, includes each one
      once only, and goes to nobody who is online, has read them in the app, or has the digest off.

## Better engineering notes

- **The other PrimeFaces templates** (ecuador, diamond, ultima) have no cockpit (decision 45). Adding it later means the
  button, the composite and docking styles in each, since each duplicates the page frame.
- **SmartClient and Vue on one page** is new. SmartClient's home page loads only the `cockpit.js` entry and its chunks;
  keep it small and its styles scoped.
- **Nothing is gzipped today.** Neither WildFly's configuration nor `web.xml` compresses responses. SmartClient's home
  page is about 7 MB of script and style raw, 1.7 MB gzipped; the Vue list grid's bundle 1.3 MB raw, 248 KB gzipped (26
  November 2026). C0 turns it on in the WAR (decision 37). Installations behind a proxy that already compresses should
  check the proxy passes through a response that already has a `Content-Encoding`, as nginx and Apache do by default.
- **PrimeVue 3 is the older line.** Staying on it (decision 42) defers the move rather than avoiding it; when it is
  needed, the [appendix](#appendix-primevue-4-and-tailwind-deferred) has the plan. Keeping the cockpit off PrimeVue
  keeps that move to the list grid alone.
- **skyve-vue has no JavaScript tests.** `DateFormat.js` was checked against luxon with a throwaway script. A small
  test runner (Vitest) would let that comparison, and the converters' number formatting, become committed tests.
- **The push queue drops when full** and the per-user stream cap closes the oldest tab's stream. Content-free pushes and
  fetching counts on every open make both harmless; do not put content in pushes later.
- **The archive list model has no scoping.** Messages avoid it. Any future archived document readable by ordinary users
  needs the same care, or the model needs scoping added.
- **The archive schedule runs as one customer's user.** Check, in C2, whether export covers every customer of a
  multi-customer installation.
- **Hot counters.** The unread counter update touches every member row of a channel on each post. That is fine for
  hundreds of members; a very large channel would want a different scheme.

## AI validation plan

- Build and test in an rsync'd mirror of the checkout, not in the shared checkout.
- H2 tests through the real endpoints for `/cockpit`, following [test-patterns.md](test-patterns.md): security
  (another user, a public user, an anonymous request; CSRF once decided), counts, read state, the post-commit push, reminder firing and the purge.
- Unit tests for the post-commit callback and for the push item in both push handlers.
- SAIL in both renderers on the `kitchensink` suite: the badge updates on a push from the server, the panel opens, docks
  and stays open across navigation, a notification is marked read. Condition waits only, each justified; no fixed
  sleeps.
- C2: archive tests that export a thread part way, read it across both stores, delete after archiving, and check that
  audit export is unchanged; permission tests for every read path.
- Coverage of changed lines as in [testing-and-coverage.md](testing-and-coverage.md).

## User validation suggestions

- After C1: turn the cockpit on in the kitchen sink, send yourself a notification from a Bizlet, and watch the badge in
  SmartClient and in each PrimeFaces template. Drop the network for a minute and bring it back.
- Open the panel on a phone-width window in PrimeFaces.
- Before C2: post 20 messages in a thread on a rig with the archive running every few minutes, wait for an export, then
  scroll the thread back and delete a message that has been archived.

## Appendix: PrimeVue 4 and Tailwind (deferred)

Planned as phases V4 and VT on 26 November 2026 (decisions 38 and 40), then deferred the same day (decision 42) once the
measurements below showed no download saving. Kept as written for when the list grid has to leave PrimeVue 3. Where it
says "the cockpit", read it as written before decision 43.

### PrimeVue 4 (V4)

skyve-vue uses Vue 3.5 and PrimeVue 3.52. Vue stays; PrimeVue moves to 4. What it touches, from the source on
`skyve12` (26 November 2026):

| Area | Change |
|---|---|
| Packages | `primevue` 4 and the theme preset package (`@primeuix/themes`; check the current name when starting). `primevue/resources` (theme and core CSS) goes; `theme.css`'s `@import` of the Lara theme goes |
| Theme | `app.use(PrimeVue, { theme: { preset: Lara, options: { cssLayer: { name: 'primevue' } } }, ... })`, keeping the cascade layer `theme.css` sets up now, so Skyve's and the templates' rules still win. Skyve's colour and size adjustments become preset tokens where PrimeVue offers one, CSS otherwise |
| Renamed components | `Calendar` to `DatePicker` (ListGrid twice, DateOnlyCalendar, TimeCalendar); `Dropdown` to `Select` (ListGrid, three times); `TriStateCheckbox` (ListGrid's boolean filter) to `Checkbox` with `indeterminate`, with the grid cycling unset, true and false itself as the filter does now |
| Same names | `DataTable`, `Column`, `Button`, `InputText`, `MultiSelect`, `TieredMenu`, `ContextMenu`, `Dialog` |
| Imports | `FilterOperator` (ListGrid) and `PrimeIcons` (SnapshotPicker) move from `primevue/api` to `@primevue/core/api` |
| Class names | PrimeVue 4 renames some components' inner classes. `ListGrid.vue`'s styles have 140 PrimeVue selectors (7 on the renamed components) and editorial's `vue.css` has 6; each is checked against PrimeVue 4's markup |
| Behaviour to recheck | DataTable's lazy, sort, filter and page events and the filter menu, which the grid drives heavily; the calendar props from `calendarProps()` in `Converters.js` (which `ConverterRegistrationTest` reads); the AM/PM locale fix in `main.js` (PrimeVue 3 read a typed PM as AM); `alignFont()` and `build.xml`'s Inter font removal |

- **Runtime styles and CSP.** PrimeVue 4 adds its styles in `<style>` elements. Skyve's `SecurityHeadersFilter`
  Content-Security-Policy has `style-src 'self' 'unsafe-inline'` (checked 26 November 2026), so they are allowed; an
  installation with a stricter policy can give PrimeVue a nonce (its `csp.nonce` option).
- **Faces partial updates.** The grid is re-mounted when Faces re-renders its container. PrimeVue 4 adds each
  component's styles once per document and leaves them in place, so a re-mount adds nothing; checked in the SAIL runs.
- **Lara is kept only through V4.** VT replaces it with Tailwind (decision 40), so the preset's look is not tuned in V4
  beyond what the comparison screenshots need.

### Styling with Tailwind (VT)

**The approach.** PrimeVue 4 in unstyled mode keeps the components' behaviour and accessibility and renders no theme;
their look comes from Tailwind classes. PrimeTek publishes this combination as Volt: unstyled PrimeVue components with
Tailwind v4 classes, copied into the project as source and owned there. skyve-vue starts from Volt's versions of the
components it uses and changes them to editorial's look (check that Volt's licence suits Skyve's when starting).

**Following editorial's CSS variables.** Tailwind v4's theme is declared in CSS and compiled to CSS variables, so its
values can be the template's own variables:

```css
@theme {
  --color-primary: var(--skyve-editorial-accent, rgb(37 99 235));
  --color-surface: var(--skyve-editorial-panel-bg, #fff);
  --color-border: var(--skyve-editorial-panel-border, #d2d7d9d9);
  --color-text: var(--skyve-editorial-menu-text, #444c56);
  --font-sans: var(--skyve-editorial-font, "Montserrat", system-ui, sans-serif);
  /* ... one entry per editorial variable the components need */
}
```

Editorial defines 62 `--skyve-editorial-*` variables, and its six themes (blue, emerald, indigo, custom, dark,
smartclient) and dark mode (`prefers-color-scheme` or forced) work by changing them. A component styled
`tw:bg-surface tw:text-text` therefore changes with the theme with no extra rules. The fallback after each variable is
editorial's default value, which is what other templates and SmartClient's home page get (decision 41).

**Living on a PrimeFaces page.**

- **No preflight.** Tailwind's base reset would restyle the whole PrimeFaces page; only Tailwind's theme and utilities
  are imported.
- **A prefix.** Utilities are prefixed (`tw:flex`), so they cannot collide with PrimeFaces', the layouts' or
  editorial's classes (`.hidden`, `.flex`, `.grid`, `.field` all exist on the page). The prefixed utilities replace the
  `src/primeflex.css` subset from the trim, except `.field`, which is page markup and stays.
- **Cascade layers.** The page's own CSS is not in a layer, and unlayered rules beat layered ones whatever their
  specificity. If Tailwind's utilities sit in a layer, an editorial rule such as `table td { ... }` would beat
  `tw:p-2`. The utilities therefore go unlayered (or marked important), and VT checks for page rules that still reach
  inside the components.
- **Editorial's current overrides go.** Editorial's `vue.css` (which maps Lara's variables to editorial's) and the
  rules in editorial's `main.css` that restyle the Vue grid (47 `.p-*` selectors) are removed once the components carry
  editorial's look themselves. 12 of those rules are combined with PrimeFaces `ui-*` selectors and are split, not
  deleted.
- **Size.** Tailwind writes only the classes used: 17 KB (4 KB gzipped) for Volt's versions of the grid's components.
  The download as a whole does not shrink, though; see [Download sizes](#download-sizes-measured-26-november-2026).

#### Download sizes (measured 26 November 2026)

The same eleven components the grid uses, built alone (no Skyve code) as an IIFE, minified:

| Build | Script | CSS | Total | Gzipped |
|---|---|---|---|---|
| PrimeVue 3.52 + Lara (today's approach) | 638 KB | 216 KB | 854 KB | 178 KB |
| PrimeVue 4.5 + Lara preset | 947 KB | none (injected) | 947 KB | 203 KB |
| PrimeVue 4.5 unstyled, no styling | 833 KB | none | 833 KB | 188 KB |
| PrimeVue 4.5 unstyled + Volt + Tailwind | 868 KB | 17 KB | 885 KB | 206 KB |

PrimeVue 4 moves styling from CSS into script and its script is larger: its icons (66 KB), core and styling engine
(about 95 KB) and each component's style module (24 KB) are bundled even in unstyled mode, and the Lara preset adds
114 KB of script. So for the list grid (today 913 KB, 198 KB gzipped, after the trim) V4 plus VT comes to about
945 KB, about 225 KB gzipped: roughly the same raw and about 15% more gzipped. V4 and VT are for the styling and for
staying on PrimeVue's current line, not for size. The download savings come from gzip (decision 37: about 913 KB to
about 225 KB on the wire, as nothing is compressed today), from C0's entry splitting (a view without a grid does not
load DataTable), and from the one-year cache.

### V4: PrimeVue 4

**Goal.** skyve-vue runs on PrimeVue 4 with the Lara preset; the list grid, the calendars, the image component and the
snapshot picker look and behave as on PrimeVue 3 in all four PrimeFaces templates; the theme CSS no longer ships as one
file.

**Steps.**

1. Branch off `skyve12`. Record the grid's current sizes and take reference screenshots of the grid, its filter menus,
   the date and time fields and the snapshot picker in each template, for comparison.
2. Upgrade `primevue` to 4 and add the theme preset package; remove the `primevue/resources` imports and `theme.css`'s
   Lara import; configure the Lara preset in the `primevue` cascade layer, keeping `ripple` and the locale.
3. Move `FilterOperator` and `PrimeIcons` to `@primevue/core/api`.
4. `Calendar` to `DatePicker` in its four places, keeping `calendarProps()`'s formats; check the AM/PM locale fix still
   applies and still matters.
5. `Dropdown` to `Select`; `TriStateCheckbox` to a `Checkbox` with `indeterminate`, cycling unset, true and false, and
   giving the filter the same values as now.
6. DataTable: check the lazy load, sort, filter (menu and match modes), paginator template and row selection events
   against what `ListGrid.vue` reads; fix any payload or prop that changed.
7. Styles: check the 140 selectors in `ListGrid.vue` and the 6 in editorial's `vue.css` against PrimeVue 4's markup;
   move Skyve's theme adjustments to preset tokens where there is one; check `alignFont()` and the Inter regex in
   `build.xml` (drop it if the preset ships no font).
8. Bump `WEB_RESOURCE_FILE_VERSION`; build; compare against the reference screenshots; record the sizes.

**Exit.** The Vue list grid, date-field and converters SAIL tests pass in all four PrimeFaces templates,
`ConverterRegistrationTest` passes, the screenshots match apart from differences Mike accepts, and the sizes are
recorded.

### VT: styling with Tailwind

**Goal.** The list grid, the calendars, the image component and the snapshot picker are unstyled PrimeVue 4 with
Tailwind classes, look like the editorial template in each of its themes and in dark mode, follow its variables when
they change, and still work on the other templates with editorial's defaults.

**Steps.**

1. Reference screenshots on the editorial template, in each theme and in dark mode: the grid with data, each filter
   menu, the paginator, the context menu, the snapshot picker, the date and time fields, open and closed. These are
   what VT is judged against, alongside editorial's PrimeFaces data table and inputs on the same page.
2. Add Tailwind v4 and its Vite plugin. One stylesheet imports Tailwind's theme and utilities with the `tw` prefix and
   without preflight, utilities unlayered, and declares `@theme` from editorial's variables with editorial's defaults as
   fallbacks.
3. Switch PrimeVue to unstyled. Bring in Volt's versions of DataTable, Column, Button, InputText, Select, MultiSelect,
   DatePicker, Checkbox, Dialog, TieredMenu and ContextMenu as source, and change them to editorial's look using the
   screenshots and editorial's current Vue rules as the reference.
4. Replace the `src/primeflex.css` classes in ListGrid and SnapshotPicker with prefixed utilities; keep `.field`.
5. Remove the Lara preset, editorial's `vue.css` and the grid rules in editorial's `main.css` (splitting the 12 shared
   with PrimeFaces selectors); remove `alignFont()` if the font now comes from the theme variable.
6. Check dark mode both ways (system setting and forced), theme switching without a reload where editorial allows it,
   and that no page rule reaches inside a component.
7. The other templates and SmartClient: the grid works and reads clearly with the fallbacks in ecuador, diamond and
   ultima; screenshots for Mike.
8. Bump `WEB_RESOURCE_FILE_VERSION`; record the sizes; run the SAIL tests.

**Exit.** The editorial screenshots match apart from differences Mike accepts; the Vue list grid, date-field and
converters SAIL tests pass in all four PrimeFaces templates; no Lara, `vue.css` or editorial grid override is left;
sizes recorded.

### Acceptance checklists

V4:

- [ ] `package.json` has PrimeVue 4 and its theme preset; no `primevue/resources` import, `primevue/api` import,
      `Calendar`, `Dropdown` or `TriStateCheckbox` is left in `skyve-vue/src`.
- [ ] The Vue list grid's filters (text, number, date, time, boolean, enumeration), sorting, paging, selection, the New
      button and the snapshot picker work in all four PrimeFaces templates, with their SAIL tests passing.
- [ ] The date and time fields show and read every converter pattern, 12-hour times' AM and PM included, and
      `ConverterRegistrationTest` passes.
- [ ] A Faces partial update that re-renders a grid leaves one set of PrimeVue styles in the page.
- [ ] Before-and-after sizes are recorded.

VT:

- [ ] PrimeVue runs unstyled; no Lara preset, `primevue` theme package, editorial `vue.css` or editorial rule that
      restyles the Vue grid is left.
- [ ] On the editorial template, the grid, its menus and overlays, the snapshot picker and the date and time fields match
      the reference screenshots in every theme and in dark mode, system and forced.
- [ ] Changing an editorial variable in the browser's developer tools changes the components at once.
- [ ] Tailwind adds no reset and no unprefixed class to the page; PrimeFaces components elsewhere on the page look as
      before.
- [ ] On ecuador, diamond and ultima the grid works and is legible with editorial's defaults.
- [ ] The Vue list grid, date-field and converters SAIL tests pass in all four templates; sizes are recorded.
