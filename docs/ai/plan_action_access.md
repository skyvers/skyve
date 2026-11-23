# Plan: Declared action accesses - let a view grant a direct call to an action

## Summary

M1 (`docs/ai/plan_m1.md`, commit `ff745a2974`) refuses a view action when the view, as last rendered, offers no reachable trigger for it. That breaks a legitimate pattern: an action called from code rather than from a control. The usual case is custom JavaScript calling `doAction('X')` for an action declared `inActionPanel="false"`, with no button, link or `<server>` handler. M1 shipped with "no metadata opt-out" (`docs/server-side-action-availability.md`, Upgrading).

This plan adds that opt-out as a new entry in a view's `<accesses>`:

```xml
<accesses generate="true">
	<actionAccess className="RecalculateTotals" />
</accesses>
```

The entry says "this view calls `RecalculateTotals` from code". `ActionAvailability.check` passes a request for that class without walking the view. Role permission (`canExecuteAction`) is unchanged.

The design follows the discussion with Mike on 2026-10-26:

- **Explicit accesses are unconditional.** An access entry has no widget behind it, so there is no condition to evaluate. Conditions belong to the widgets and actions that render triggers.
- **Action accesses are never generated.** `ActionAvailability` already walks the view against the bean on every request and sees every trigger with its live conditions. A generated access would be unconditional, so one conditional `<button>` would grant its action always, which is exactly what M1 stops.
- **No conditions on access entries.** Declared accesses do not get `invisible` or `disabled` attributes. A state rule for an action called from code belongs in the action's `execute`, because there is no control in the client to show the state.

Modules: `skyve-core` (metadata, `ViewImpl`, `ActionAvailability`, validation, fluent API, XSD), `skyve-war` (XSD copy, kitchen-sink fixture, H2 tests), docs.

## Scope

### Decisions from the discussion (Mike, 2026-10-26)

1. **A new `<actionAccess>` entry in a view's `<accesses>`**, rather than an attribute on `<action>`.
2. **Unconditional.** A declared action access grants direct calls whatever the action's own conditions are, and whatever the conditions of any widget that also triggers it.
3. **Declared only, never generated.** `ViewImpl.resolve()` does not add action accesses, and `generate` has no effect on them.
4. **No conditions on any access entry.**

### Decisions proposed in this plan (need Mike's confirmation)

5. **Matched by class (resource) name only**, the same rule M1 uses for requests (`ActionAvailability.isRequested`). The attribute is `className`, as on `<action>`.
6. **Kept out of the user access model.** An action access does not become a `UserAccess`, is not added to the view's `accesses` set, and never reaches `AccessProcessor` or the user's access map. `canExecuteAction` already covers the role side. The declaration is a statement about one view, so the check reads it from that view.
7. **Independent of `UtilImpl.ACCESS_CONTROL`.** M1 is enforced whether access control is on or not, so its opt-out must work either way. Today `ViewImpl.accesses` is only partly filled when access control is off, which is another reason to store action accesses separately.
8. **Covers custom actions and the class-based BizExport, BizImport, Download and Upload.** It does not cover built-in OK, Save and Delete, which are requested by implicit name (`check(..., ImplicitActionName)`). The servlet branch that sends a custom action whose class is `OK` or `Save` uses the class-name `check`, so a declared access covers that case too.
9. **No `<action>` declaration is required.** The server resolves the class on the document whether or not a view declares it (M1 plan, "SmartClient (and Flutter): no check"). Validation checks that the class exists for the document. An existing `<action>` for the class keeps governing its own toolbar button.
10. **Row requests are unaffected.** `checkRow` serves only `_g` requests, which come from a row link, never from code. The access is checked in the form-level `check` only.
11. **`<uxui>` children are honoured.** An action access with `<uxui name="desktop"/>` applies only when the request's UX/UI is `desktop`. With no `<uxui>` children it applies to every UX/UI, as other accesses do. See [Related defects](#related-defects-found): data accesses in views currently ignore `<uxui>`.
12. **Component fragments count.** An action access declared in a `<component>`'s view applies to the host view that includes it, with the class name unchanged. The request resolves the class against the host document, as it does for the component's buttons today. If the fragment's document differs from the host's and the host document has no such class, the existing lookup fails as it would without the access.

### Work items

- **W1** Metadata: `ViewActionUserAccessMetaData`, its place in `ViewUserAccessesMetaData`, and the regenerated XSD.
- **W2** `ViewImpl`: hold the declared action accesses and answer whether one applies, including component fragments.
- **W3** Validation: required attribute, duplicates, `<uxui>` names, class existence.
- **W4** `ActionAvailability.check` consults the declared accesses.
- **W5** Fluent API: `FluentViewActionAccess` and `FluentView` add, find and remove.
- **W6** Kitchen-sink fixture and H2 tests through the real servlets; update the every-view report.
- **W7** Documentation.

## Affected modules

| Module | Why |
|---|---|
| `skyve-core` | `ViewActionUserAccessMetaData` (`org.skyve.impl.metadata.repository.view.access`), `ViewUserAccessesMetaData`, `ViewImpl`, `ActionAvailability`, `ViewValidator` or `LocalDesignRepository.validateViewForGenerateDomain`, `FluentViewActionAccess` and `FluentView`, `src/test/resources/schemas/view.xsd` |
| `skyve-war` | `src/main/java/schemas/view.xsd`, the kitchen-sink `ActionsFixture` view and a new action class, H2 tests, `ActionAvailabilityAllViewsH2Test` |
| `skyve-web` | None expected. Every call site already goes through `ActionAvailability.check`. |
| `skyve-ext` | None. |

## Dependencies

- M1 is on `master` (`ff745a2974`, follow-up `a93fda4a32`).
- Work locally in the shared checkout, as for M1, and keep a manifest of touched files. Other sessions edit the same tree.
- Build and test in the rsync'd mirror, as for M1 (VS Code's compiler clobbers `target/classes`).

## Backward compatibility

- **Metadata format: additive.** One new optional element. Existing views are unchanged.
- **Behaviour: relaxes M1 only where a view opts in.** No request refused today is allowed unless a view declares an access for its class.
- **`<accesses>` requires `generate`.** The XSD makes `generate` required. A view that adds `<accesses>` only to declare an action access must write `generate="true"`, or it switches off generated data accesses for that view. The docs must say this prominently.
- **Public API.** `ViewActionUserAccessMetaData` is in an `impl` package. `FluentViewActionAccess` is new public fluent API.

## Current state observed

Read on `master` at `6f9c68b3f2` on 2026-10-26.

- **Access metadata.** `ViewUserAccessesMetaData.getAccesses()` is a `List<ViewUserAccessMetaData>` bound through `@XmlElementRefs`, one ref per subtype. The abstract base declares `validate(String, Module)` and `toUserAccess(String, String)`, and holds the `<uxui>` children.
- **Conversion.** `MutableCachedRepository` (about line 710) calls `ViewImpl.convertAccesses(module, documentName, metaDataName, accessesMetaData)`. It validates each entry, validates its `<uxui>` names, and adds `toUserAccess(...)` to `ViewImpl.accesses`, refusing duplicates.
- **Generation.** `ViewImpl.resolve()` (about line 662) creates `accesses` when `ACCESS_CONTROL && generate` and adds generated accesses with a `NoOpViewVisitor`. It records `<component>`s in `components` without recursing.
- **Fragments.** `ViewImpl.getAccesses()` (about line 523) recurses into `components` with `component.getFragment(customer, uxui)` and translates fragment accesses into the host document.
- **Second-pass validation.** `LocalDesignRepository.validateViewForGenerateDomain` (about line 1085) runs `ViewValidator` and then checks model, dynamic image, binding and report accesses. `ViewValidator.validateClassAction(resourceName)` (about line 2148) checks that an action class exists for the document.
- **The check.** `ActionAvailability.check(user, document, view, uxui, bean, actionName)` builds the visitor and calls `visitAndRefuseUnlessReachable`. Call sites: `SmartClientEditServlet` (about lines 449, 1202 and 1233), `BizportExportServlet` (107), `BizportImportView` (187) and `ContentUploadView` (639). None of them consults accesses.
- **Fixture.** `kitchensink.ActionsFixture` already has `RunWithoutTrigger` (`invisible="true"`), refused by `ActionsFixtureSmartClientH2Test.anActionWithNoTriggerIsRefused`. The H2 tests switch `ACCESS_CONTROL` off in `@BeforeEach`.
- **XSD.** Generated from JAXB by `XMLMetaData.main`. Two committed copies: `skyve-war/src/main/java/schemas/view.xsd` and `skyve-core/src/test/resources/schemas/view.xsd`. `MetadataBoilerplateEscapingTest` reads both.

## Implementation plan

Make each work item its own commit, in this order.

### W1: metadata and XSD

- Add `ViewActionUserAccessMetaData extends ViewUserAccessMetaData`, `@XmlRootElement(name = "actionAccess")`, with one required attribute `className` (processed with `UtilImpl.processStringValue`).
  - `validate(metaDataName, module)`: throw `MetaDataException` when `className` is null, in the style of `ViewReportUserAccessMetaData`.
  - `toUserAccess(...)`: an action access has no `UserAccess` (decision 6). Throw `IllegalStateException` with a clear message, and make sure no caller reaches it (W2). Do not add a `UserAccess` type.
- Add the class to `@XmlElementRefs` in `ViewUserAccessesMetaData`.
- Regenerate the XSD with `XMLMetaData.main` and copy `view.xsd` to both committed locations. Check that the diff contains only the new element and type.
- Javadoc to `docs/ai/javadoc-standards.md`.

### W2: `ViewImpl`

- Add a field `Map<String, Set<String>> actionAccesses`: class name to UX/UIs, using `UserAccess.ALL_UX_UIS` when no `<uxui>` is given. Fill it in `convertAccesses`. Branch on `ViewActionUserAccessMetaData` before `toUserAccess` is called, including in the error messages that call it, so an action access never reaches `accesses`.
  - Refuse a duplicate class in one view with the existing "Duplicate user access" message style.
  - Fill it whatever `ACCESS_CONTROL` and `generate` are (decision 7).
- Add `public boolean isActionAccessDeclared(@Nonnull CustomerImpl customer, @Nonnull String uxui, @Nonnull String className)`.
  - True when this view declares the class for `uxui` (or for all UX/UIs).
  - Otherwise recurse into `components` with `component.getFragment(customer, uxui)`, as `getAccesses` does (decision 12).
  - `components` is filled by `resolve()`. Confirm that every view reaching `ActionAvailability` has been resolved; the M1 visitor already depends on that for fragments.
- Unit tests in `ViewImpl`'s existing test class or a new `ViewImplActionAccessTest`: declared, not declared, UX/UI scoped, duplicate refused, fragment declared, fragment of a fragment, access control off, `generate="false"`.

### W3: validation

- In `ViewValidator` (it already has `validateClassAction`), validate each declared action access: the class must exist for the view's document. Reuse `validateClassAction`, and name the view and the `actionAccess` in the message.
- `<uxui>` names are validated by `convertAccesses` as for other accesses.
- Do not require a matching `<action>` (decision 9).
- Tests: a missing class fails domain generation's second pass with a message naming the view and class.

### W4: `ActionAvailability`

- In `check(user, document, view, uxui, bean, actionName)` only (decisions 8 and 10): before building the visitor, return when `((ViewImpl) view).isActionAccessDeclared(customer, uxui, actionName)`.
  - No walk, no condition evaluation, no log line.
  - Consider a debug-level log naming the view and class, to help when a direct call is allowed unexpectedly.
- Leave `check(..., ImplicitActionName)` and `checkRow` unchanged.
- Update the class Javadoc: the trigger list gains "or the view (or one of its component fragments) declares an `<actionAccess>` for the class, for the request's UX/UI".
- Tests in `ActionAvailabilityTest`:
  - an action with no trigger and a declared access passes for every truth assignment;
  - an action with an `invisible` and a `disabled` condition and a declared access passes when both are true (decision 2);
  - a declared access scoped to another UX/UI does not help;
  - a declared access in a fragment helps the host;
  - a declared access does not help `checkRow`, nor a built-in requested by `ImplicitActionName`;
  - a declared access for class A does not help class B.
  Use one `@Test` with `assertAll`, never `@TestFactory` (surefire reports a failing dynamic test as a flake).

### W5: fluent API

- Add `FluentViewActionAccess` modelled on `FluentViewReportAccess`, with `className(String)` and the inherited `uxui` methods from `FluentViewUserAccess`.
- Add `addActionAccess`, `findActionAccess(String className)` and `removeActionAccess(String className)` to `FluentView`, as for the other access types.
- Tests modelled on `FluentViewReportAccessTest`, plus a round trip through XML marshalling. `XMLMetaData` attribute ordering (`02071c9a16`) may need the new element added.

### W6: fixture and H2 tests

- Add an action class `RunFromCode` to `kitchensink.ActionsFixture`, with no `<action>` entry, and declare it in the edit view: `<accesses generate="true"><actionAccess className="RunFromCode"/></accesses>`.
- Add a second case for decision 2: an `<action className="RunConditionalFromCode" inActionPanel="false" disabled="toolbarActionDisabled"/>` that is also declared as an action access. It runs whether `disableToolbarAction` is on or off.
- Update the view's blurb to describe both.
- `ActionsFixtureSmartClientH2Test`:
  - `anActionDeclaredAsAnAccessRunsWithoutATrigger`;
  - `anActionDeclaredAsAnAccessRunsWhileItsOwnConditionIsTrue`;
  - keep `anActionWithNoTriggerIsRefused` passing, which shows the access is per class.
- PrimeFaces: the only PrimeFaces paths that take an action name are the upload and BizPort import pages. Add one case to `ActionsFixtureBizPortAndUploadH2Test` only if it costs little, for example a declared access for `ActionsUpload` while `uploadDisabled` is true. Otherwise the core tests cover it.
- `ActionAvailabilityAllViewsH2Test`: list "actions reachable only through a declared action access" in their own section of the report, and stop listing them under "no trigger for any bean state".
- Regenerate the fixture's domain only if the document changes (it should not).

### W7: documentation

- `docs/server-side-action-availability.md`:
  - Upgrading: replace "There is no metadata opt-out" with the `<actionAccess>` migration, keeping the advice that a real trigger is better where one makes sense.
  - What counts as a trigger: add the declared access.
  - Say plainly that `<accesses>` needs `generate="true"` to keep generated data accesses.
  - Explain that conditions on an action declared as an access govern only its button.
- `docs/skyve-10-changes.md`: one line under Server-side Action Availability.
- Note for the external developer guide (view `<accesses>` page): the new element, outside this repo.

## Acceptance checklist

- [ ] A view can declare `<actionAccess className="X"/>`, and it validates against `view.xsd` in both copies.
- [ ] A direct request for X is allowed with no trigger, and with the action's own conditions true.
- [ ] A request for any other class is checked exactly as before.
- [ ] `checkRow` and built-in OK, Save and Delete behave exactly as before.
- [ ] `<uxui>` scoping, component fragments, access control off and `generate="false"` behave as decided.
- [ ] A missing class fails domain generation with a clear message.
- [ ] No action access appears in `ViewImpl.getAccesses()`, `AccessProcessor` output or any user's access map.
- [ ] Fluent API round-trips the element.
- [ ] Docs updated, including the `generate="true"` warning.

## Related defects found

Found while reading the access code. Not in scope; fix separately if Mike agrees.

1. **`reportAccess` in a view becomes a model access.** `ViewReportUserAccessMetaData.toUserAccess` returns `UserAccess.modelAggregate(moduleName, documentName, reportName)` instead of `UserAccess.report(...)`. A view that declares `<reportAccess>` grants the wrong access, and `validateViewForGenerateDomain` then looks for a model named after the report, so it probably fails domain generation. No view in this repo declares one.
2. **`<uxui>` on view accesses is ignored.** `ViewImpl.convertAccesses` builds the `uxuis` set, validates it, and then drops it: `accesses.add(accessMetaData.toUserAccess(...))` takes no UX/UIs. `AccessProcessor.processView` uses only the view's overridden UX/UI. So a view-level `<uxui>` restriction on a data access has no effect. Decision 11 honours it for action accesses, which is inconsistent with data accesses until this is fixed.

## Better engineering notes

- `toUserAccess` on the abstract base forces every subtype to produce a `UserAccess`. An action access does not fit. If more non-`UserAccess` entries appear, split the base into a marker for "grants a `UserAccess`" and leave `ViewUserAccessMetaData` for the XML binding only.
- `generate` is required in the XSD but defaults to `true` in Java. Making it optional in the XSD would remove the `generate="true"` trap for views that only declare action accesses. That is a schema change worth its own decision.

## AI validation plan

- Unit tests (W2, W4, W5) and H2 tests (W6) in the mirror, with `-am` for downstream modules.
- Eclipse null analysis (ECJ batch recipe) and SonarQube on changed lines.
- JaCoCo coverage of changed lines, to `docs/ai/testing-and-coverage.md`.
- Run `ActionAvailabilityAllViewsH2Test` and diff the report against M1's: only the fixture's new actions should move.

## User validation suggestions

- Kitchen Sink → Actions Fixture in SmartClient: from the browser console, call `doAction('RunFromCode')` on the open view and check that Outcome changes. Then call `doAction('RunWithoutTrigger')` and check it is refused with the warning.
- Tick "disable toolbar action" and check that `RunConditionalFromCode` still runs from the console.
