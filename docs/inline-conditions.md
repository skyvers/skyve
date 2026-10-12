# Inline Conditions in Views

A view attribute that takes a condition can hold a Skyve expression directly,
instead of the name of a condition declared on the document.

```xml
<textField binding="reason" visible="{el:bean.status eq 'rejected'}" />
<button action="Approve" disabled="{role:admin.Approver}" />
<dataGrid binding="lines" enableAdd="{el:bean.open and not bean.locked}">
```

A value that starts with `{` and ends with `}` is an inline condition
expression. Anything else is a condition name, `true` or `false`, as before.

## Where It Applies

Every view attribute that is evaluated as a condition:

| Attributes | Elements |
|---|---|
| `visible` / `invisible` | containers, widgets, actions, components |
| `enabled` / `disabled` | input widgets, forms, tabs, grids, buttons, actions |
| `enableAdd`, `enableEdit`, `enableZoom`, `enableRemove` and their `disable` forms | `dataGrid`, `listGrid`, `treeGrid` |
| `enablePick`, `enableEdit`, `enableAdd`, `enableClear` and their `disable` forms | `lookupDescription` |
| `postRefresh` | `listGrid`, `listRepeater`, `treeGrid` |
| `refreshIf` | `view` |
| `enabled` / `disabled`, `visible` / `invisible` | `setDisabled` and `setInvisible` event actions |

Both attributes of a pair accept an expression. Use whichever reads better;
there is no need to negate the expression yourself.

## Developer Contract

- The expression must evaluate to a boolean. Any expression prefix that can
  yield one may be used: `el`, `rtel`, `role`, `stash`, `user`, a boolean
  binding, or a registered custom prefix.
- The result type is checked when views are validated only where it can be
  determined from metadata: an `el` expression and a binding, with the same
  rules as an expression in a document `<condition>`. A `role` expression is
  always a boolean.
- `rtel`, `stash` and `user` expressions are not type checked, because their
  result is only known when the view is rendered. The value must be a
  `Boolean` then; anything else, including the text `true`, counts as false.
  An `i18n` expression passes validation but yields text, so it is never true.
- A result of `null` counts as false. The opposite attribute of the pair is
  the strict complement, so exactly one of `visible="{X}"` and
  `invisible="{X}"` shows the widget.
- The expression is evaluated against the same bean a condition name in that
  position is evaluated against.
- Conditions inside a `<component>` are evaluated in the view that contains
  it; see [Conditions in Components](#conditions-in-components).
- View metadata keeps what was authored. A view that is unmarshalled and
  marshalled again has the same attribute and the same expression.

## Writing Expressions in XML

- `|` introduces a formatter in a Skyve expression, so `||` is not available.
  Use `or`.
- `<` and `&` must be escaped in an XML attribute. The EL word operators
  avoid that: `lt`, `le`, `gt`, `ge`, `eq`, `ne`, `and`, `or`, `not`.
- Use `'` for string literals inside a `"`-quoted attribute.

Prefer a named document condition for anything used in more than one place or
long enough to be hard to read in an attribute. A named condition is also
available to Java code and reports, which an inline expression is not.

## What Clients See

PrimeFaces evaluates the expression on the server; nothing about it is sent
to the browser.

A Jasper report generated from a view evaluates its conditions on the server
through `Bean.evaluateCondition()`, so an inline expression works there as a
condition name does.

SmartClient needs a name to correlate the view definition with the data it is
sent. An inline expression is given an identifier: the SHA-1 digest of the
expression text, written in full as 27 letters and digits starting with a
letter. The opposite sense of the same expression is `not` followed by that
identifier.

```
disabled="{X}"  ->  disabledCondition:'kT3f9Xq2AbR7mPz0LwE4sVnY8hC'
enabled="{X}"   ->  disabledCondition:'notkT3f9Xq2AbR7mPz0LwE4sVnY8hC'
```

The identifier depends only on the expression text. The same expression used
in several places shares one identifier, and one value in each response.
The server can evaluate the expression more than once while it builds a
response, because it also evaluates conditions to decide which parts of the
view to process, so an expression should be cheap and have no side effects.
The expression text itself is not sent to a SmartClient browser.

The view metadata served by `MetaDataServlet` uses the same identifier as the
condition name and adds the expression beside it:

```json
"invisibleCondition": "notkT3f9Xq2AbR7mPz0LwE4sVnY8hC",
"invisibleConditionExpression": "{X}"
```

The expression property always carries the expression as authored without
`not`; the name carries the sense.

## Conditions in Components

A `<component>` is expanded into the view that contains it, and its conditions
are evaluated there, against the containing view's bean. Every condition in a
component view must therefore be one its containing view can evaluate. This is
true of inline expressions as it is of condition names, whether or not the
component has a `binding`. What differs is what a `binding` does to each:

- A condition name is never changed by the `binding`. It must exist on the
  containing view's document, under the same name or through a
  [condition mapping](#mapping-conditions-and-actions).
- An inline expression must be valid, and yield a boolean, for the containing
  view's document. When the component has a `binding`, the expression is
  rewritten to follow it first, as the widget bindings in the component are;
  when it has none, the expression is used as written. An expression that has
  a condition mapping takes what it is mapped to instead.

With `<component binding="startup" />`:

```
visible="{flag}"             ->  {startup.flag}
visible="{el:bean.flag}"     ->  {el:bean.startup.flag}
visible="{el:bean['flag']}"  ->  {el:bean.startup['flag']}
visible="{el:empty bean}"    ->  {el:empty bean.startup}
visible="flagged"            ->  flagged, which the containing document must declare
```

Only references to the bean are rewritten: a plain binding, and the `bean`
variable in an `el` or `rtel` expression, whatever follows it. String
literals are left alone, as are other names that contain `bean`. `role`,
`stash` and `user` expressions are the same in every view and are left alone.

A component view that is used from several views has to satisfy this for each
of them. An `edit` or `create` view that is also used as a component is
validated against its own document as well, as written.

### Mapping Conditions and Actions

A component can map what its view calls a condition, or an action, onto what
the containing view calls it:

```xml
<component binding="startup" name="_details">
    <actions>
        <action fromComponent="Check" mappedTo="CheckStartup" />
    </actions>
    <conditions>
        <condition fromComponent="editable" mappedTo="canEditStartup" />
        <condition fromComponent="{el:bean.flag}" mappedTo="{el:bean.startup.flag and bean.unlocked}" />
    </conditions>
</component>
```

- `fromComponent` of a `<condition>` is a condition name, or an inline
  expression exactly as the component view writes it. The match is on the
  text, so the expression has to be the same character for character.
- `mappedTo` is a condition name or an inline expression of the containing
  view's document. It is used as it is; the `binding` is not applied to it.
- The opposite sense follows the mapping. Mapping `editable` also maps
  `notEditable`, and mapping an expression used in `visible` also covers the
  same expression used in `invisible`.
- `<actions>` maps action names only, and `<conditions>` maps conditions only.
  A name is never looked up in the other.
- `<actions>` comes before `<conditions>`.

These replace `<names>` and `<name>`, which mapped both and could not map an
expression. Move each `<name>` to `<actions>` or `<conditions>` according to
what it names.

## Java API

Because a condition attribute may now hold an expression, the view metadata
accessors end in `Condition` where they used to end in `ConditionName`. Each
condition pair keeps what was authored and derives what renderers need, and
the method name says which is which.

| Accessor | Meaning |
|---|---|
| `getXmlVisibleCondition()` / `setXmlVisibleCondition(String)` | The `visible` attribute exactly as authored. This is what is marshalled. |
| `getXmlInvisibleCondition()` / `setXmlInvisibleCondition(String)` | The `invisible` attribute exactly as authored. |
| `getInvisibleCondition()` | Derived: the authored `invisible` condition, or else the negation of the authored `visible` condition. Renderers and other consumers call this. |

The rule holds for every pair: the `Xml` prefix means "as authored", and the
un-prefixed getter of the negative sense (`getInvisibleCondition()`,
`getDisabledCondition()`, `getDisableAddCondition()`, ...) is the derived
value. Setting either attribute of a pair clears the other. `postRefresh` and
`refreshIf` have no opposite attribute, so they are plain
`getPostRefreshCondition()` / `getRefreshIfCondition()` properties.

The old methods were removed. Replace them as follows; the same mapping
applies to `Enabled`/`Disabled` and to the `Add`, `Edit`, `Zoom`, `Remove`,
`Pick` and `Clear` flags.

| Before | After |
|---|---|
| `getInvisibleConditionName()` | `getInvisibleCondition()` |
| `setInvisibleConditionName(x)` | `setXmlInvisibleCondition(x)` |
| `setVisibleConditionName(x)` | `setXmlVisibleCondition(x)` |
| `getDisableAddConditionName()` | `getDisableAddCondition()` |
| `setEnableAddConditionName(x)` | `setXmlEnableAddCondition(x)` |
| `getPostRefreshConditionName()` | `getPostRefreshCondition()` |
| `View.getRefreshConditionName()` | `View.getRefreshIfCondition()` |

Fluent builders are renamed the same way, and gain the positive sense:

| Before | After |
|---|---|
| `invisibleConditionName(x)` | `invisibleCondition(x)`, or `visibleCondition(x)` |
| `disabledConditionName(x)` | `disabledCondition(x)`, or `enabledCondition(x)` |
| `disableAddConditionName(x)` | `disableAddCondition(x)`, or `enableAddCondition(x)` |
| `postRefreshConditionName(x)` | `postRefreshCondition(x)` |
| `refreshConditionName(x)` | `refreshIfCondition(x)` |

`Component.getNames()` is replaced by `Component.getActions()` and
`Component.getConditions()`, and `ComponentViewVisitor` takes the two lists
separately. `FluentComponent` gains `addAction()`, `addCondition()`,
`clearActions()` and `clearConditions()`.

The property names sent to clients drop `Name` as well. SmartClient view
definitions and `MetaDataServlet` JSON now use `invisibleCondition`,
`disabledCondition`, `disableAddCondition`, `postRefreshCondition`,
`refreshIfCondition` and so on, where they used `invisibleConditionName` etc.
They keep the `Condition` suffix because SmartClient widgets already have
properties of their own called `disabled` and `visible`. Custom client code
that reads the old property names needs the same change.

## Limitations

- In a component with a `binding`, a lambda parameter called `bean` in an
  `el` expression is rewritten as if it were the `bean` variable. Call the
  parameter something else.
- A condition on a widget inside a `dataGrid` or `dataRepeater` is evaluated
  against the view's bean, not the row, and is validated against the view's
  document. This is true of condition names too. The exception is a `link` to
  an action inside a data widget in PrimeFaces: the action comes from the row
  document's own view, so its conditions are evaluated against the row.
- The React and Flutter client generators pass conditions through as names
  and do not support inline conditions.
- Views of dynamic documents: the value sent to SmartClient is evaluated, but
  server-side checks that skip hidden or disabled widgets treat every
  condition on a dynamic bean as false, as they already do for condition
  names.
