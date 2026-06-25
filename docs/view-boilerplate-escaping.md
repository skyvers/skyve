# View Boilerplate Escaping

Skyve 10 adds metadata-controlled escaping flags for view boilerplate text, such
as titles, labels, help text, tooltips, confirmations, headings, and headers.

The default is safe rendering: missing flags and `true` values escape metadata
text before it reaches the renderer. Set the paired `escapeX` flag to `false`
only when the metadata value is trusted markup that should be rendered as HTML.
Renderer-specific syntax escaping still applies after that decision, so
`escapeX="false"` does not disable JavaScript, JSON, or HTML attribute escaping
needed to keep the generated output syntactically valid.

## Developer Contract

- Metadata values are stored raw.
- Expressions, localisation, and formatters are resolved before escaping.
- The final renderer boundary applies the matching `escapeX` decision.
- Framework-owned markup, icons, required markers, wrappers, and generated
  anchors remain framework markup; only the metadata text is controlled by the
  paired flag.
- Existing widget-level `escape` and `sanitise` behaviour for labels, blurbs,
  rich text, HTML, and grid cell values is unchanged.

Use `escapeX="false"` only with trusted metadata. If intentional markup contains
dynamic values, escape those values inside the expression/formatter before
opting the surrounding boilerplate text out of renderer HTML escaping.

## Supported Fields

| Metadata field | Escape flag |
|---|---|
| `view/@title` | `escapeTitle` |
| `tab/@title` | `escapeTitle` |
| `vbox/@borderTitle` | `escapeBorderTitle` |
| `hbox/@borderTitle` | `escapeBorderTitle` |
| `form/@borderTitle` | `escapeBorderTitle` |
| `formItem/@label` | `escapeLabel` |
| `formItem/@requiredMessage` | `escapeRequiredMessage` |
| `formItem/@help` | `escapeHelp` |
| `abstractDataWidget/@title` | `escapeTitle` |
| `abstractListWidget/@title` | `escapeTitle` |
| `dataGridColumn/@title` | `escapeTitle` |
| `abstractAction/@displayName` | `escapeDisplayName` |
| `abstractAction/@toolTip` | `escapeToolTip` |
| `abstractAction/@confirm` | `escapeConfirm` |
| `zoomIn/@displayName` | `escapeDisplayName` |
| `zoomIn/@toolTip` | `escapeToolTip` |
| `dialogButton/@displayName` | `escapeDisplayName` |
| `listMembership/@membersHeading` | `escapeMembersHeading` |
| `listMembership/@candidatesHeading` | `escapeCandidatesHeading` |
| `link/@value` | `escapeValue` |

The same nullable Boolean flags are available on the runtime metadata classes
and fluent builders. View actions also expose `getEscapeDisplayName()`,
`getEscapeToolTip()`, and `getEscapeConfirm()` on the public `Action` API as
default methods, so existing `Action` implementations remain compatible.

## Renderer Behaviour

SmartClient and PrimeFaces both apply these flags where the supported metadata
fields render in server-generated UI output. Missing and explicit `true` values
HTML-escape first, then apply the renderer's required JavaScript, JSON, or
component/attribute syntax escaping. Explicit `false` skips only the HTML
escaping of the metadata text.

Important details:

- Required markers such as `&nbsp;*:` are appended after escaping the label
  text.
- PrimeFaces help text rendered into `data-tooltip` still receives
  attribute-context escaping even when `escapeHelp="false"`.
- PrimeFaces confirmation messages keep the existing confirm behaviour, but
  metadata confirmation text is pre-escaped unless `escapeConfirm="false"`.
- Attribute-derived labels, help, and required messages do not inherit
  `formItem` escape flags when the paired form-item text is absent; they keep
  escaped-by-default behaviour.
- Query/list-model column titles that do not have these XML flags are escaped by
  default and have no metadata opt-out.

## JSON And Vue

Most metadata JSON output remains JSON-only escaped because the audited
consumers render the values as text. In particular, Vue list-grid titles,
column headers, and enum labels remain raw JSON values and rely on
Vue/PrimeVue text rendering.

The implemented exception is SmartClient link format snippets:
`link/@escapeValue` is applied to the link display text before Skyve inserts it
into the generated `<a>...</a>` HTML snippet that is then JSON-marshalled.

Chart model `title` and `label` are not covered by this change.

## Examples

Escaped by default:

```xml
<formItem binding="description" label="Description &amp; Notes" />
```

Trusted markup:

```xml
<tab title="&lt;strong&gt;Summary&lt;/strong&gt;" escapeTitle="false" />
```

Trusted markup with an escaped expression value:

```xml
<tab title="&lt;strong&gt;{name|EscapedHTML}&lt;/strong&gt;" escapeTitle="false" />
```

Action confirmation with trusted markup:

```xml
<action name="Archive"
		displayName="Archive"
		confirm="&lt;strong&gt;Archive this record?&lt;/strong&gt;"
		escapeConfirm="false" />
```
