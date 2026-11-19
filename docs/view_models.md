ViewModel interface extends MetaData and adds postConstruct(Customer customer, boolean runtime).
ChartModel, ComparisonModel, ListModel and MapModel implement ViewModel.

Domain Generation instantiates the ViewModels and calls postConstruct method with the runtime argument of false.
Access Control does the same.
For the above 2 use cases, the models do not have their dependencies injected via CDI and the runtime argument can be used to conditionally control the use of Skyve services from CORE and EXT, like Persistence etc.

Any Skyve function that uses a ViewModel also calls postConstruct method with the runtime argument of true.

If ListModel.getDrivingDocument() yields null then Domain Generation can't validate column bindings and parameter bindings and access control list items cannot be added.
The null is allowed to cater for late-resolved driving documents in dynamic list models in tandem with the <s:view/> dynamic attribute. 


## Lookup descriptions backed by list models

A lookup can use a document-owned list model instead of a metadata query:

```xml
<lookupDescription binding="contact" descriptionBinding="name" model="ContactsModel"/>
```

The model belongs to the document owning the enclosing view, just as it does for
`listGrid`. For an `Order` view in the `sales` module, this resolves
`modules/sales/Order/models/ContactsModel.java`, including customer overrides.
It also applies to lookups inside data grids: the model belongs to the enclosing
view document, not the grid row document.

`query` and `model` are mutually exclusive. Omitting both retains the existing
reference-query/default-query resolution. A lookup model must declare its driving
document during metadata validation; it must match the lookup relation's target
document. The description and drop-down columns must be available in the model's
columns, with projected columns enabled (`bizKey` is implicit). Normal filter
parameters and model parameters are supported. At fetch time the model receives
the current view bean through `setBean()`.

Faces autocomplete and SmartClient autocomplete/pick lists use the same model
fetch paths as list grids. Generated view accesses include
`UserAccess.modelAggregate(module, enclosingDocument, model)` instead of the
lookup's query/document aggregate access. Existing singular access for lookup
navigation is retained. When access generation is disabled, declare the model
aggregate access explicitly using the existing view/module model access metadata.
Fetching still requires model access for the active UX/UI and read permission on
the driving document; normal persistence scoping continues to apply to models
using Skyve persistence. Custom models remain responsible for the data they expose,
as with list grids.

## Lookup dropdown columns in PrimeFaces and SmartClient

Both renderers display the projected columns selected by a lookup's `dropDown`
definitions, in query/model column order. Faces renders these as a table in the
autocomplete popup, using the query column titles, formatting, widths, alignment,
and escaping settings. The selected input still displays `descriptionBinding`
(or `bizKey` by default).

Typing searches eligible dropdown columns with OR semantics; the search is ANDed
with existing filter parameters before fetching the result page. A dropdown
column with `filterable="false"` is excluded; `filterable="true"` explicitly
includes it. When omitted, filterable text columns and variant-domain columns are
searched automatically, while numeric, constant-domain, and dynamic-domain
columns are excluded. If no columns qualify, the description field is searched.
Lookups without dropdown columns display only the description values
in a plain autocomplete list.
