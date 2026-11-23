# Model lookup fixture

Open **Kitchen Sink → Model Lookup Fixture** with the `kitchensink.dev` role in
both SmartClient and PrimeFaces. The shared edit view exercises:

- A form `lookupDescription` using `LookupValuesModel`, with **Lookup Description**
  and **Record ID** dropdown columns. Description is searchable by default; the ID
  explicitly enables searching with `filterable="true"`.
- An inline data-grid bound-column `lookupDescription` using that same model.
  The model belongs to `ModelLookupFixture`, while the association belongs to
  `ModelLookupRow`, so this also checks enclosing-view model resolution.
- Matching single-column form and inline-grid lookups with independent selections.
  They use the same model but omit `dropDown`; in PF the popup shows description
  values without a heading, even though the model provides multiple columns.
- A list grid displaying `LookupValuesModel` directly.
- A query list grid displaying saved fixtures, including the selected association
  and its description.

## Manual checks

1. In **Kitchen Sink → Lookup values**, create two records with descriptions
   `Alpha model lookup` and `Beta model lookup`.
2. Open **Model Lookup Fixture**. Confirm both records appear in the model list
   grid, sorted by description.
3. Open the form lookup and confirm both dropdown columns are visible. Type
   `Alpha` and select the suggestion. Clear it, then type part of the Beta record's
   ID (shown in the model list grid) and select `Beta`. Confirm the selected input
   displays the description, not the ID. In SC, also exercise the lookup's picker
   dialog and its filter.
4. Add a row to **Data-grid model lookups**. Confirm the same two columns appear
   and searching by description or ID works. Select `Alpha`, finish editing the
   row, then edit it and select `Beta`. Confirm the description remains visible
   outside the editor and that clearing/reselecting works.
5. In **Single-column model lookup (no heading)**, open the dropdown and confirm
   PF displays only descriptions, without a heading or Record ID column. Search
   for `Alpha`, select it, clear it, and select `Beta`. Repeat in the inline grid's
   **Single-column lookup (no heading)** column. Confirm these selections are
   independent of the multi-column selections.
6. Give the fixture a distinct name and save it. Refresh **Saved selections**;
   its association and description columns should display the chosen form value,
   not an ID. Reopen the saved fixture and confirm all form and grid selections persisted.
7. Repeat steps 2–6 in the other UI using the saved record.

The lookup source is the persistent `kitchensink.LookupDescription` document;
there are no generated fake IDs or automatic data inserts. Delete fixture records
before deleting referenced lookup values after testing.

View access generation supplies the model aggregate access for
`kitchensink.ModelLookupFixture.LookupValuesModel`. Normal target-document read
permissions and persistence scoping apply. For access-control regression testing,
use a restricted role to verify that removing target read permission prevents
fetching, and test denied model access with explicit view accesses and generation
disabled. The `dev` role is the positive fixture role, not a denial fixture.

`ModelLookupFixtureH2Test` checks persisted child selections, model fetching and
association-description projection for the saved selections list. Browser checks
above are still required for autocomplete, pickers and inline editing.
