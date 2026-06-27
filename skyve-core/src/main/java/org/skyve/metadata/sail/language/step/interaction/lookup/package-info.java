/**
 * SAIL steps for interacting with Skyve lookup-description widgets.
 *
 * <p>A lookup-description widget allows users to search for and select a related
 * document instance. This package covers the four interaction modes that the widget
 * supports:
 * <ul>
 *   <li>{@link org.skyve.metadata.sail.language.step.interaction.lookup.LookupDescriptionAutoComplete} —
 *       types a search string into the lookup field and selects the first match from
 *       the auto-complete dropdown.
 *   <li>{@link org.skyve.metadata.sail.language.step.interaction.lookup.LookupDescriptionPick} —
 *       opens the lookup picker dialog and selects the row at the specified zero-based index.
 *   <li>{@link org.skyve.metadata.sail.language.step.interaction.lookup.LookupDescriptionEdit} —
 *       zooms into the currently selected related instance for editing.
 *   <li>{@link org.skyve.metadata.sail.language.step.interaction.lookup.LookupDescriptionNew} —
 *       opens a new-instance form for the related document type.
 * </ul>
 *
 * @see org.skyve.metadata.sail.language.step.interaction
 */
package org.skyve.metadata.sail.language.step.interaction.lookup;
