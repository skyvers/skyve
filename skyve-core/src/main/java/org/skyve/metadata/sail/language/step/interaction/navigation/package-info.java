/**
 * SAIL steps for navigating to different view types within a Skyve application.
 *
 * <p>Navigation steps change the current view without requiring an established context.
 * They are often used to set up a test scenario by landing on a known starting view:
 * <ul>
 *   <li>{@link org.skyve.metadata.sail.language.step.interaction.navigation.NavigateEdit} —
 *       navigates to the edit view for a specific document instance identified by its
 *       {@code bizId}.
 *   <li>{@link org.skyve.metadata.sail.language.step.interaction.navigation.NavigateList} —
 *       navigates to the list view for a specified module and document.
 *   <li>{@link org.skyve.metadata.sail.language.step.interaction.navigation.NavigateLink} —
 *       follows a named navigation link within the current view.
 *   <li>{@link org.skyve.metadata.sail.language.step.interaction.navigation.NavigateMap} —
 *       navigates to the map view for a specified module and document.
 *   <li>{@link org.skyve.metadata.sail.language.step.interaction.navigation.NavigateTree} —
 *       navigates to the tree view for a specified module and document.
 *   <li>{@link org.skyve.metadata.sail.language.step.interaction.navigation.NavigateCalendar} —
 *       navigates to the calendar view for a specified module and document.
 * </ul>
 *
 * @see org.skyve.metadata.sail.language.step.interaction
 * @see org.skyve.metadata.sail.language.step.context
 */
package org.skyve.metadata.sail.language.step.interaction.navigation;
