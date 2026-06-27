/**
 * SAIL steps that model direct user interactions with the current view.
 *
 * <p>Interaction steps operate within the context established by a
 * {@link org.skyve.metadata.sail.language.step.context.PushEditContext} or
 * {@link org.skyve.metadata.sail.language.step.context.PushListContext}. They
 * represent concrete user gestures:
 * <ul>
 *   <li>{@link org.skyve.metadata.sail.language.step.interaction.DataEnter} — types a
 *       value into the widget bound to a specified model binding.
 *   <li>{@link org.skyve.metadata.sail.language.step.interaction.TestDataEnter} — populates
 *       the view using a randomly generated test fixture, useful for smoke testing.
 *   <li>{@link org.skyve.metadata.sail.language.step.interaction.TabSelect} — selects a
 *       named tab in the current view, supporting nested paths with {@code /} separators.
 * </ul>
 *
 * <p>Sub-packages organise interactions by widget category:
 * <ul>
 *   <li>{@link org.skyve.metadata.sail.language.step.interaction.actions} — implicit and
 *       explicit action buttons (Save, Cancel, Delete, ZoomIn, ZoomOut, Ok, Remove).
 *   <li>{@link org.skyve.metadata.sail.language.step.interaction.grids} — data-grid and
 *       list-grid row operations (new, select, edit, remove, zoom).
 *   <li>{@link org.skyve.metadata.sail.language.step.interaction.lookup} — lookup-description
 *       widget operations (auto-complete, pick, edit, new).
 *   <li>{@link org.skyve.metadata.sail.language.step.interaction.navigation} — view navigation
 *       (navigate to edit, list, map, tree, calendar, link).
 *   <li>{@link org.skyve.metadata.sail.language.step.interaction.session} — session lifecycle
 *       (login, logout).
 * </ul>
 *
 * @see org.skyve.metadata.sail.language.step
 * @see org.skyve.metadata.sail.language.step.context
 */
package org.skyve.metadata.sail.language.step.interaction;
