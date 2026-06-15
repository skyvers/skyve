/**
 * Client-side event sources and server-side event actions for the view metadata model.
 *
 * <p>Event sources ({@code Changeable}, {@code Focusable}, {@code Addable},
 * {@code Removable}, {@code Selectable}, {@code Editable}) are mix-in interfaces
 * applied to widgets to declare what events they can fire.
 *
 * <p>Event actions ({@code EventAction} and its subtypes) describe what the server
 * should do when an event fires:
 * <ul>
 *   <li>{@code ServerSideActionEventAction} — invokes a named action class.
 *   <li>{@code RerenderEventAction} — re-renders the view with or without saving.
 *   <li>{@code SetDisabledEventAction} / {@code ToggleDisabledEventAction} — disables
 *       or toggles the disabled state of a binding.
 *   <li>{@code SetInvisibleEventAction} / {@code ToggleVisibilityEventAction} — hides
 *       or toggles visibility of a binding.
 * </ul>
 *
 * @see org.skyve.metadata.view.widget
 */
package org.skyve.impl.metadata.view.event;
