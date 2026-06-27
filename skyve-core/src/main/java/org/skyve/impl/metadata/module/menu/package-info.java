/**
 * Internal implementations of the {@link org.skyve.metadata.module.menu} menu item
 * contracts.
 *
 * <p>The classes in this package are JAXB-annotated implementations of the
 * {@link org.skyve.metadata.module.menu.MenuItem} hierarchy. Each class corresponds
 * to one menu item type that can appear in a module's {@code <menu>} element:
 * <ul>
 *   <li>{@code MenuImpl} — the root container; holds a list of
 *       {@link org.skyve.metadata.module.menu.MenuItem} entries.
 *   <li>{@code MenuGroupImpl} — a named folder/group of menu items.
 *   <li>{@code ListItem} — navigates to a document list view.
 *   <li>{@code EditItem} — navigates directly to the edit view of a document (new
 *       instance or singleton).
 *   <li>{@code CalendarItem} — navigates to a calendar view.
 *   <li>{@code MapItem} — navigates to a map view.
 *   <li>{@code TreeItem} — navigates to a tree view.
 *   <li>{@code LinkItem} — navigates to an arbitrary URL.
 *   <li>{@code AbstractMenuItem} — base carrying role visibility, icon, and tooltip.
 *   <li>{@code AbstractDocumentMenuItem} — extends {@code AbstractMenuItem} with
 *       module/document targeting.
 *   <li>{@code AbstractDocumentOrQueryOrModelMenuItem} — extends with optional query
 *       and model overrides.
 * </ul>
 *
 * @see org.skyve.metadata.module.menu
 */
package org.skyve.impl.metadata.module.menu;
