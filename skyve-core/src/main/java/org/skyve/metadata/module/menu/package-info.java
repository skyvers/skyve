/**
 * Defines the navigational menu model for Skyve modules.
 *
 * <p>The menu system models module navigation as a tree of {@link org.skyve.metadata.module.menu.MenuItem}s.
 * Leaf nodes represent direct navigation targets (list views, edit views, calendars, maps, trees,
 * and external links); interior nodes are {@link org.skyve.metadata.module.menu.MenuGroup} composites.
 * The full tree for a module is accessed via {@link org.skyve.metadata.module.Module#getMenu()}.
 *
 * <p>Rendering is handled by extending {@link org.skyve.metadata.module.menu.MenuRenderer} and
 * overriding the typed callback methods. The renderer drives a depth-first traversal and applies
 * UX/UI and role filtering automatically.
 *
 * <p>Menu structures are declared in module XML metadata and loaded by the metadata repository;
 * they are not modified at runtime.
 *
 * @see org.skyve.metadata.module.menu.Menu
 * @see org.skyve.metadata.module.menu.MenuItem
 * @see org.skyve.metadata.module.menu.MenuGroup
 * @see org.skyve.metadata.module.menu.MenuRenderer
 */
package org.skyve.metadata.module.menu;
