package org.skyve.metadata.module.menu;

/**
 * A composite menu node that acts simultaneously as a {@link Menu} (contains child
 * {@link MenuItem}s) and as a {@link MenuItem} itself (can be nested within another menu).
 *
 * <p>Implements the <em>Composite</em> role of the GoF Composite pattern: a
 * {@code MenuGroup} may be placed anywhere a {@link MenuItem} is expected, and its
 * {@link Menu#getItems()} method exposes the sub-tree beneath it.
 *
 * <p>Menu groups typically render as expandable/collapsible sections in a navigation
 * sidebar. A group has a name (from {@link org.skyve.metadata.NamedMetaData}) and
 * optional role and UX/UI restrictions (from {@link MenuItem}); child items carry
 * their own independent restrictions.
 *
 * @see Menu
 * @see MenuItem
 * @see MenuRenderer#renderMenuGroup
 */
public interface MenuGroup extends Menu, MenuItem {
	// nothing to see here
}
