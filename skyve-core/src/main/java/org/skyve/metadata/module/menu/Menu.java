package org.skyve.metadata.module.menu;

import java.util.List;

import org.skyve.metadata.DecoratedMetaData;

/**
 * Represents the navigational menu for a Skyve module.
 *
 * <p>A {@code Menu} is the top-level container of navigable {@link MenuItem}s for a
 * {@link org.skyve.metadata.module.Module}. The menu structure is declared in module
 * metadata XML and may be filtered at runtime by UX/UI name to support different
 * interface variants (e.g. desktop, mobile, API-only).
 *
 * <p>The items returned by {@link #getItems()} may include leaf items (actions or
 * links) and {@link MenuGroup} composites, forming a tree that UI renderers traverse
 * using {@link MenuRenderer}.
 *
 * <p>Access control is enforced at the {@link MenuItem} level via role names; this
 * interface does not itself carry role restrictions.
 *
 * @see MenuGroup
 * @see MenuItem
 * @see MenuRenderer
 */
public interface Menu extends DecoratedMetaData {
	/**
	 * Returns the ordered list of top-level items in this menu.
	 *
	 * <p>Items may be leaf {@link MenuItem}s or {@link MenuGroup} composites.
	 * The list reflects the declaration order from module metadata.
	 *
	 * @return a mutable ordered list of top-level menu items; never {@code null}
	 */
	public List<MenuItem> getItems();

	/**
	 * Returns {@code true} if this menu should be rendered for the given UX/UI variant.
	 *
	 * <p>A menu is applicable when its UX/UI restriction list is empty (all variants
	 * permitted) or contains {@code uxui}.
	 *
	 * @param uxui  the UX/UI name to test; may be {@code null} to indicate no variant filter
	 * @return {@code true} if this menu is visible under the given UX/UI
	 */
	public boolean isApplicable(String uxui);
}
