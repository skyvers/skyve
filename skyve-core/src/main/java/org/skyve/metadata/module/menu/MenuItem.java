package org.skyve.metadata.module.menu;

import java.util.Set;

import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.NamedMetaData;
import org.skyve.util.Util;

/**
 * A single navigable element within a Skyve module menu.
 *
 * <p>A {@code MenuItem} has a programmatic name (from {@link org.skyve.metadata.NamedMetaData})
 * and optional restrictions that control which roles and UX/UI variants can see it.
 * Concrete subtypes include list views, edit views, calendar views, map views,
 * tree views, external links, and composite {@link MenuGroup}s.
 *
 * <p>Access control is applied at two orthogonal levels:
 * <ul>
 *   <li><strong>Role-based:</strong> {@link #getRoleNames()} lists the module roles
 *       permitted to see this item. An empty set means all roles may see it.</li>
 *   <li><strong>UX/UI-based:</strong> {@link #getUxUis()} lists the UX/UI variants
 *       that expose this item. An empty set means all variants expose it.</li>
 * </ul>
 *
 * <p>Use {@link #isApplicable(String)} to test whether the item should appear for a
 * given UX/UI name; role filtering is performed separately by the security layer.
 *
 * @see MenuGroup
 * @see Menu
 * @see MenuRenderer
 */
public interface MenuItem extends NamedMetaData, DecoratedMetaData {
	/**
	 * Returns the display name of this menu item, localised for the current user locale.
	 *
	 * <p>Delegates to {@link Util#i18n(String)} using {@link #getName()} as the
	 * resource key. If no translation is found the key itself is returned as a fallback.
	 *
	 * @return a non-{@code null} localised display name
	 */
	public default String getLocalisedName() {
		return Util.i18n(getName());
	}
	
	/**
	 * Returns the set of module role names that are permitted to see this menu item.
	 *
	 * <p>An empty set means the item is visible to all roles. Role names are
	 * unqualified (no module prefix) and must match roles declared in the owning module.
	 *
	 * @return a non-{@code null} set of permitted role names; empty means unrestricted
	 */
	public Set<String> getRoleNames();
	
	/**
	 * Returns the set of UX/UI variant names for which this item is visible.
	 *
	 * <p>An empty set means the item appears in all UX/UI variants. UX/UI names
	 * correspond to the keys defined in the customer UX/UI configuration.
	 *
	 * @return a non-{@code null} set of permitted UX/UI names; empty means unrestricted
	 */
	public Set<String> getUxUis();
	
	/**
	 * Returns {@code true} if this menu item should be rendered for the given UX/UI variant.
	 *
	 * <p>Returns {@code true} when the UX/UI set is empty (unrestricted) or contains
	 * {@code uxui}.
	 *
	 * @param uxui  the UX/UI name to test; may be {@code null} to bypass UX/UI filtering
	 * @return {@code true} if this item is applicable for the given UX/UI
	 */
	public boolean isApplicable(String uxui);
}
