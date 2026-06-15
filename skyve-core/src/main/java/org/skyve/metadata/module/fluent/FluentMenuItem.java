package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.GrantedTo;
import org.skyve.impl.metadata.repository.module.ItemMetaData;
import org.skyve.metadata.module.menu.MenuItem;

/**
 * Base fluent builder for role-constrained menu items.
 */
public abstract class FluentMenuItem<T extends FluentMenuItem<T>> extends FluentMenuAction<T> {
	/**
	 * Creates an empty fluent menu item wrapper.
	 */
	protected FluentMenuItem() {
		// nothing to see
	}
	
	/**
	 * Copies menu item state including role grants from an existing menu item.
	 *
	 * @param item The source menu item.
	 * @return this fluent instance.
	 */
	@Override
	@SuppressWarnings("unchecked")
	protected T from(MenuItem item) {
		super.from(item);
		item.getRoleNames().forEach(this::addRole);
		return (T) this;
	}
	
	/**
	 * Grants visibility to a role.
	 *
	 * @param roleName The role name to grant.
	 * @return this fluent instance.
	 */
	@SuppressWarnings("unchecked")
	public T addRole(String roleName) {
		GrantedTo role = new GrantedTo();
		role.setRoleName(roleName);
		get().getRoles().add(role);
		return (T) this;
	}

	/**
	 * Revokes visibility for a role.
	 *
	 * @param name The role name to revoke.
	 * @return this fluent instance.
	 */
	@SuppressWarnings("unchecked")
	public T removeRole(String name) {
		get().getRoles().removeIf(r -> name.equals(r.getRoleName()));
		return (T) this;
	}

	/**
	 * Clears all role grants from the menu item.
	 *
	 * @return this fluent instance.
	 */
	@SuppressWarnings("unchecked")
	public T clearRoles() {
		get().getRoles().clear();
		return (T) this;
	}

	/**
	 * Returns the mutable metadata backing this fluent wrapper.
	 *
	 * @return The item metadata instance.
	 */
	@Override
	public abstract ItemMetaData get();
}
