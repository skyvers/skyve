package org.skyve.metadata.user;

import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.NamedMetaData;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;

/**
 * A named role declared within a Skyve module, grouping a set of document permissions.
 *
 * <p>Roles are the primary mechanism for access control in Skyve. Each role is declared
 * in module XML metadata and grants specific {@link DocumentPermission}s to the documents
 * within (and potentially outside) the owning module. Users are assigned one or more
 * roles at the customer level; the effective permission for a document is the union of
 * all assigned role permissions.
 *
 * @see DocumentPermission
 * @see DocumentPermissionScope
 * @see User
 */
public interface Role extends NamedMetaData, DecoratedMetaData {
	/**
	 * Returns the human-readable description of this role.
	 *
	 * <p>For a localised version use {@link #getLocalisedDescription()}.
	 *
	 * @return the role description; may be {@code null} if none is specified
	 */
	public String getDescription();
	
	/**
	 * Returns the localised description of this role for the current user locale.
	 *
	 * @return a non-{@code null} localised description; falls back to the raw key
	 */
	public default String getLocalisedDescription() {
		return Util.i18n(getDescription());
	}
	
	/**
	 * Returns the module that declares this role.
	 *
	 * @return the owning module; never {@code null}
	 */
	public Module getOwningModule();
	
	/**
	 * Returns optional long-form documentation for this role.
	 *
	 * <p>Documentation is free-form prose intended for developer or administrator
	 * reference describing the intended audience and purpose of the role.
	 *
	 * @return the documentation string, or {@code null} if none is specified
	 */
	public String getDocumentation();
}
