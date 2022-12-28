package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.ModuleRoleUserAccessMetaData;
import org.skyve.impl.metadata.repository.module.ModuleRoleUserAccessUxUiMetadata;

/**
 * Abstract base fluent builder class for working with {@link FluentModuleRoleAccess}.
 * 
 * @author benpetito
 *
 * @param <T> The fluent module role access builder subtype
 * @param <M> The module role access metadata subtype
 */
abstract class FluentModuleRoleAccess<T extends FluentModuleRoleAccess<T, M>, M extends ModuleRoleUserAccessMetaData> {
	/**
	 * The underlying metadata object
	 */
	protected M access;

	protected FluentModuleRoleAccess() {
		// no implementation
	}

	/**
	 * Returns the underlying module role access metadata from this builder.
	 */
	public abstract M get();
	
	/**
	 * Add a new ModuleRoleUserAccessUxUiMetadata to this module role access.
	 */
	@SuppressWarnings("unchecked")
	public T addUxUi(ModuleRoleUserAccessUxUiMetadata uxui) {
		access.getUxuis().add(uxui);
		return (T) this;
	}

	/**
	 * Add a new uxui to this module role access with the specified uxui name.
	 */
	@SuppressWarnings("unchecked")
	public T addUxUi(String uxui) {
		ModuleRoleUserAccessUxUiMetadata namedUxUi = new ModuleRoleUserAccessUxUiMetadata();
		namedUxUi.setName(uxui);
		access.getUxuis().add(namedUxUi);
		return (T) this;
	}

	/**
	 * Clears all the uxuis for this module role access.
	 */
	@SuppressWarnings("unchecked")
	public T clearUxUis() {
		access.getUxuis().clear();
		return (T) this;
	}

	/**
	 * Removes the specified module role access uxui with the specified name if one is defined
	 * for this access.
	 */
	@SuppressWarnings("unchecked")
	public T removeUxUi(String uxui) {
		access.getUxuis().removeIf(u -> uxui.equals(u.getName()));
		return (T) this;
	}
}
