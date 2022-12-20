package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.ModuleRoleUserAccessMetaData;
import org.skyve.impl.metadata.repository.module.ModuleRoleUserAccessUxUiMetadata;

/**
 * Abstract base fluent builder class for working with {@link FluentModuleRoleAccess}.
 * 
 * @author benpetito
 *
 * @param <T> The fluent module role access builder subtype
 * @param <U> The module role access metadata subtype
 */
abstract class FluentModuleRoleAccess<T extends FluentModuleRoleAccess<T, U>, U extends ModuleRoleUserAccessMetaData> {

	/**
	 * The underlying metadata object
	 */
	protected U access;

	protected FluentModuleRoleAccess() {
		// no implementation
	}

	/**
	 * Returns a fluent module role access builder subtype from a module role access metadata subtype.
	 */
	@SuppressWarnings("unchecked")
	protected T from(@SuppressWarnings("hiding") U access) {
		access.getUxuis().forEach(u -> addUxUi(u));
		return (T) this;
	}

	/**
	 * Returns the underlying module role access metadata from this builder.
	 */
	public ModuleRoleUserAccessMetaData get() {
		return access;
	}

	/**
	 * Add a new ModuleRoleUserAccessUxUiMetadata to this module role access.
	 */
	@SuppressWarnings("unchecked")
	public T addUxUi(ModuleRoleUserAccessUxUiMetadata uxui) {
		get().getUxuis().add(uxui);
		return (T) this;
	}

	/**
	 * Add a new uxui to this module role access with the specified uxui name.
	 */
	@SuppressWarnings("unchecked")
	public T addUxUi(String uxui) {
		ModuleRoleUserAccessUxUiMetadata namedUxUi = new ModuleRoleUserAccessUxUiMetadata();
		namedUxUi.setName(uxui);
		get().getUxuis().add(namedUxUi);
		return (T) this;
	}

	/**
	 * Clears all the uxuis for this module role access.
	 */
	@SuppressWarnings("unchecked")
	public T clearUxUis() {
		get().getUxuis().clear();
		return (T) this;
	}

	/**
	 * Removes the specified module role access uxui with the specified name if one is defined
	 * for this access.
	 */
	@SuppressWarnings("unchecked")
	public T removeUxUi(String uxui) {
		get().getUxuis().removeIf(u -> uxui.equals(u.getName()));
		return (T) this;
	}
}
