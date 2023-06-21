package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.repository.view.access.ViewUserAccessMetaData;
import org.skyve.impl.metadata.repository.view.access.ViewUserAccessUxUiMetadata;

/**
 * Abstract base fluent builder class for working with {@link FluentViewUserAccess}.
 * 
 * @author brandon-klar
 *
 * @param <T> The fluent view user access builder subtype
 * @param <M> The view user access metadata subtype
 */
abstract class FluentViewUserAccess<T extends FluentViewUserAccess<T, M>, M extends ViewUserAccessMetaData> {
	protected M access;

	protected FluentViewUserAccess() {
		// no implementation
	}

	/**
	 * Returns the underlying view user access metadata from this builder.
	 */
	public abstract M get();

	/**
	 * Add a new ViewUserAccessUxUiMetadata to this view user access.
	 */
	@SuppressWarnings("unchecked")
	public T addUxUi(ViewUserAccessUxUiMetadata uxui) {
		access.getUxuis().add(uxui);
		return (T) this;
	}

	/**
	 * Add a new uxui to this view user access with the specified uxui name.
	 */
	@SuppressWarnings("unchecked")
	public T addUxUi(String uxui) {
		ViewUserAccessUxUiMetadata namedUxUi = new ViewUserAccessUxUiMetadata();
		namedUxUi.setName(uxui);
		access.getUxuis().add(namedUxUi);
		return (T) this;
	}

	/**
	 * Clears all the uxuis for this view user access.
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
