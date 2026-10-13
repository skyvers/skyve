package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.ActionMetaData;
import org.skyve.impl.metadata.repository.module.ApplicableTo;
import org.skyve.metadata.module.menu.MenuItem;

import jakarta.annotation.Nonnull;

/**
 * Base fluent builder for menu actions.
 */
public abstract class FluentMenuAction<T extends FluentMenuAction<T>> {
	/**
	 * Creates an empty fluent menu action wrapper.
	 */
	protected FluentMenuAction() {
		// nothing to see here
	}

	/**
	 * Copies common menu action fields from an existing menu item.
	 *
	 * @param item The source menu item.
	 * @return this fluent instance.
	 */
	@SuppressWarnings("unchecked")
	protected T from(MenuItem item) {
		name(item.getName());
		item.getUxUis().forEach(this::addUxUi);
		item.getProperties().entrySet().forEach(p -> putProperty(p.getKey(), p.getValue()));

		return (T) this;
	}
	
	/**
	 * Sets the action name.
	 *
	 * @param name The action name.
	 * @return this fluent instance.
	 */
	@SuppressWarnings("unchecked")
	public T name(String name) {
		get().setName(name);
		return (T) this;
	}

	/**
	 * Adds a UX/UI channel restriction.
	 *
	 * @param uxui The channel identifier.
	 * @return this fluent instance.
	 */
	@SuppressWarnings("unchecked")
	public T addUxUi(String uxui) {
		ApplicableTo applicable = new ApplicableTo();
		applicable.setUxUi(uxui);
		get().getUxuis().add(applicable);
		return (T) this;
	}

	/**
	 * Removes a UX/UI channel restriction.
	 *
	 * @param uxui The channel identifier to remove.
	 * @return this fluent instance.
	 */
	@SuppressWarnings("unchecked")
	public T removeUxUi(String uxui) {
		get().getUxuis().removeIf(u -> uxui.equals(u.getUxUi()));
		return (T) this;
	}

	/**
	 * Clears all UX/UI channel restrictions.
	 *
	 * @return this fluent instance.
	 */
	@SuppressWarnings("unchecked")
	public T clearUxUis() {
		get().getUxuis().clear();
		return (T) this;
	}

	/**
	 * Adds or replaces a custom property on the wrapped metadata.
	 * Properties keep the order in which they were first added.
	 *
	 * @param k the property name
	 * @param v the property value
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public @Nonnull T putProperty(@Nonnull String k, @Nonnull String v) {
		get().getProperties().put(k, v);
		return (T) this;
	}

	/**
	 * Returns the mutable metadata backing this fluent wrapper.
	 *
	 * @return The action metadata instance.
	 */
	public abstract ActionMetaData get();
}
