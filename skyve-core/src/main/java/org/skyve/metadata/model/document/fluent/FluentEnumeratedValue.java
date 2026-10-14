package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Enumeration.EnumeratedValue;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Provides a fluent builder for FluentEnumeratedValue metadata.
 */
public class FluentEnumeratedValue {
	private EnumeratedValue value = null;
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentEnumeratedValue() {
		value = new EnumeratedValue();
	}
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentEnumeratedValue(@Nonnull EnumeratedValue value) {
		this.value = value;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public @Nonnull FluentEnumeratedValue from(@SuppressWarnings("hiding") @Nonnull EnumeratedValue value) {
		name(value.getName());
		code(value.getCode());
		description(value.getDescription());
		return this;
	}	
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public @Nonnull FluentEnumeratedValue name(@Nullable String name) {
		value.setName(name);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public @Nonnull FluentEnumeratedValue code(@Nonnull String code) {
		value.setCode(code);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public @Nonnull FluentEnumeratedValue description(@Nullable String description) {
		value.setDescription(description);
		return this;
	}
	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	public @Nonnull EnumeratedValue get() {
		return value;
	}
}
