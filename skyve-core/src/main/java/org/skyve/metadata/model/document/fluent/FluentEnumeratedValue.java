package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Enumeration.EnumeratedValue;

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
	public FluentEnumeratedValue(EnumeratedValue value) {
		this.value = value;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public FluentEnumeratedValue from(@SuppressWarnings("hiding") EnumeratedValue value) {
		name(value.getName());
		code(value.getCode());
		description(value.getDescription());
		return this;
	}	
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentEnumeratedValue name(String name) {
		value.setName(name);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentEnumeratedValue code(String code) {
		value.setCode(code);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentEnumeratedValue description(String description) {
		value.setDescription(description);
		return this;
	}
	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	public EnumeratedValue get() {
		return value;
	}
}
