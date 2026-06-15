package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Integer;
import org.skyve.impl.metadata.model.document.field.validator.IntegerValidator;

/**
 * Provides a fluent builder for FluentInteger metadata.
 */
public class FluentInteger extends FluentConvertibleField<FluentInteger> {
	private Integer integer = null;
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentInteger() {
		integer = new Integer();
	}

	/**
	 * Creates a fluent builder instance.
	 */
	public FluentInteger(Integer integer) {
		this.integer = integer;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public FluentInteger from(@SuppressWarnings("hiding") Integer integer) {
		super.from(integer);
		validator(integer.getValidator());
		return this;
	}
	/**
	 * Sets validation metadata used by this field definition.
	 */
	public FluentInteger validator(IntegerValidator validator) {
		integer.setValidator(validator);
		return this;
	}

	@Override
	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	public Integer get() {
		return integer;
	}
}
