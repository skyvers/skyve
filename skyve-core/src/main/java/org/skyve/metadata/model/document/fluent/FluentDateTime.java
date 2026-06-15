package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.DateTime;
import org.skyve.impl.metadata.model.document.field.validator.DateValidator;

/**
 * Provides a fluent builder for FluentDateTime metadata.
 */
public class FluentDateTime extends FluentConvertibleField<FluentDateTime> {
	private DateTime dateTime = null;
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentDateTime() {
		dateTime = new DateTime();
	}

	/**
	 * Creates a fluent builder instance.
	 */
	public FluentDateTime(DateTime dateTime) {
		this.dateTime = dateTime;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public FluentDateTime from(@SuppressWarnings("hiding") DateTime dateTime) {
		super.from(dateTime);
		validator(dateTime.getValidator());
		return this;
	}
	/**
	 * Sets validation metadata used by this field definition.
	 */
	public FluentDateTime validator(DateValidator validator) {
		dateTime.setValidator(validator);
		return this;
	}

	@Override
	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	public DateTime get() {
		return dateTime;
	}
}
