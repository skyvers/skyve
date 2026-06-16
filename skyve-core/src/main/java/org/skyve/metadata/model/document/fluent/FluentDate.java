package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Date;
import org.skyve.impl.metadata.model.document.field.validator.DateValidator;

/**
 * Provides a fluent builder for FluentDate metadata.
 */
public class FluentDate extends FluentConvertibleField<FluentDate> {
	private Date date = null;
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentDate() {
		date = new Date();
	}
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentDate(Date date) {
		this.date = date;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public FluentDate from(@SuppressWarnings("hiding") Date date) {
		super.from(date);
		validator(date.getValidator());
		return this;
	}
	/**
	 * Sets validation metadata used by this field definition.
	 */
	public FluentDate validator(DateValidator validator) {
		date.setValidator(validator);
		return this;
	}

	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	@Override
	public Date get() {
		return date;
	}
}
