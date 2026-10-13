package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Date;
import org.skyve.impl.metadata.model.document.field.validator.DateValidator;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

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
	public FluentDate(@Nonnull Date date) {
		this.date = date;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public @Nonnull FluentDate from(@SuppressWarnings("hiding") @Nonnull Date date) {
		super.from(date);
		validator(date.getValidator());
		return this;
	}
	/**
	 * Sets validation metadata used by this field definition.
	 */
	public @Nonnull FluentDate validator(@Nullable DateValidator validator) {
		date.setValidator(validator);
		return this;
	}

	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	@Override
	public @Nonnull Date get() {
		return date;
	}
}
