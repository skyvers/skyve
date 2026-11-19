package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.DateTime;
import org.skyve.impl.metadata.model.document.field.validator.DateValidator;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

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
	public FluentDateTime(@Nonnull DateTime dateTime) {
		this.dateTime = dateTime;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public @Nonnull FluentDateTime from(@SuppressWarnings("hiding") @Nonnull DateTime dateTime) {
		super.from(dateTime);
		validator(dateTime.getValidator());
		return this;
	}
	/**
	 * Sets validation metadata used by this field definition.
	 */
	public @Nonnull FluentDateTime validator(@Nullable DateValidator validator) {
		dateTime.setValidator(validator);
		return this;
	}

	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	@Override
	public @Nonnull DateTime get() {
		return dateTime;
	}
}
