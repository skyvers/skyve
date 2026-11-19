package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Time;
import org.skyve.impl.metadata.model.document.field.validator.DateValidator;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Provides a fluent builder for FluentTime metadata.
 */
public class FluentTime extends FluentConvertibleField<FluentTime> {
	private Time time = null;
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentTime() {
		time = new Time();
	}

	/**
	 * Creates a fluent builder instance.
	 */
	public FluentTime(@Nonnull Time time) {
		this.time = time;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public @Nonnull FluentTime from(@SuppressWarnings("hiding") @Nonnull Time time) {
		super.from(time);
		validator(time.getValidator());
		return this;
	}
	/**
	 * Sets validation metadata used by this field definition.
	 */
	public @Nonnull FluentTime validator(@Nullable DateValidator validator) {
		time.setValidator(validator);
		return this;
	}

	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	@Override
	public @Nonnull Time get() {
		return time;
	}
}
