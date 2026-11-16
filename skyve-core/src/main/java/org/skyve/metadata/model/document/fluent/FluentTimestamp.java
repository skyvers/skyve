package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Timestamp;
import org.skyve.impl.metadata.model.document.field.validator.DateValidator;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Provides a fluent builder for FluentTimestamp metadata.
 */
public class FluentTimestamp extends FluentConvertibleField<FluentTimestamp> {
	private Timestamp timestamp = null;
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentTimestamp() {
		timestamp = new Timestamp();
	}

	/**
	 * Creates a fluent builder instance.
	 */
	public FluentTimestamp(@Nonnull Timestamp timestamp) {
		this.timestamp = timestamp;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public @Nonnull FluentTimestamp from(@SuppressWarnings("hiding") @Nonnull Timestamp timestamp) {
		super.from(timestamp);
		validator(timestamp.getValidator());
		return this;
	}
	/**
	 * Sets validation metadata used by this field definition.
	 */
	public @Nonnull FluentTimestamp validator(@Nullable DateValidator validator) {
		timestamp.setValidator(validator);
		return this;
	}

	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	@Override
	public @Nonnull Timestamp get() {
		return timestamp;
	}
}
