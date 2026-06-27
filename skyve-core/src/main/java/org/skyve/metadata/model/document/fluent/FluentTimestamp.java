package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Timestamp;
import org.skyve.impl.metadata.model.document.field.validator.DateValidator;

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
	public FluentTimestamp(Timestamp timestamp) {
		this.timestamp = timestamp;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public FluentTimestamp from(@SuppressWarnings("hiding") Timestamp timestamp) {
		super.from(timestamp);
		validator(timestamp.getValidator());
		return this;
	}
	/**
	 * Sets validation metadata used by this field definition.
	 */
	public FluentTimestamp validator(DateValidator validator) {
		timestamp.setValidator(validator);
		return this;
	}

	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	@Override
	public Timestamp get() {
		return timestamp;
	}
}
