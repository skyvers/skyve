package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Id;

/**
 * Provides a fluent builder for FluentId metadata.
 */
public class FluentId extends FluentField<FluentId> {
	private Id id = null;
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentId() {
		id = new Id();
	}

	/**
	 * Creates a fluent builder instance.
	 */
	public FluentId(Id id) {
		this.id = id;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public FluentId from(@SuppressWarnings("hiding") Id id) {
		super.from(id);
		return this;
	}
	
	@Override
	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	public Id get() {
		return id;
	}
}
