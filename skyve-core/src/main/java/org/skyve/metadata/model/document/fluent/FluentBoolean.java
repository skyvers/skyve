package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Boolean;

import jakarta.annotation.Nonnull;

/**
 * Provides a fluent builder for FluentBoolean metadata.
 */
public class FluentBoolean extends FluentConvertibleField<FluentBoolean> {
	private Boolean bool = null;
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentBoolean() {
		bool = new Boolean();
	}
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentBoolean(@Nonnull Boolean bool) {
		this.bool = bool;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public @Nonnull FluentBoolean from(@SuppressWarnings("hiding") @Nonnull Boolean bool) {
		super.from(bool);
		return this;
	}
	
	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	@Override
	public @Nonnull Boolean get() {
		return bool;
	}
}
