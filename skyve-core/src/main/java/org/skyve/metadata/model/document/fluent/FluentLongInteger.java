package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.LongInteger;
import org.skyve.impl.metadata.model.document.field.validator.LongValidator;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Provides a fluent builder for FluentLongInteger metadata.
 */
public class FluentLongInteger extends FluentConvertibleField<FluentLongInteger> {
	private LongInteger longInteger = null;
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentLongInteger() {
		longInteger = new LongInteger();
	}

	/**
	 * Creates a fluent builder instance.
	 */
	public FluentLongInteger(@Nonnull LongInteger integer) {
		this.longInteger = integer;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public @Nonnull FluentLongInteger from(@SuppressWarnings("hiding") @Nonnull LongInteger longInteger) {
		super.from(longInteger);
		validator(longInteger.getValidator());
		return this;
	}
	/**
	 * Sets validation metadata used by this field definition.
	 */
	public @Nonnull FluentLongInteger validator(@Nullable LongValidator validator) {
		longInteger.setValidator(validator);
		return this;
	}

	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	@Override
	public @Nonnull LongInteger get() {
		return longInteger;
	}
}
