package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Decimal2;
import org.skyve.impl.metadata.model.document.field.validator.DecimalValidator;

/**
 * Provides a fluent builder for FluentDecimal2 metadata.
 */
public class FluentDecimal2 extends FluentConvertibleField<FluentDecimal2> {
	private Decimal2 decimal = null;
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentDecimal2() {
		decimal = new Decimal2();
	}
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentDecimal2(Decimal2 decimal) {
		this.decimal = decimal;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public FluentDecimal2 from(@SuppressWarnings("hiding") Decimal2 decimal) {
		super.from(decimal);
		validator(decimal.getValidator());
		return this;
	}
	/**
	 * Sets validation metadata used by this field definition.
	 */
	public FluentDecimal2 validator(DecimalValidator validator) {
		decimal.setValidator(validator);
		return this;
	}

	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	@Override
	public Decimal2 get() {
		return decimal;
	}
}
