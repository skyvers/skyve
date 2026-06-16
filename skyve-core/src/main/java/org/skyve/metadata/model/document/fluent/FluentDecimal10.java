package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Decimal10;
import org.skyve.impl.metadata.model.document.field.validator.DecimalValidator;

/**
 * Provides a fluent builder for FluentDecimal10 metadata.
 */
public class FluentDecimal10 extends FluentConvertibleField<FluentDecimal10> {
	private Decimal10 decimal = null;
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentDecimal10() {
		decimal = new Decimal10();
	}
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentDecimal10(Decimal10 decimal) {
		this.decimal = decimal;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public FluentDecimal10 from(@SuppressWarnings("hiding") Decimal10 decimal) {
		super.from(decimal);
		validator(decimal.getValidator());
		return this;
	}
	/**
	 * Sets validation metadata used by this field definition.
	 */
	public FluentDecimal10 validator(DecimalValidator validator) {
		decimal.setValidator(validator);
		return this;
	}

	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	@Override
	public Decimal10 get() {
		return decimal;
	}
}
