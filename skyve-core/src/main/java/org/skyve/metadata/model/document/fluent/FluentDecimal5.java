package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Decimal5;
import org.skyve.impl.metadata.model.document.field.validator.DecimalValidator;

/**
 * Provides a fluent builder for FluentDecimal5 metadata.
 */
public class FluentDecimal5 extends FluentConvertibleField<FluentDecimal5> {
	private Decimal5 decimal = null;
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentDecimal5() {
		decimal = new Decimal5();
	}
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentDecimal5(Decimal5 decimal) {
		this.decimal = decimal;
	}

	/**
	 * Copies metadata values from an existing definition into this builder.
	 */

	public FluentDecimal5 from(@SuppressWarnings("hiding") Decimal5 decimal) {
		super.from(decimal);
		validator(decimal.getValidator());
		return this;
	}
	/**
	 * Sets validation metadata used by this field definition.
	 */
	public FluentDecimal5 validator(DecimalValidator validator) {
		decimal.setValidator(validator);
		return this;
	}

	@Override
	/**
	 * Returns the mutable metadata instance represented by this builder.
	 */
	public Decimal5 get() {
		return decimal;
	}
}
