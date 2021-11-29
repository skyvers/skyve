package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Decimal10;
import org.skyve.impl.metadata.model.document.field.validator.DecimalValidator;

public class FluentDecimal10 extends FluentConvertableField<FluentDecimal10> {
	private Decimal10 decimal = new Decimal10();
	
	public FluentDecimal10() {
		// nothing to see
	}
	
	public FluentDecimal10(Decimal10 decimal) {
		super(decimal);
		validator(decimal.getValidator());
	}
	
	public FluentDecimal10 validator(DecimalValidator validator) {
		decimal.setValidator(validator);
		return this;
	}

	@Override
	public Decimal10 get() {
		return decimal;
	}
}
