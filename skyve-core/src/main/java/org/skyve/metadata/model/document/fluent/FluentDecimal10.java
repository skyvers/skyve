package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Decimal10;
import org.skyve.impl.metadata.model.document.field.validator.DecimalValidator;

public class FluentDecimal10 extends FluentConvertableField<FluentDecimal10> {
	private Decimal10 decimal = null;
	
	public FluentDecimal10() {
		decimal = new Decimal10();
	}
	
	public FluentDecimal10(Decimal10 decimal) {
		this.decimal = decimal;
	}

	public FluentDecimal10 from(@SuppressWarnings("hiding") Decimal10 decimal) {
		super.from(decimal);
		validator(decimal.getValidator());
		return this;
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
