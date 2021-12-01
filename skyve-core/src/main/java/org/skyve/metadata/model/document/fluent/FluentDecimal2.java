package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Decimal2;
import org.skyve.impl.metadata.model.document.field.validator.DecimalValidator;

public class FluentDecimal2 extends FluentConvertableField<FluentDecimal2> {
	private Decimal2 decimal = null;
	
	public FluentDecimal2() {
		decimal = new Decimal2();
	}
	
	public FluentDecimal2(Decimal2 decimal) {
		this.decimal = decimal;
	}

	public FluentDecimal2 from(@SuppressWarnings("hiding") Decimal2 decimal) {
		super.from(decimal);
		validator(decimal.getValidator());
		return this;
	}
	
	public FluentDecimal2 validator(DecimalValidator validator) {
		decimal.setValidator(validator);
		return this;
	}

	@Override
	public Decimal2 get() {
		return decimal;
	}
}
