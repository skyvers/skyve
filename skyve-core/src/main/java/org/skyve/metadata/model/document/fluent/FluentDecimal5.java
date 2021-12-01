package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Decimal5;
import org.skyve.impl.metadata.model.document.field.validator.DecimalValidator;

public class FluentDecimal5 extends FluentConvertableField<FluentDecimal5> {
	private Decimal5 decimal = null;
	
	public FluentDecimal5() {
		decimal = new Decimal5();
	}
	
	public FluentDecimal5(Decimal5 decimal) {
		this.decimal = decimal;
	}

	public FluentDecimal5 from(@SuppressWarnings("hiding") Decimal5 decimal) {
		super.from(decimal);
		validator(decimal.getValidator());
		return this;
	}
	
	public FluentDecimal5 validator(DecimalValidator validator) {
		decimal.setValidator(validator);
		return this;
	}

	@Override
	public Decimal5 get() {
		return decimal;
	}
}
