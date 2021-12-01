package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Integer;
import org.skyve.impl.metadata.model.document.field.validator.IntegerValidator;

public class FluentInteger extends FluentConvertableField<FluentInteger> {
	private Integer integer = null;
	
	public FluentInteger() {
		integer = new Integer();
	}

	public FluentInteger(Integer integer) {
		this.integer = integer;
	}

	public FluentInteger from(@SuppressWarnings("hiding") Integer integer) {
		super.from(integer);
		validator(integer.getValidator());
		return this;
	}
	
	public FluentInteger validator(IntegerValidator validator) {
		integer.setValidator(validator);
		return this;
	}

	@Override
	public Integer get() {
		return integer;
	}
}
