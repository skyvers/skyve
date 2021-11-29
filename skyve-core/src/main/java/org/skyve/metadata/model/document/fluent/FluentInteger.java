package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Integer;
import org.skyve.impl.metadata.model.document.field.validator.IntegerValidator;

public class FluentInteger extends FluentConvertableField<FluentInteger> {
	private Integer integer = new Integer();
	
	public FluentInteger() {
		// nothing to see
	}

	public FluentInteger(Integer integer) {
		super(integer);
		validator(integer.getValidator());
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
