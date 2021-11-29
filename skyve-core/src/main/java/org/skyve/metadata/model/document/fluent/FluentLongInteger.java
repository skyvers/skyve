package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.LongInteger;
import org.skyve.impl.metadata.model.document.field.validator.LongValidator;

public class FluentLongInteger extends FluentConvertableField<FluentLongInteger> {
	private LongInteger longInteger = new LongInteger();
	
	public FluentLongInteger() {
		// nothing to see
	}
	
	public FluentLongInteger(LongInteger longInteger) {
		super(longInteger);
		validator(longInteger.getValidator());
	}
	
	public FluentLongInteger validator(LongValidator validator) {
		longInteger.setValidator(validator);
		return this;
	}

	@Override
	public LongInteger get() {
		return longInteger;
	}
}
