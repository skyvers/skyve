package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.LongInteger;
import org.skyve.impl.metadata.model.document.field.validator.LongValidator;

public class FluentLongInteger extends FluentConvertableField<FluentLongInteger> {
	private LongInteger longInteger = null;
	
	public FluentLongInteger() {
		longInteger = new LongInteger();
	}

	public FluentLongInteger(LongInteger integer) {
		this.longInteger = integer;
	}

	public FluentLongInteger from(@SuppressWarnings("hiding") LongInteger longInteger) {
		super.from(longInteger);
		validator(longInteger.getValidator());
		return this;
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
