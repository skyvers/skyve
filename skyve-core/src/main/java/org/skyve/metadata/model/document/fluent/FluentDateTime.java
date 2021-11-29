package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.DateTime;
import org.skyve.impl.metadata.model.document.field.validator.DateValidator;

public class FluentDateTime extends FluentConvertableField<FluentDateTime> {
	private DateTime dateTime = new DateTime();
	
	public FluentDateTime() {
		// nothing to see
	}

	public FluentDateTime(DateTime dateTime) {
		super(dateTime);
		validator(dateTime.getValidator());
	}
	
	public FluentDateTime validator(DateValidator validator) {
		dateTime.setValidator(validator);
		return this;
	}

	@Override
	public DateTime get() {
		return dateTime;
	}
}
