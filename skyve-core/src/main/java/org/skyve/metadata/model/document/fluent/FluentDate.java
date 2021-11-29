package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Date;
import org.skyve.impl.metadata.model.document.field.validator.DateValidator;

public class FluentDate extends FluentConvertableField<FluentDate> {
	private Date date = new Date();
	
	public FluentDate() {
		// nothing to see
	}
	
	public FluentDate(Date date) {
		super(date);
		validator(date.getValidator());
	}
	
	public FluentDate validator(DateValidator validator) {
		date.setValidator(validator);
		return this;
	}

	@Override
	public Date get() {
		return date;
	}
}
