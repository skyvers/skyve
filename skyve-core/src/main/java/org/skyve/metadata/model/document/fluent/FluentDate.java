package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Date;
import org.skyve.impl.metadata.model.document.field.validator.DateValidator;

public class FluentDate extends FluentConvertableField<FluentDate> {
	private Date date = null;
	
	public FluentDate() {
		date = new Date();
	}
	
	public FluentDate(Date date) {
		this.date = date;
	}

	public FluentDate from(@SuppressWarnings("hiding") Date date) {
		super.from(date);
		validator(date.getValidator());
		return this;
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
