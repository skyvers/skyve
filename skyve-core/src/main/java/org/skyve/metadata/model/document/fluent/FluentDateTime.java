package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.DateTime;
import org.skyve.impl.metadata.model.document.field.validator.DateValidator;

public class FluentDateTime extends FluentConvertableField<FluentDateTime> {
	private DateTime dateTime = null;
	
	public FluentDateTime() {
		dateTime = new DateTime();
	}

	public FluentDateTime(DateTime dateTime) {
		this.dateTime = dateTime;
	}

	public FluentDateTime from(@SuppressWarnings("hiding") DateTime dateTime) {
		super.from(dateTime);
		validator(dateTime.getValidator());
		return this;
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
