package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Time;
import org.skyve.impl.metadata.model.document.field.validator.DateValidator;

public class FluentTime extends FluentConvertableField<FluentTime> {
	private Time time = new Time();
	
	public FluentTime() {
		// nothing to see
	}

	public FluentTime(Time time) {
		super(time);
		validator(time.getValidator());
	}
	
	public FluentTime validator(DateValidator validator) {
		time.setValidator(validator);
		return this;
	}

	@Override
	public Time get() {
		return time;
	}
}
