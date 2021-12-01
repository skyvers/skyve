package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Time;
import org.skyve.impl.metadata.model.document.field.validator.DateValidator;

public class FluentTime extends FluentConvertableField<FluentTime> {
	private Time time = null;
	
	public FluentTime() {
		time = new Time();
	}

	public FluentTime(Time time) {
		this.time = time;
	}

	public FluentTime from(@SuppressWarnings("hiding") Time time) {
		super.from(time);
		validator(time.getValidator());
		return this;
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
