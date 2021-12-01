package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Timestamp;
import org.skyve.impl.metadata.model.document.field.validator.DateValidator;

public class FluentTimestamp extends FluentConvertableField<FluentTimestamp> {
	private Timestamp timestamp = null;
	
	public FluentTimestamp() {
		timestamp = new Timestamp();
	}

	public FluentTimestamp(Timestamp timestamp) {
		this.timestamp = timestamp;
	}

	public FluentTimestamp from(@SuppressWarnings("hiding") Timestamp timestamp) {
		super.from(timestamp);
		validator(timestamp.getValidator());
		return this;
	}
	
	public FluentTimestamp validator(DateValidator validator) {
		timestamp.setValidator(validator);
		return this;
	}

	@Override
	public Timestamp get() {
		return timestamp;
	}
}
