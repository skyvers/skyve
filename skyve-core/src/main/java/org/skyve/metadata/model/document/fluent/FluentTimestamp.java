package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Timestamp;
import org.skyve.impl.metadata.model.document.field.validator.DateValidator;

public class FluentTimestamp extends FluentConvertableField<FluentTimestamp> {
	private Timestamp timestamp = new Timestamp();
	
	public FluentTimestamp() {
		// nothing to see
	}
	
	public FluentTimestamp(Timestamp timestamp) {
		super(timestamp);
		validator(timestamp.getValidator());
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
