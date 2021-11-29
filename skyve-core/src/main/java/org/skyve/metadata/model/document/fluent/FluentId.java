package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Id;

public class FluentId extends FluentField<FluentId> {
	private Id id = new Id();
	
	public FluentId() {
		// nothing to see
	}

	public FluentId(Id id) {
		super(id);
	}
	
	@Override
	public Id get() {
		return id;
	}
}
