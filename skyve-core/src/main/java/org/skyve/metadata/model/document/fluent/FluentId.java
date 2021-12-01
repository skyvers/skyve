package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Id;

public class FluentId extends FluentField<FluentId> {
	private Id id = null;
	
	public FluentId() {
		id = new Id();
	}

	public FluentId(Id id) {
		this.id = id;
	}

	public FluentId from(@SuppressWarnings("hiding") Id id) {
		super.from(id);
		return this;
	}
	
	@Override
	public Id get() {
		return id;
	}
}
