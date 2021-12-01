package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Boolean;

public class FluentBoolean extends FluentConvertableField<FluentBoolean> {
	private Boolean bool = null;
	
	public FluentBoolean() {
		bool = new Boolean();
	}
	
	public FluentBoolean(Boolean bool) {
		this.bool = bool;
	}

	public FluentBoolean from(@SuppressWarnings("hiding") Boolean bool) {
		super.from(bool);
		return this;
	}
	
	@Override
	public Boolean get() {
		return bool;
	}
}
