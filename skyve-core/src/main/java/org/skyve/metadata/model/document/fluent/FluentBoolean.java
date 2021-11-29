package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Boolean;

public class FluentBoolean extends FluentConvertableField<FluentBoolean> {
	private Boolean bool = new Boolean();
	
	public FluentBoolean() {
		// nothing to see
	}
	
	public FluentBoolean(Boolean bool) {
		super(bool);
	}
	
	@Override
	public Boolean get() {
		return bool;
	}
}
