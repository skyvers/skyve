package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Enumeration.EnumeratedValue;

public class FluentEnumeratedValue {
	private EnumeratedValue value = null;
	
	public FluentEnumeratedValue() {
		value = new EnumeratedValue();
	}
	
	public FluentEnumeratedValue(EnumeratedValue value) {
		this.value = value;
	}

	public FluentEnumeratedValue from(@SuppressWarnings("hiding") EnumeratedValue value) {
		name(value.getName());
		code(value.getCode());
		description(value.getDescription());
		return this;
	}	
	
	public FluentEnumeratedValue name(String name) {
		value.setName(name);
		return this;
	}

	public FluentEnumeratedValue code(String code) {
		value.setCode(code);
		return this;
	}

	public FluentEnumeratedValue description(String description) {
		value.setDescription(description);
		return this;
	}

	public EnumeratedValue get() {
		return value;
	}
}
