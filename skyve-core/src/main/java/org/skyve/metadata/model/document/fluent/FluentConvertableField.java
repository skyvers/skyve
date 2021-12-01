package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.ConvertableField;
import org.skyve.metadata.ConverterName;

abstract class FluentConvertableField<T extends FluentConvertableField<T>> extends FluentConstrainableField<T> {
	protected FluentConvertableField() {
		// nothing to see
	}
	
	@SuppressWarnings("unchecked")
	protected T from(ConvertableField field) {
		super.from(field);
		converterName(field.getConverterName());
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public T converterName(ConverterName name) {
		get().setConverterName(name);
		return (T) this;
	}

	@Override
	public abstract ConvertableField get();
}
