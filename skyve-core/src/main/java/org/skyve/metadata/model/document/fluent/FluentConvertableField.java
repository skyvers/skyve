package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.ConvertableField;
import org.skyve.metadata.ConverterName;

abstract class FluentConvertableField<T extends FluentConvertableField<T>> extends FluentConstrainableField<T> {
	protected FluentConvertableField() {
		// nothing to see
	}
	
	protected FluentConvertableField(ConvertableField field) {
		super(field);
		converterName(field.getConverterName());
	}
	
	@SuppressWarnings("unchecked")
	public T converterName(ConverterName name) {
		get().setConverterName(name);
		return (T) this;
	}

	@Override
	public abstract ConvertableField get();
}
