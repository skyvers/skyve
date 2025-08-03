package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.ConvertibleField;
import org.skyve.metadata.ConverterName;

public abstract class FluentConvertibleField<T extends FluentConvertibleField<T>> extends FluentConstrainableField<T> {
	protected FluentConvertibleField() {
		// nothing to see
	}
	
	@SuppressWarnings("unchecked")
	protected T from(ConvertibleField field) {
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
	public abstract ConvertibleField get();
}
