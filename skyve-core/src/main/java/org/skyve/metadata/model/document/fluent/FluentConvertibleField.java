package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.ConvertibleField;
import org.skyve.metadata.ConverterName;

/**
 * Provides a fluent builder for FluentConvertibleField metadata.
 */
public abstract class FluentConvertibleField<T extends FluentConvertibleField<T>> extends FluentConstrainableField<T> {
	/**
	 * Creates a fluent builder instance.
	 */
	protected FluentConvertibleField() {
		// nothing to see
	}
	
	/**
	 * Copies convertible field metadata, including converter selection.
	 */
	@SuppressWarnings("unchecked")
	protected T from(ConvertibleField field) {
		super.from(field);
		converterName(field.getConverterName());
		return (T) this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	@SuppressWarnings("unchecked")
	public T converterName(ConverterName name) {
		get().setConverterName(name);
		return (T) this;
	}

	@Override
	public abstract ConvertibleField get();
}
