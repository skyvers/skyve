package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Field;
import org.skyve.impl.metadata.model.document.field.Field.IndexType;

abstract class FluentField<T extends FluentField<T>> extends FluentAttribute<T> {
	protected FluentField() {
		// nothing to see
	}
	
	@SuppressWarnings("unchecked")
	protected T from(Field field) {
		super.from(field);
		required(field.isRequired());
		persistent(field.isPersistent());
		dynamic(field.isDynamic());
		index(field.getIndex());
		defaultValue(field.getDefaultValue());
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T required(boolean required) {
		get().setRequired(required);
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public T persistent(boolean persistent) {
		get().setPersistent(persistent);
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public T dynamic(boolean dynamic) {
		get().setDynamic(dynamic);
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public T index(IndexType index) {
		get().setIndex(index);
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T defaultValue(String defaultValue) {
		get().setDefaultValue(defaultValue);
		return (T) this;
	}

	@Override
	public abstract Field get();
}
