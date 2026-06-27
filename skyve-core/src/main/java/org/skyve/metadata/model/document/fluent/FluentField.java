package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.Field;
import org.skyve.impl.metadata.model.document.field.Field.GeneratedType;
import org.skyve.impl.metadata.model.document.field.Field.IndexType;

/**
 * Provides a fluent builder for FluentField metadata.
 */
public abstract class FluentField<T extends FluentField<T>> extends FluentAttribute<T> {
	/**
	 * Creates a fluent builder instance.
	 */
	protected FluentField() {
		// nothing to see
	}
	
	/**
	 * Copies field-level metadata, including validation and generation settings.
	 */
	@SuppressWarnings("unchecked")
	protected T from(Field field) {
		super.from(field);
		required(field.isRequired());
		requiredMessage(field.getRequiredMessage());
		persistent(field.isPersistent());
		dynamic(field.isDynamic());
		index(field.getIndex());
		defaultValue(field.getDefaultValue());
		generated(field.getGenerated());
		return (T) this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	@SuppressWarnings("unchecked")
	public T required(boolean required) {
		get().setRequired(required);
		return (T) this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	@SuppressWarnings("unchecked")
	public T requiredMessage(String message) {
		get().setRequiredMessage(message);
		return (T) this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	@SuppressWarnings("unchecked")
	public T persistent(boolean persistent) {
		get().setPersistent(persistent);
		return (T) this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	@SuppressWarnings("unchecked")
	public T dynamic(boolean dynamic) {
		get().setDynamic(dynamic);
		return (T) this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	@SuppressWarnings("unchecked")
	public T index(IndexType index) {
		get().setIndex(index);
		return (T) this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	@SuppressWarnings("unchecked")
	public T defaultValue(String defaultValue) {
		get().setDefaultValue(defaultValue);
		return (T) this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	@SuppressWarnings("unchecked")
	public T generated(GeneratedType generated) {
		get().setGenerated(generated);
		return (T) this;
	}

	@Override
	public abstract Field get();
}
