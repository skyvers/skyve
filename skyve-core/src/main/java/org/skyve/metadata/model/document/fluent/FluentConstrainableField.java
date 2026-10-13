package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.ConstrainableField;
import org.skyve.metadata.model.document.DomainType;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Provides a fluent builder for FluentConstrainableField metadata.
 */
public abstract class FluentConstrainableField<T extends FluentConstrainableField<T>> extends FluentField<T> {
	/**
	 * Creates a fluent builder instance.
	 */
	protected FluentConstrainableField() {
		// nothing to see
	}
	
	/**
	 * Copies constrainable field metadata, including its domain type binding.
	 */
	@SuppressWarnings("unchecked")
	protected @Nonnull T from(@Nonnull ConstrainableField field) {
		super.from(field);
		domainType(field.getDomainType());
		return (T) this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	@SuppressWarnings("unchecked")
	public @Nonnull T domainType(@Nullable DomainType domainType) {
		get().setDomainType(domainType);
		return (T) this;
	}

	@Override
	public abstract @Nonnull ConstrainableField get();
}
