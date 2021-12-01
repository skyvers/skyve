package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.ConstrainableField;
import org.skyve.metadata.model.document.DomainType;

abstract class FluentConstrainableField<T extends FluentConstrainableField<T>> extends FluentField<T> {
	protected FluentConstrainableField() {
		// nothing to see
	}
	
	@SuppressWarnings("unchecked")
	protected T from(ConstrainableField field) {
		super.from(field);
		domainType(field.getDomainType());
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T domainType(DomainType domainType) {
		get().setDomainType(domainType);
		return (T) this;
	}

	@Override
	public abstract ConstrainableField get();
}
