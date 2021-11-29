package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.field.ConstrainableField;
import org.skyve.metadata.model.document.DomainType;

abstract class FluentConstrainableField<T extends FluentConstrainableField<T>> extends FluentField<T> {
	protected FluentConstrainableField() {
		// nothing to see
	}
	
	protected FluentConstrainableField(ConstrainableField field) {
		super(field);
		domainType(field.getDomainType());
	}

	@SuppressWarnings("unchecked")
	public T domainType(DomainType domainType) {
		get().setDomainType(domainType);
		return (T) this;
	}

	@Override
	public abstract ConstrainableField get();
}
