package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.module.query.BizQLReferenceImpl;
import org.skyve.impl.metadata.repository.module.BizQLReferenceMetaData;

public class FluentBizQLReference extends FluentQueryReference<FluentBizQLReference> {
	private BizQLReferenceMetaData bizql = null;
	
	public FluentBizQLReference() {
		bizql = new BizQLReferenceMetaData();
	}
	
	public FluentBizQLReference(BizQLReferenceMetaData bizql) {
		this.bizql = bizql;
	}
	
	public FluentBizQLReference from(@SuppressWarnings("hiding") BizQLReferenceImpl bizql) {
		super.from(bizql);
		return this;
	}
	
	@Override
	public BizQLReferenceMetaData get() {
		return bizql;
	}
}
