package org.skyve.impl.metadata.module.query;

import org.skyve.CORE;
import org.skyve.metadata.module.query.BizQLDefinition;

public class BizQLReferenceImpl extends QueryReferenceImpl implements BizQLDefinition {
	private static final long serialVersionUID = -3499880086844926911L;

	public BizQLReferenceImpl(String name, String moduleRef, String ref) {
		super(name, moduleRef, ref);
	}

	@Override
	public String getQuery() {
		return getTarget().getQuery();
	}

	@Override
	@SuppressWarnings("unchecked")
	protected BizQLDefinition getTarget() {
		return CORE.getCustomer().getModule(moduleRef).getBizQL(ref);
	}
}
