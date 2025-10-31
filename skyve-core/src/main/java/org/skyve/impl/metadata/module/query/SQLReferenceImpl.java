package org.skyve.impl.metadata.module.query;

import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.metadata.module.query.SQLDefinition;

public class SQLReferenceImpl extends QueryReferenceImpl implements SQLDefinition {
	private static final long serialVersionUID = -5989614154659932867L;

	public SQLReferenceImpl(String name, String moduleRef, String ref) {
		super(name, moduleRef, ref);
	}

	@Override
	public String getQuery() {
		return getTarget().getQuery();
	}

	@Override
	@SuppressWarnings("unchecked")
	protected SQLDefinition getTarget() {
		// TODO this should use the current customer (can't get customer during generate domain)
		return ProvidedRepositoryFactory.get().getModule(null, moduleRef).getSQL(ref);
	}
}
