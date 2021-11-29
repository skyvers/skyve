package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.BizQLMetaData;
import org.skyve.metadata.module.query.BizQLDefinition;

public class FluentBizQL extends FluentQuery<FluentBizQL> {
	private BizQLMetaData bizql = new BizQLMetaData();
	
	public FluentBizQL() {
		// nothing to see
	}
	
	public FluentBizQL(BizQLDefinition bizql) {
		super(bizql);
		query(bizql.getQuery());
	}
	
	public FluentBizQL query(String query) {
		bizql.setQuery(query);
		return this;
	}

	@Override
	public BizQLMetaData get() {
		return bizql;
	}
}
