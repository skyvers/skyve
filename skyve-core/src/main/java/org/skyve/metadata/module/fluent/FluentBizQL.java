package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.BizQLMetaData;
import org.skyve.metadata.module.query.BizQLDefinition;

public class FluentBizQL extends FluentQuery<FluentBizQL> {
	private BizQLMetaData bizql = null;
	
	public FluentBizQL() {
		bizql = new BizQLMetaData();
	}
	
	public FluentBizQL(BizQLMetaData bizql) {
		this.bizql = bizql;
	}
	
	public FluentBizQL from(@SuppressWarnings("hiding") BizQLDefinition bizql) {
		super.from(bizql);
		query(bizql.getQuery());
		return this;
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
