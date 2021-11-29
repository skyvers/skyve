package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.SQLMetaData;
import org.skyve.metadata.module.query.SQLDefinition;

public class FluentSQL extends FluentQuery<FluentSQL> {
	private SQLMetaData sql = new SQLMetaData();
	
	public FluentSQL() {
		// nothing to see
	}
	
	public FluentSQL(SQLDefinition sql) {
		super(sql);
		query(sql.getQuery());
	}
	
	public FluentSQL query(String query) {
		sql.setQuery(query);
		return this;
	}

	@Override
	public SQLMetaData get() {
		return sql;
	}
}
