package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.SQLMetaData;
import org.skyve.metadata.module.query.SQLDefinition;

public class FluentSQL extends FluentQuery<FluentSQL> {
	private SQLMetaData sql = null;
	
	public FluentSQL() {
		this.sql = new SQLMetaData();
	}
	
	public FluentSQL(SQLMetaData sql) {
		this.sql = sql;
	}

	public FluentSQL from(@SuppressWarnings("hiding") SQLDefinition sql) {
		super.from(sql);
		query(sql.getQuery());
		return this;
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
