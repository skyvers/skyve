package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.module.query.SQLReferenceImpl;
import org.skyve.impl.metadata.repository.module.SQLReferenceMetaData;

public class FluentSQLReference extends FluentQueryReference<FluentSQLReference> {
	private SQLReferenceMetaData sql = null;
	
	public FluentSQLReference() {
		this.sql = new SQLReferenceMetaData();
	}
	
	public FluentSQLReference(SQLReferenceMetaData sql) {
		this.sql = sql;
	}

	public FluentSQLReference from(@SuppressWarnings("hiding") SQLReferenceImpl sql) {
		super.from(sql);
		return this;
	}
	
	@Override
	public SQLReferenceMetaData get() {
		return sql;
	}
}
