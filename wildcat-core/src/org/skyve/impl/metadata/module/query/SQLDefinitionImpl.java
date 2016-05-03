package org.skyve.impl.metadata.module.query;

import org.skyve.metadata.module.query.SQLDefinition;
import org.skyve.impl.metadata.module.query.QueryDefinitionImpl;

public class SQLDefinitionImpl extends QueryDefinitionImpl implements SQLDefinition {
	private static final long serialVersionUID = 4044590120129931022L;

	private String query;

	@Override
	public String getQuery() {
		return query;
	}

	public void setQuery(String query) {
		this.query = query;
	}
}
