package org.skyve.impl.metadata.module.query;

import org.skyve.metadata.module.query.BizQLDefinition;
import org.skyve.impl.metadata.module.query.QueryDefinitionImpl;

public class BizQLDefinitionImpl extends QueryDefinitionImpl implements BizQLDefinition {
	private static final long serialVersionUID = -6010414111423395137L;

	private String query;

	@Override
	public String getQuery() {
		return query;
	}

	public void setQuery(String query) {
		this.query = query;
	}
}
