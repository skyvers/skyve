package org.skyve.wildcat.metadata.module.query;

import org.skyve.metadata.module.query.BizQLDefinition;

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
