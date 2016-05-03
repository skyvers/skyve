package org.skyve.impl.metadata.module.menu;

public abstract class AbstractDocumentOrQueryOrModelMenuItem extends AbstractDocumentMenuItem {
	private static final long serialVersionUID = 840971349939181558L;

	private String queryName;
	private String modelName;

	public String getQueryName() {
		return queryName;
	}
	public void setQueryName(String queryName) {
		this.queryName = queryName;
	}

	public String getModelName() {
		return modelName;
	}
	public void setModelName(String modelName) {
		this.modelName = modelName;
	}
}
