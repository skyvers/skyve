package org.skyve.wildcat.metadata.module.menu;

public class GridItem extends AbstractMenuItem {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -7998788239200957754L;

	private String documentName;
	private String queryName;
	private String modelName;

	public String getDocumentName() {
		return documentName;
	}
	public void setDocumentName(String documentName) {
		this.documentName = documentName;
	}

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
