package org.skyve.wildcat.metadata.module.menu;

public class CalendarItem extends AbstractMenuItem {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -7998788239200957754L;

	private String documentName;
	private String queryName;
	private String modelName;
	private String startBinding;
	private String endBinding;

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

	public String getStartBinding() {
		return startBinding;
	}
	public void setStartBinding(String startBinding) {
		this.startBinding = startBinding;
	}

	public String getEndBinding() {
		return endBinding;
	}
	public void setEndBinding(String endBinding) {
		this.endBinding = endBinding;
	}
}
