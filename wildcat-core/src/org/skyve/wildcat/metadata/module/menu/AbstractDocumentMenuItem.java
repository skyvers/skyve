package org.skyve.wildcat.metadata.module.menu;

public abstract class AbstractDocumentMenuItem extends AbstractMenuItem {
	private static final long serialVersionUID = 2956568471144635374L;

	private String documentName;

	public String getDocumentName() {
		return documentName;
	}
	public void setDocumentName(String documentName) {
		this.documentName = documentName;
	}
}
