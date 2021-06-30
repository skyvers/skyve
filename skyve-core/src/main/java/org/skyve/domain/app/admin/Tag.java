package org.skyve.domain.app.admin;

import org.skyve.domain.PersistentBean;

public interface Tag extends PersistentBean {
	public static final String MODULE_NAME = Contact.MODULE_NAME;
	public static final String DOCUMENT_NAME = "Tag";
	
	public static final String namePropertyName = "name";
	
	long countDocument(String moduleName, String documentName);
	
	void setName(String name);
	void setVisible(Boolean visible);
}
