package org.skyve.domain.app.admin;

import org.skyve.domain.PersistentBean;

public interface Tag extends PersistentBean {
	public static final String MODULE_NAME = "admin";
	public static final String DOCUMENT_NAME = "Tag";
	
	long countDocument(String moduleName, String documentName);
}
