package org.skyve.domain.app.admin;

import org.skyve.domain.PersistentBean;

public interface Tag extends PersistentBean {
	long countDocument(String moduleName, String documentName);
	
	void setName(String name);
	void setVisible(Boolean visible);
}
