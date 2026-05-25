package org.skyve.domain.app.admin;

import org.skyve.domain.PersistentBean;

/**
 * Domain contract for reusable tagging records applied to business documents.
 */
public interface Tag extends PersistentBean {
	long countDocument(String moduleName, String documentName);
	
	void setName(String name);
	void setVisible(Boolean visible);
}
