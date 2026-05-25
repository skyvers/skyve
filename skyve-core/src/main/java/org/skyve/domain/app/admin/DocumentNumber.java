package org.skyve.domain.app.admin;

import org.skyve.domain.PersistentBean;

/**
 * Domain contract for document numbering sequences managed by the admin
 * module.
 */
public interface DocumentNumber extends PersistentBean {
	String getDocumentNumber();

	void setModuleName(String moduleName);
	void setDocumentName(String documentName);
	void setSequenceName(String fieldName);
	void setDocumentNumber(String nextNumber);

}
