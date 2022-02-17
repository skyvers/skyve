package org.skyve.domain.app.admin;

import org.skyve.domain.PersistentBean;

public interface DocumentNumber extends PersistentBean {
	public static final String MODULE_NAME = "admin";
	public static final String DOCUMENT_NAME = "DocumentNumber";

	public static final String moduleNamePropertyName = "moduleName";
	public static final String documentNamePropertyName = "documentName";
	public static final String sequenceNamePropertyName = "sequenceName";
	public static final String documentNumberPropertyName = "documentNumber";

	String getDocumentNumber();

	void setModuleName(String moduleName);
	void setDocumentName(String documentName);
	void setSequenceName(String fieldName);
	void setDocumentNumber(String nextNumber);

}
