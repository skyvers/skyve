package org.skyve.domain.app.admin;

import org.skyve.domain.PersistentBean;

public interface Tagged extends PersistentBean {
	public static final String MODULE_NAME = Contact.MODULE_NAME;
	public static final String DOCUMENT_NAME = "Tagged";

	public static final String tagPropertyName = "tag";
	public static final String taggedModulePropertyName = "taggedModule";
	public static final String taggedDocumentPropertyName = "taggedDocument";
	public static final String taggedBizIdPropertyName = "taggedBizId";
}
