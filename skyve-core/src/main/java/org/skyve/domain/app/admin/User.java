package org.skyve.domain.app.admin;

import org.skyve.domain.PersistentBean;

public interface User extends PersistentBean {
	public static final String MODULE_NAME = Contact.MODULE_NAME;
	public static final String DOCUMENT_NAME = "User";

	Contact getContact();
}
