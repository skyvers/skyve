package org.skyve.domain.app.admin;

import org.skyve.domain.PersistentBean;

public interface Contact extends PersistentBean {
	public static final String MODULE_NAME = "admin";
	public static final String DOCUMENT_NAME = "Contact";

	String getEmail1();
}
