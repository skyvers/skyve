package org.skyve.domain.app.admin;

import org.skyve.domain.PersistentBean;

public interface User extends PersistentBean {
	Contact getContact();
}
