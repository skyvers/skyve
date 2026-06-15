package org.skyve.domain.app.admin;

import org.skyve.domain.PersistentBean;

/**
 * Domain contract for application user accounts and profile/security settings.
 */
public interface User extends PersistentBean {
	Contact getContact();
}
