package org.skyve.domain.app.admin;

import org.skyve.domain.PersistentBean;

import jakarta.annotation.Nullable;

/**
 * Domain contract for application user accounts and profile/security settings.
 */
public interface User extends PersistentBean {
	@Nullable Contact getContact();
}
