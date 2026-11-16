package org.skyve.domain.app.admin;

import org.skyve.domain.PersistentBean;

import jakarta.annotation.Nullable;

/**
 * Domain contract for an admin contact/person record.
 */
public interface Contact extends PersistentBean {
	@Nullable String getEmail1();
}
