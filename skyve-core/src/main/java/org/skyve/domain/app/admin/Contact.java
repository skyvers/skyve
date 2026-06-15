package org.skyve.domain.app.admin;

import org.skyve.domain.PersistentBean;

/**
 * Domain contract for an admin contact/person record.
 */
public interface Contact extends PersistentBean {
	String getEmail1();
}
