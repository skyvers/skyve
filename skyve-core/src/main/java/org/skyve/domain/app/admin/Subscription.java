package org.skyve.domain.app.admin;

import org.skyve.domain.PersistentBean;

public interface Subscription extends PersistentBean {
	Boolean getDeclined();
	String getPreferredReceiverIdentifier();
}
