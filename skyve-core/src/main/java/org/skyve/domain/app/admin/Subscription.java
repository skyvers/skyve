package org.skyve.domain.app.admin;

import org.skyve.domain.PersistentBean;

/**
 * Domain contract for user subscriptions to data/events in the admin module.
 */
public interface Subscription extends PersistentBean {
	Boolean getDeclined();
	String getPreferredReceiverIdentifier();
}
