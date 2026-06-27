package org.skyve.domain.app.admin;

import org.skyve.CORE;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.app.AppConstants;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.Timestamp;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;

import jakarta.annotation.Nonnull;

/**
 * Domain contract for security audit log entries captured by the platform.
 */
public interface SecurityLog extends PersistentBean {

	/**
	 * Creates a new security log instance using the current runtime user.
	 *
	 * @return a new mutable security log instance
	 */
	public static @Nonnull SecurityLog newInstance() {
		return newInstance(CORE.getUser());
	}

	/**
	 * Creates a new security log instance for the supplied user context.
	 *
	 * @param user the user context used to resolve customer/module/document metadata
	 * @return a new mutable security log instance
	 * @throws DomainException when metadata lookup or instantiation fails
	 */
	public static @Nonnull SecurityLog newInstance(@Nonnull org.skyve.metadata.user.User user) {
		try {
			Customer customer = user.getCustomer();
			org.skyve.metadata.module.Module module = customer.getModule(AppConstants.ADMIN_MODULE_NAME);
			Document document = module.getDocument(customer, AppConstants.SECURITY_LOG_DOCUMENT_NAME);

			return document.newInstance(user);
		} catch (Exception e) {
			throw new DomainException("Could not instantiate a new SecurityLog", e);
		}
	}

	/**
	 * Returns when the security event occurred.
	 *
	 * @return the security event timestamp
	 */
	Timestamp getTimestamp();

	/**
	 * Returns the thread identifier that produced the event.
	 *
	 * @return the thread identifier
	 */
	Long getThreadId();

	/**
	 * Returns the thread name that produced the event.
	 *
	 * @return the thread name
	 */
	String getThreadName();

	/**
	 * Returns the source IP address captured for the event.
	 *
	 * @return the source IP address
	 */
	String getSourceIP();

	/**
	 * Returns the username associated with the event.
	 *
	 * @return the username
	 */
	String getUsername();

	/**
	 * Returns the business identifier of the logged-in user.
	 *
	 * @return the logged-in user business identifier
	 */
	String getLoggedInUserId();

	/**
	 * Returns the security event type.
	 *
	 * @return the security event type
	 */
	String getEventType();

	/**
	 * Returns the security event message.
	 *
	 * @return the security event message
	 */
	String getEventMessage();

	/**
	 * Returns provenance metadata for the event.
	 *
	 * @return the event provenance metadata
	 */
	String getProvenance();

	/**
	 * Sets when the security event occurred.
	 *
	 * @param timestamp the security event timestamp
	 */
	void setTimestamp(Timestamp timestamp);

	/**
	 * Sets the thread identifier that produced the event.
	 *
	 * @param threadId the thread identifier
	 */
	void setThreadId(Long threadId);

	/**
	 * Sets the thread name that produced the event.
	 *
	 * @param threadName the thread name
	 */
	void setThreadName(String threadName);

	/**
	 * Sets the source IP address captured for the event.
	 *
	 * @param sourceIP the source IP address
	 */
	void setSourceIP(String sourceIP);

	/**
	 * Sets the username associated with the event.
	 *
	 * @param username the username
	 */
	void setUsername(String username);

	/**
	 * Sets the business identifier of the logged-in user.
	 *
	 * @param bizId the logged-in user business identifier
	 */
	void setLoggedInUserId(String bizId);

	/**
	 * Sets provenance metadata for the event.
	 *
	 * @param provenance the event provenance metadata
	 */
	void setProvenance(String provenance);

	/**
	 * Sets the security event type.
	 *
	 * @param eventType the security event type
	 */
	void setEventType(String eventType);

	/**
	 * Sets the security event message.
	 *
	 * @param message the security event message
	 */
	void setEventMessage(String message);
}
