package org.skyve.domain.app.admin;

import org.skyve.CORE;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.app.AppConstants;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.Timestamp;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;

import jakarta.annotation.Nonnull;

public interface SecurityLog extends PersistentBean {

	public static @Nonnull SecurityLog newInstance() {
		return newInstance(CORE.getUser());
	}

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

	Timestamp getTimestamp();
	Long getThreadId();
	String getThreadName();
	String getSourceIP();
	String getUsername();
	String getLoggedInUserId();
	String getEventType();
	String getEventMessage();
	String getProvenance();

	void setTimestamp(Timestamp timestamp);
	void setThreadId(Long threadId);
	void setThreadName(String threadName);
	void setSourceIP(String sourceIP);
	void setUsername(String username);
	void setLoggedInUserId(String bizId);
	void setProvenance(String provenance);
	void setEventType(String eventType);
	void setEventMessage(String message);
}
