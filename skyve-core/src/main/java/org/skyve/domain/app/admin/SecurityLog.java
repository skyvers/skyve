package org.skyve.domain.app.admin;

import org.skyve.CORE;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.app.AppConstants;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.Timestamp;

public interface SecurityLog extends PersistentBean {

	public static SecurityLog newInstance() {
		try {
			return CORE.getUser()
					.getCustomer()
					.getModule(AppConstants.ADMIN_MODULE_NAME)
					.getDocument(CORE.getUser().getCustomer(), AppConstants.SECURITY_LOG_DOCUMENT_NAME)
					.newInstance(CORE.getUser());
		} catch (RuntimeException e) {
			throw e;
		} catch (Exception e) {
			throw new DomainException(e);
		}
	}

	Timestamp getTimestamp();
	Long getThreadID();
	String getSourceIP();
	String getUsername();
	String getLoggedInUserId();
	String getEventType();
	String getEventMessage();
	String getProvenance();

	void setTimestamp(Timestamp timestamp);
	void setThreadID(Long threadID);
	void setSourceIP(String sourceIP);
	void setUsername(String username);
	void setLoggedInUserId(String bizId);
	void setProvenance(String provenance);
	void setEventType(String eventType);
	void setEventMessage(String message);
}
