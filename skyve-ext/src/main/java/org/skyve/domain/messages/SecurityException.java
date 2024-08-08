package org.skyve.domain.messages;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.app.admin.SecurityLog;
import org.skyve.domain.app.admin.SecurityLog.ExceptionType;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.persistence.hibernate.AbstractHibernatePersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.HttpServletRequestResponse;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.skyve.util.Mail;
import org.skyve.util.SecurityUtil;
import org.skyve.util.Util;

import jakarta.servlet.http.HttpServletRequest;

/**
 * Thrown when the current user does not have privileges to 
 * a perform an operation on a data entity or web resource.
 */
public class SecurityException extends DomainException {
	private static final long serialVersionUID = 2941808458696267548L;

	public SecurityException(String resource, String userName) {
		super(userName + " does not have access to " + resource);
		log();
	}

	/**
	 * Creates a {@link SecurityLog} entry and emails a link to it (if email is configured)
	 *
	 * @author Simeon Solomou
	 */
	private void log() {
		// Get current persistence
		Persistence p = CORE.getPersistence();
		User user = p.getUser();

		// Create a new, temporary persistence
		AbstractHibernatePersistence tempP = (AbstractHibernatePersistence) AbstractPersistence.newInstance();
		try {
			// Setting user and beginning transaction
			tempP.setUser(user);
			tempP.begin();

			try {
				SecurityLog sl = SecurityLog.newInstance();

				// Timestamp
				sl.setTimestamp(new Timestamp());

				// Thread ID
				Long threadID = Long.valueOf(Thread.currentThread()
						.getId());
				sl.setThreadID(threadID);

				// Source IP
				HttpServletRequestResponse requestResponse = EXT.getHttpServletRequestResponse();
				if (requestResponse == null) {
					Util.LOGGER.severe("Failed to get HTTP request/response");
				} else {
					HttpServletRequest request = requestResponse.getRequest();
					if (request == null) {
						Util.LOGGER.severe("Failed to get HTTP request");
					} else {
						sl.setSourceIP(SecurityUtil.getSourceIpAddress(request));
					}
				}

				// Username
				sl.setUsername(user.getName());

				// Logged in user (ID)
				sl.setLoggedInUserId(user.getId());

				// Exception type
				if (this instanceof AccessException) {
					sl.setExceptionType(ExceptionType.accessException);
				} else {
					sl.setExceptionType(ExceptionType.securityException);
				}

				// Exception message
				sl.setExceptionMessage(this.getMessage());
				
				// Provenance
				sl.setProvenance(SecurityUtil.getProvenance(this));

				try {
					// Upsert
					tempP.upsertBeanTuple(sl);
				} catch (Exception e) {
					Util.LOGGER.severe("Failed to save security log entry");
					e.printStackTrace();
				}

				try {
					// Email
					email(sl);
				} catch (Exception e) {
					Util.LOGGER.severe("Failed to email security log entry");
					e.printStackTrace();
				}
			} catch (Exception e) {
				Util.LOGGER.severe("Failed to create security log entry");
				e.printStackTrace();
			} finally {
				try {
					tempP.commit(false);
				} catch (Exception e) {
					Util.LOGGER.severe("Failed to commit temporary persistence");
					e.printStackTrace();
				}
			}
		} finally {
			try {
				tempP.close();
			} catch (Exception e) {
				Util.LOGGER.severe("Failed to close temporary persistence");
				e.printStackTrace();
			}
		}
	}

	/**
	 * Sends an email to the support email address notifying of newly logged {@link SecurityException}.
	 * <br/>
	 * If either support email is not specified, or email is not configured, a warning is logged.
	 * 
	 * @param sl The newly created security log
	 * @author Simeon Solomou
	 */
	private static void email(SecurityLog sl) {
		String supportEmail = UtilImpl.SUPPORT_EMAIL_ADDRESS;
		if (supportEmail == null) {
			Util.LOGGER.warning("Cannot send security log notification as no support email address is specified");
			return;
		}
		if ("localhost".equals(UtilImpl.SMTP)) {
			Util.LOGGER.warning("Cannot send security log notification as email is not configured");
			return;
		}

		// Format email content
		StringBuilder body = new StringBuilder();
		body.append("A security exception has been logged:<br/><br/>");
		body.append("Timestamp: ").append(sl.getTimestamp()).append("<br/>");
		body.append("Thread ID: ").append(sl.getThreadID()).append("<br/>");
		body.append("Source IP: ").append(sl.getSourceIP()).append("<br/>");
		body.append("Username: ").append(sl.getUsername()).append("<br/>");
		body.append("Logged in User ID: ").append(sl.getLoggedInUserId()).append("<br/>");
		body.append("Exception Type: ").append(sl.getExceptionType().toLocalisedDescription()).append("<br/>");
		body.append("Exception Message: ").append(sl.getExceptionMessage()).append("<br/>");
		body.append("Provenance: ").append(sl.getProvenance()).append("<br/>");

		// Send
		EXT.sendMail(new Mail().from(UtilImpl.SMTP_SENDER)
				.addTo(supportEmail)
				.subject("New security log entry")
				.body(body.toString()));
	}
}
