package org.skyve.util;

import org.apache.commons.text.StringTokenizer;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.app.admin.SecurityLog;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.persistence.hibernate.AbstractHibernatePersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.HttpServletRequestResponse;
import org.skyve.impl.web.WebContainer;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.servlet.http.HttpServletRequest;

public class SecurityUtil {

	/**
	 * Creates a {@link SecurityLog} entry and emails its contents to the defined support user.
	 * 
	 * @param exception The exception raised for this security event
	 */
	public static void log(@Nonnull Exception exception) {
		// Get human-readable exception name
		String eventType = exception.getClass()
				.getSimpleName()
				.replaceAll("([a-z])([A-Z]+)", "$1 $2");

		String eventMessage = exception.getMessage();
		String provenance = getProvenance(exception);

		log(eventType, eventMessage, provenance);
	}

	/**
	 * Creates a {@link SecurityLog} entry and emails its contents to the defined support user.
	 * 
	 * @param eventType The type of security event
	 * @param eventMessage What is this security event
	 */
	public static void log(@Nonnull String eventType, @Nonnull String eventMessage) {
		log(eventType, eventMessage, null);
	}

	/**
	 * Creates a {@link SecurityLog} entry and emails its contents to the defined support user.
	 *
	 * @param eventType The type of security event
	 * @param eventMessage What is this security event
	 * @param provenance The first line of the stack trace
	 * 
	 * @author Simeon Solomou
	 */
	private static void log(@Nonnull String eventType, @Nonnull String eventMessage, @Nullable String provenance) {
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

				Thread currentThread = Thread.currentThread();
				if (currentThread != null) {
					// Thread ID
					sl.setThreadId(Long.valueOf(currentThread.getId()));

					// Thread Name
					sl.setThreadName(currentThread.getName());
				}

				// Source IP
				HttpServletRequestResponse requestResponse = WebContainer.getHttpServletRequestResponse();
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

				// Event type
				sl.setEventType(eventType);

				// Event message
				sl.setEventMessage(eventMessage);

				// Provenance
				sl.setProvenance(provenance);

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
	 * Sends an email to the support email address notifying of the new {@link SecurityLog} entry.
	 * <br/>
	 * If either support email is not specified, or email is not configured, a warning is logged.
	 * 
	 * @param sl The newly created security log
	 * @author Simeon Solomou
	 */
	private static void email(@Nonnull SecurityLog sl) {
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
		body.append("Thread ID: ").append(sl.getThreadId()).append("<br/>");
		body.append("Thread Name: ").append(sl.getThreadName()).append("<br/>");
		body.append("Source IP: ").append(sl.getSourceIP()).append("<br/>");
		body.append("Username: ").append(sl.getUsername()).append("<br/>");
		body.append("Logged in User ID: ").append(sl.getLoggedInUserId()).append("<br/>");
		body.append("Event Type: ").append(sl.getEventType()).append("<br/>");
		body.append("Event Message: ").append(sl.getEventMessage()).append("<br/>");
		body.append("Provenance: ").append(sl.getProvenance()).append("<br/>");

		// Send
		EXT.sendMail(new Mail().from(UtilImpl.SMTP_SENDER)
				.addTo(supportEmail)
				.subject("New security log entry")
				.body(body.toString()));
	}

	/**
	 * Returns the source IP for the parsed {@link HttpServletRequest}
	 * 
	 * @param request
	 * @return source IP
	 */
	public static @Nonnull String getSourceIpAddress(@Nonnull HttpServletRequest request) {
		// Check "Forwarded" header
	    String forwardedHeader = request.getHeader("Forwarded");
	    if (forwardedHeader != null) {
	        // Parse the "Forwarded" header for the 'for' field
	        for (String part : forwardedHeader.split(";")) {
	            if (part.trim().startsWith("for=")) {
	                return part.substring(4).split(",")[0].trim();
	            }
	        }
	    }
	    
	    // Check "X-Forwarded-For" header
	    String xForwardedForHeader = request.getHeader("X-Forwarded-For");
	    if (xForwardedForHeader != null) {
	    	StringTokenizer tokenizer = new StringTokenizer(xForwardedForHeader, ",");
			if (tokenizer.hasNext()) {
                return tokenizer.nextToken().trim();
            }
	    }

		// If none are present, return the remote address
		return request.getRemoteAddr();
	}

	/**
	 * Returns the first line of the stack trace for the parsed {@link Exception}
	 * 
	 * @param e
	 * @return provenance
	 */
	public static @Nullable String getProvenance(@Nonnull Exception e) {
		StackTraceElement[] stackTrace = e.getStackTrace();
		if (stackTrace.length > 0) {
			StackTraceElement firstElement = stackTrace[0];
			return firstElement.toString();
		}
		return null;
	}
}