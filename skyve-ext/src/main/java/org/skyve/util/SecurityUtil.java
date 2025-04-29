package org.skyve.util;

import java.util.HashMap;
import java.util.Map;

import org.apache.commons.text.StringTokenizer;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.app.admin.SecurityLog;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.persistence.hibernate.AbstractHibernatePersistence;
import org.skyve.impl.security.SkyveLegacyPasswordEncoder;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.HttpServletRequestResponse;
import org.skyve.impl.web.WebContainer;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.crypto.argon2.Argon2PasswordEncoder;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.DelegatingPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.crypto.password.Pbkdf2PasswordEncoder;
import org.springframework.security.crypto.scrypt.SCryptPasswordEncoder;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.servlet.http.HttpServletRequest;

/**
 * Utility class for handling security-related operations.
 * Provides functionality for security logging, password hashing, and IP address handling.
 */
public class SecurityUtil {

    private static final Logger LOGGER = LoggerFactory.getLogger(SecurityUtil.class);

	/**
	 * Creates a security log entry and optionally sends an email notification for the specified exception.
	 * The event type is derived from the exception class name, and the message is taken from the exception.
	 *
	 * @param exception The exception that triggered the security event
	 * @param email Whether to attempt sending an email notification
	 * @throws IllegalArgumentException if exception is null
	 */
	public static void log(@Nonnull Exception exception, boolean email) {
		// Get human-readable exception name
		String eventType = exception.getClass()
				.getSimpleName()
				.replaceAll("([a-z])([A-Z]+)", "$1 $2");

		String eventMessage = exception.getMessage();
		String provenance = getProvenance(exception);

		log(eventType, eventMessage, provenance, null, email);
	}

	/**
	 * Creates a security log entry and optionally sends an email notification for a security event.
	 * Uses the current user from the persistence context.
	 *
	 * @param eventType The type of security event (e.g., "Password Change")
	 * @param eventMessage A detailed description of the security event
	 * @param email Whether to attempt sending an email notification
	 * @throws IllegalArgumentException if eventType or eventMessage is null
	 */
	public static void log(@Nonnull String eventType, @Nonnull String eventMessage, boolean email) {
		log(eventType, eventMessage, null, null, email);
	}

	/**
	 * Creates a security log entry and optionally sends an email notification for a security event with a specific user.
	 * Use this method when the user for the security event is not available in the current persistence context.
	 *
	 * @param eventType The type of security event (e.g., "Password Change")
	 * @param eventMessage A detailed description of the security event
	 * @param user The user associated with this security event
	 * @param email Whether to attempt sending an email notification
	 * @throws IllegalArgumentException if eventType, eventMessage, or user is null
	 */
	public static void log(@Nonnull String eventType, @Nonnull String eventMessage, @Nonnull User user, boolean email) {
		log(eventType, eventMessage, null, user, email);
	}

	/**
	 * Creates a security log entry and optionally sends an email notification.
	 * Creates a new persistence instance to handle the logging operation.
	 *
	 * @param eventType The type of security event
	 * @param eventMessage A detailed description of the security event
	 * @param provenance The stack trace information for the event
	 * @param user The user associated with this security event (if null, uses current persistence user)
	 * @param email Whether to attempt sending an email notification
	 * @throws IllegalArgumentException if eventType or eventMessage is null
	 */
	private static void log(@Nonnull String eventType, @Nonnull String eventMessage, @Nullable String provenance, @Nullable User user, boolean email) {
		// Get current persistence
		User currentUser = user;
		if (currentUser == null) {
			Persistence p = CORE.getPersistence();
			currentUser = p.getUser();
		}

		// Create a new, temporary persistence
		AbstractHibernatePersistence tempP = (AbstractHibernatePersistence) AbstractPersistence.newInstance();
		try {
			// Setting user and beginning transaction
			tempP.setUser(currentUser);
			tempP.begin();

			try {
				SecurityLog sl = SecurityLog.newInstance(currentUser);

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
					LOGGER.error("Failed to get HTTP request/response");
				} else {
					HttpServletRequest request = requestResponse.getRequest();
					sl.setSourceIP(SecurityUtil.getSourceIpAddress(request));
				}

				// Username
				sl.setUsername(currentUser.getName());

				// Logged in user (ID)
				sl.setLoggedInUserId(currentUser.getId());

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
					LOGGER.error("Failed to save security log entry", e);
				}

				if (email) {
					try {
						// Email
						email(sl);
					} catch (Exception e) {
						LOGGER.error("Failed to email security log entry", e);
					}
				}
			} catch (Exception e) {
				LOGGER.error("Failed to create security log entry", e);
			} finally {
				try {
					tempP.commit(false);
				} catch (Exception e) {
					LOGGER.error("Failed to commit temporary persistence", e);
				}
			}
		} finally {
			try {
				tempP.close();
			} catch (Exception e) {
				LOGGER.error("Failed to close temporary persistence", e);
			}
		}
	}
	
	/**
	 * Sends an email notification about a security log entry to the configured security notifications or support email address.
	 * The email includes detailed information about the security event.
	 *
	 * @param sl The security log entry to be notified about
	 * @throws IllegalArgumentException if sl is null
	 */
	private static void email(@Nonnull SecurityLog sl) {
		// Retrieve email address
		String sendTo = UtilImpl.SECURITY_NOTIFICATIONS_EMAIL_ADDRESS;
		if (sendTo == null) {
			sendTo = UtilImpl.SUPPORT_EMAIL_ADDRESS;
			if (sendTo == null) {
				LOGGER.warn("Cannot send security log notification as no email address is specified.");
				return;
			}
		}

		// Check email configuration
		if ("localhost".equals(UtilImpl.SMTP)) {
			LOGGER.warn("Cannot send security log notification as email is not configured.");
			return;
		}

		// Extract event information
		Timestamp timestamp = sl.getTimestamp();
		Long threadId = sl.getThreadId();
		String threadName = sl.getThreadName();
		String sourceIP = sl.getSourceIP();
		String username = sl.getUsername();
		String loggedInUserId = sl.getLoggedInUserId();
		String eventType = sl.getEventType();
		String eventMessage = sl.getEventMessage();
		String provenance = sl.getProvenance();

		// Format email content
		StringBuilder body = new StringBuilder();
		body.append("A new security event has been logged:<br/><br/>");
		if (timestamp != null) {
			body.append("Timestamp: ").append(timestamp).append("<br/>");
		}
		if (threadId != null) {
			body.append("Thread ID: ").append(threadId).append("<br/>");
		}
		if (threadName != null) {
			body.append("Thread Name: ").append(threadName).append("<br/>");
		}
		if (sourceIP != null) {
			body.append("Source IP: ").append(sourceIP).append("<br/>");
		}
		if (username != null) {
			body.append("Username: ").append(username).append("<br/>");
		}
		if (loggedInUserId != null) {
			body.append("Logged in User ID: ").append(loggedInUserId).append("<br/>");
		}
		if (eventType != null) {
			body.append("Event Type: ").append(eventType).append("<br/>");
		}
		if (eventMessage != null) {
			body.append("Event Message: ").append(eventMessage).append("<br/>");
		}
		if (provenance != null) {
			body.append("Provenance: ").append(provenance).append("<br/>");
		}

		// Send
		EXT.sendMail(new Mail().from(UtilImpl.SMTP_SENDER)
				.addTo(sendTo)
				.subject("Security Log Entry - " + (eventType != null ? eventType : "Unknown"))
				.body(body.toString()));
	}

	/**
	 * Extracts the source IP address from an HTTP request, checking various headers in order:
	 * 1. "Forwarded" header
	 * 2. "X-Forwarded-For" header
	 * 3. Remote address
	 *
	 * @param request The HTTP request to extract the IP address from
	 * @return The source IP address as a string
	 * @throws IllegalArgumentException if request is null
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
	 * Extracts the first line of the stack trace from an exception to use as provenance information.
	 *
	 * @param e The exception to extract provenance from
	 * @return The first line of the stack trace, or null if no stack trace is available
	 * @throws IllegalArgumentException if e is null
	 */
	public static @Nullable String getProvenance(@Nonnull Exception e) {
		StackTraceElement[] stackTrace = e.getStackTrace();
		if (stackTrace.length > 0) {
			StackTraceElement firstElement = stackTrace[0];
			return firstElement.toString();
		}
		return null;
	}

	/**
	 * Create Skyve's version of Spring Security's DelegatingPasswordEncoder (from their PasswordEncoderFactories class)
	 * @return	Skyve's delegating password encoder
	 */
	public static @Nonnull PasswordEncoder createDelegatingPasswordEncoder() {
		String encodingId = "argon2";
		Map<String, PasswordEncoder> encoders = new HashMap<>();
		encoders.put("argon2", Argon2PasswordEncoder.defaultsForSpringSecurity_v5_8());
		encoders.put("bcrypt", new BCryptPasswordEncoder());
		encoders.put("pbkdf2", Pbkdf2PasswordEncoder.defaultsForSpringSecurity_v5_8());
		encoders.put("scrypt", SCryptPasswordEncoder.defaultsForSpringSecurity_v5_8());
		DelegatingPasswordEncoder result = new DelegatingPasswordEncoder(encodingId, encoders);

		// TODO Legacy hashing with no SALT - REMOVE when RevSA password time period expires 
		result.setDefaultPasswordEncoderForMatches(new SkyveLegacyPasswordEncoder());

		return result;
	}
	
	/**
	 * Provide a hash of a clear text password.
	 * 
	 * @param clearText
	 * @return	The encoded password.
	 */
	public static @Nonnull String hashPassword(@Nonnull String clearText) {
		String result = null;

		String passwordHashingAlgorithm = Util.getPasswordHashingAlgorithm();
		if ("argon2".equals(passwordHashingAlgorithm)) {
			result = "{argon2}" + Argon2PasswordEncoder.defaultsForSpringSecurity_v5_8().encode(clearText);
		}
		else if ("bcrypt".equals(passwordHashingAlgorithm)) {
			result = "{bcrypt}" + new BCryptPasswordEncoder().encode(clearText);
		}
		else if ("pbkdf2".equals(passwordHashingAlgorithm)) {
			result = "{pbkdf2}" + Pbkdf2PasswordEncoder.defaultsForSpringSecurity_v5_8().encode(clearText);
		}
		else if ("scrypt".equals(passwordHashingAlgorithm)) {
			result = "{scrypt}" + SCryptPasswordEncoder.defaultsForSpringSecurity_v5_8().encode(clearText);
		}
		// TODO Legacy hashing with no SALT - REMOVE when RevSA password time period expires 
		else if ("SHA1".equals(passwordHashingAlgorithm)) {
			result = SkyveLegacyPasswordEncoder.encode(clearText, passwordHashingAlgorithm);
		}
		else {
			throw new DomainException(passwordHashingAlgorithm + " not supported");
		}
		
		return result;
	}
}
