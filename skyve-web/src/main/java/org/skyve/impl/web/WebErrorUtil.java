package org.skyve.impl.web;

import java.util.UUID;

import org.apache.commons.text.StringEscapeUtils;
import org.skyve.domain.messages.MessageException;
import org.skyve.util.OWASP;
import org.slf4j.Logger;

/**
 * Centralises client-safe web error messages.
 * <p>
 * Logging context should contain enough non-sensitive support information to
 * identify the failing operation, such as servlet, operation, module, document,
 * action or user name. Do not include session tokens, CSRF tokens, OTP values,
 * passwords, request cookies, raw authorization headers or full request bodies.
 */
public final class WebErrorUtil {
	public static final String GENERIC_ERROR_MESSAGE = "An error occurred while processing your request.";
	public static final String ERROR_REFERENCE_PARAMETER = "errorReference";

	private WebErrorUtil() {
		// Prevent instantiation.
	}

	/**
	 * Generates a new opaque support reference for correlating a client-safe
	 * error response with the corresponding server log entry.
	 *
	 * @return A random UUID string suitable for display to a user.
	 */
	public static String newErrorReference() {
		return UUID.randomUUID().toString();
	}

	/**
	 * Builds the generic client-safe error message for a support reference.
	 *
	 * @param reference The support reference already written to the server log.
	 * @return A generic error message containing the supplied reference.
	 */
	public static String genericMessage(String reference) {
		return new StringBuilder(GENERIC_ERROR_MESSAGE.length() + reference.length() + 12)
				.append(GENERIC_ERROR_MESSAGE)
				.append(" Reference: ")
				.append(reference)
				.toString();
	}

	/**
	 * Appends an error reference query parameter to a URI.
	 *
	 * @param uri The base URI to append to.
	 * @param reference The support reference to include in the query string.
	 * @return The URI with the error reference parameter appended.
	 */
	public static String appendErrorReference(String uri, String reference) {
		StringBuilder result = new StringBuilder(uri);
		result.append(uri.indexOf('?') >= 0 ? '&' : '?');
		result.append(ERROR_REFERENCE_PARAMETER).append('=').append(reference);
		return result.toString();
	}

	/**
	 * Logs an unexpected exception with a known support reference.
	 * <p>
	 * The context should identify the failing operation without including
	 * secrets, credentials, tokens, cookies or full request bodies.
	 *
	 * @param logger The logger to write to.
	 * @param reference The support reference that will be shown to the client.
	 * @param context Non-sensitive context describing the failing operation.
	 * @param t The unexpected exception or error.
	 */
	public static void logUnexpected(Logger logger, String reference, String context, Throwable t) {
		logger.error("{} Reference: {}", context, reference, t);
	}

	/**
	 * Generates a support reference and logs an unexpected exception with it.
	 * <p>
	 * Use the returned reference in the client-safe response so support can
	 * correlate the user-visible message with the detailed server log entry.
	 *
	 * @param logger The logger to write to.
	 * @param context Non-sensitive context describing the failing operation.
	 * @param t The unexpected exception or error.
	 * @return The generated support reference.
	 */
	public static String logUnexpectedAndGetReference(Logger logger, String context, Throwable t) {
		String reference = newErrorReference();
		logUnexpected(logger, reference, context, t);
		return reference;
	}

	/**
	 * Logs a failed request, generating a support reference only when the failure is unexpected.
	 * <p>
	 * A {@link MessageException}, such as a validation failure, is expected: its messages are shown to the
	 * user, so it is logged at warn level, as {@code FacesAction} does, and no reference is generated.
	 * Anything else is logged at error level with a new support reference, as
	 * {@link #logUnexpectedAndGetReference(Logger, String, Throwable)} does.
	 *
	 * @param logger The logger to write to.
	 * @param context Non-sensitive context describing the failing operation.
	 * @param t The exception or error that failed the request.
	 * @return The generated support reference, or null if {@code t} is a {@link MessageException}.
	 */
	public static String logFailureAndGetReference(Logger logger, String context, Throwable t) {
		if (t instanceof MessageException) {
			logger.warn(context, t);
			return null;
		}
		return logUnexpectedAndGetReference(logger, context, t);
	}

	/**
	 * Escapes text for safe inclusion in a JavaScript string literal.
	 *
	 * @param value The value to escape.
	 * @return The escaped value.
	 */
	public static String escapeJsStringWithHtmlFormatting(String value) {
		return OWASP.escapeJsStringWithHtmlFormatting(value);
	}

	/**
	 * Escapes text for safe inclusion as XML text content.
	 *
	 * @param value The value to escape.
	 * @return The escaped value.
	 */
	public static String escapeXmlText(String value) {
		return StringEscapeUtils.escapeXml10(value);
	}
}
