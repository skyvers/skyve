package org.skyve.impl.web;

import java.util.UUID;

import org.apache.commons.text.StringEscapeUtils;
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

	public static String newErrorReference() {
		return UUID.randomUUID().toString();
	}

	public static String genericMessage(String reference) {
		return GENERIC_ERROR_MESSAGE + " Reference: " + reference;
	}

	public static String appendErrorReference(String uri, String reference) {
		StringBuilder result = new StringBuilder(uri);
		result.append(uri.indexOf('?') >= 0 ? '&' : '?');
		result.append(ERROR_REFERENCE_PARAMETER).append('=').append(reference);
		return result.toString();
	}

	public static void logUnexpected(Logger logger, String reference, String context, Throwable t) {
		logger.error("{} Reference: {}", context, reference, t);
	}

	public static String logUnexpectedAndGetReference(Logger logger, String context, Throwable t) {
		String reference = newErrorReference();
		logUnexpected(logger, reference, context, t);
		return reference;
	}

	public static String escapeJsString(String value) {
		return OWASP.escapeJsString(value);
	}

	public static String escapeXmlText(String value) {
		return StringEscapeUtils.escapeXml10(value);
	}
}
