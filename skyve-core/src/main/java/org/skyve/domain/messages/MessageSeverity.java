package org.skyve.domain.messages;

/**
 * Severity level for informational, warning, error, and fatal notifications displayed
 * in the Skyve UI.
 *
 * <p>Used by the web layer to style notification banners and client-side alerts.
 * {@code info} and {@code warn} are non-blocking; {@code error} and {@code fatal}
 * indicate problems that require user attention.
 */
public enum MessageSeverity {
	info, warn, error, fatal
}
