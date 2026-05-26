package org.skyve.domain.messages;

/**
 * Severity level for informational, warning, error, and fatal notifications displayed
 * in the Skyve UI.
 *
 * <p>Used by the web layer to style notification banners and client-side alerts.
 * {@code info} and {@code warn} are non-blocking; {@code error} and {@code fatal}
 * indicate problems that require user attention.
 */
@SuppressWarnings("java:S115") // Suppress "Constant names should comply with a naming convention" as these are not constants but enum values
public enum MessageSeverity {
	info, warn, error, fatal
}
