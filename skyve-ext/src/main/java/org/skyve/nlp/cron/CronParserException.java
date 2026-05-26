package org.skyve.nlp.cron;

/**
 * Signals that natural-language input cannot be transformed into a valid cron expression.
 */
public class CronParserException extends RuntimeException {
	private static final long serialVersionUID = -6568272503153931665L;

	/**
	 * Creates a parser exception with a human-readable validation message.
	 *
	 * @param message The parse failure reason.
	 */
	public CronParserException(String message) {
		super(message);
	}
}
