package org.skyve.impl.script;

import org.skyve.domain.messages.SkyveException;

/**
 * Represents interpreter and processing errors encountered while handling Skyve scripts.
 */
public class SkyveScriptException extends SkyveException {

	private static final long serialVersionUID = -8388742662234355631L;

	private ExceptionType type;
	private int lineNumber;

	/**
	 * Creates a script exception with explicit type, message and source line.
	 *
	 * @param type Exception severity/type.
	 * @param message Error message.
	 * @param lineNumber Source line number.
	 */
	public SkyveScriptException(final ExceptionType type, final String message, final int lineNumber) {
		super(message);
		this.lineNumber = lineNumber;
		this.type = type;
	}

	/**
	 * Creates a script exception with i18n substitution values.
	 *
	 * @param type Exception severity/type.
	 * @param message Error message key/text.
	 * @param lineNumber Source line number.
	 * @param i18nValues Substitution values.
	 */
	public SkyveScriptException(final ExceptionType type, final String message, final int lineNumber, String... i18nValues) {
		super(message, i18nValues);
		this.lineNumber = lineNumber;
		this.type = type;
	}

	/**
	 * Creates a script exception with explicit i18n mode.
	 *
	 * @param type Exception severity/type.
	 * @param message Error message.
	 * @param lineNumber Source line number.
	 * @param i18n Whether the message is an i18n key.
	 */
	public SkyveScriptException(final ExceptionType type, final String message, final int lineNumber, boolean i18n) {
		super(message, i18n);
		this.lineNumber = lineNumber;
		this.type = type;
	}

	/**
	 * Returns the script source line associated with this exception.
	 *
	 * @return The 1-based line number.
	 */
	public int getLineNumber() {
		return lineNumber;
	}

	/**
	 * Returns the severity/category for this script exception.
	 *
	 * @return The exception type.
	 */
	public ExceptionType getType() {
		return type;
	}

	/**
	 * Classifies script exceptions by severity for rendering and handling.
	 */
	public enum ExceptionType {
		/** Informational event with no action required. */
		info,
		/** Recoverable issue that should be surfaced to the script author. */
		warning,
		/** Non-recoverable script error for the current operation. */
		error,
		/** Critical error requiring immediate attention. */
		critical
	}
}
