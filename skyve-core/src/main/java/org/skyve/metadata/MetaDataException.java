package org.skyve.metadata;

import org.skyve.domain.messages.SkyveException;

/**
 * 
 */
/**
 * Signals an error in metadata loading, parsing, or validation.
 *
 * <p>Thrown by the repository and metadata processing pipelines when XML metadata
 * is malformed, references a non-existent target, or violates a Skyve constraint.
 * Because metadata errors are typically unrecoverable at startup, this exception
 * extends {@link org.skyve.domain.messages.SkyveException} and is usually propagated
 * to the deployment log.
 */
public class MetaDataException extends SkyveException {
	private static final long serialVersionUID = 1447684367914523647L;

	/**
	 * 
	 * @param cause
	 */
	public MetaDataException(Throwable cause) {
		super(cause);
	}

	/**
	 * 
	 * @param message
	 */
	public MetaDataException(String message) {
		super(message);
	}

	/**
	 * 
	 * @param message
	 * @param cause
	 */
	public MetaDataException(String message, Throwable cause) {
		super(message, cause);
	}

	public MetaDataException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
		super(message, cause, enableSuppression, writableStackTrace);
	}

	public MetaDataException(String message, String... i18nValues) {
		super(message, i18nValues);
	}

	public MetaDataException(String message, Throwable cause, String... i18nValues) {
		super(message, cause, i18nValues);
	}

	public MetaDataException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace, String... i18nValues) {
		super(message, cause, enableSuppression, writableStackTrace, i18nValues);
	}

	public MetaDataException(String message, boolean i18n) {
		super(message, i18n);
	}

	public MetaDataException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace, boolean i18n) {
		super(message, cause, enableSuppression, writableStackTrace, i18n);
	}

	public MetaDataException(String message, Throwable cause, boolean i18n) {
		super(message, cause, i18n);
	}
}
