package org.skyve.domain.messages;

/**
 * General-purpose Skyve framework exception for unrecoverable domain and infrastructure errors.
 *
 * <p>Throw {@code DomainException} when an operation fails in a way that is not
 * expected to be recoverable by the caller and does not have a more specific exception
 * subclass. For user-facing validation errors use {@link ValidationException}; for
 * conversion errors use {@link ConversionException}; for concurrency conflicts use
 * {@link OptimisticLockException}.
 *
 * <p>All message strings are resolved through the Skyve i18n mechanism
 * (via the {@link SkyveException} constructors); pass an i18n resource key or literal
 * text as {@code message}.
 *
 * @see SkyveException
 * @see ValidationException
 */
public class DomainException extends SkyveException {
	private static final long serialVersionUID = -2523236450510857431L;

	/**
	 * Creates a new DomainException instance.
	 */
	protected DomainException() {
		// used in subclasses
	}

	/**
	 * Creates a new DomainException instance.
	 * @param message the message
	 */
	public DomainException(String message) {
		super(message);
	}

	/**
	 * Creates a new DomainException instance.
	 * @param t the t
	 */
	public DomainException(Throwable t) {
		super(t);
	}

	/**
	 * Creates a new DomainException instance.
	 * @param message the message
	 * @param t the t
	 */
	public DomainException(String message, Throwable t) {
		super(message, t);
	}

	/**
	 * Creates a new DomainException instance.
	 * @param message the message
	 * @param cause the cause
	 * @param enableSuppression the enableSuppression
	 * @param writableStackTrace the writableStackTrace
	 */
	public DomainException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
		super(message, cause, enableSuppression, writableStackTrace);
	}

	/**
	 * Creates a new DomainException instance.
	 * @param message the message
	 * @param i18nValues the i18nValues
	 */
	public DomainException(String message, String... i18nValues) {
		super(message, i18nValues);
	}

	/**
	 * Creates a new DomainException instance.
	 * @param message the message
	 * @param cause the cause
	 * @param i18nValues the i18nValues
	 */
	public DomainException(String message, Throwable cause, String... i18nValues) {
		super(message, cause, i18nValues);
	}

	/**
	 * Creates a new DomainException instance.
	 * @param message the message
	 * @param cause the cause
	 * @param enableSuppression the enableSuppression
	 * @param writableStackTrace the writableStackTrace
	 * @param i18nValues the i18nValues
	 */
	public DomainException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace, String... i18nValues) {
		super(message, cause, enableSuppression, writableStackTrace, i18nValues);
	}

	/**
	 * Creates a new DomainException instance.
	 * @param message the message
	 * @param i18n the i18n
	 */
	public DomainException(String message, boolean i18n) {
		super(message, i18n);
	}

	/**
	 * Creates a new DomainException instance.
	 * @param message the message
	 * @param cause the cause
	 * @param enableSuppression the enableSuppression
	 * @param writableStackTrace the writableStackTrace
	 * @param i18n the i18n
	 */
	public DomainException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace, boolean i18n) {
		super(message, cause, enableSuppression, writableStackTrace, i18n);
	}

	/**
	 * Creates a new DomainException instance.
	 * @param message the message
	 * @param cause the cause
	 * @param i18n the i18n
	 */
	public DomainException(String message, Throwable cause, boolean i18n) {
		super(message, cause, i18n);
	}
}
