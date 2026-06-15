package org.skyve.domain.messages;

import org.skyve.util.Util;

import jakarta.annotation.Nonnull;

/**
 * Abstract base class for all Skyve runtime exceptions.
 *
 * <p>Every constructor passes the message string through
 * {@link org.skyve.util.Util#nullSafeI18n} so that message strings may be either plain
 * text or i18n resource bundle keys. The framework resolves i18n keys against the
 * current user's locale at exception construction time.
 *
 * <p>Convenience constructors accept optional {@code i18nValues} varargs that are
 * substituted as positional parameters ({@code {0}}, {@code {1}}, …) in the resolved
 * i18n message string.
 *
 * <p>Constructors that accept a boolean {@code i18n} flag allow callers to opt out of
 * i18n resolution when passing a pre-formatted literal message (pass {@code false}).
 *
 * <p>Extends {@link RuntimeException} so it propagates unchecked through the framework's
 * action and persistence pipelines.
 */
public abstract class SkyveException extends RuntimeException {
	private static final long serialVersionUID = 3326193539360595441L;

	private static final String GENERIC_MESSAGE_KEY = "exception.generic";
	
	/**
	 * Creates a new SkyveException instance.
	 */
	public SkyveException() {
		// nothing to see here
	}

	/**
	 * Creates a new SkyveException instance.
	 * @param message the message
	 * @param cause the cause
	 * @param enableSuppression the enableSuppression
	 * @param writableStackTrace the writableStackTrace
	 */
	public SkyveException(@Nonnull String message,
							@Nonnull Throwable cause,
							boolean enableSuppression,
							boolean writableStackTrace) {
		super(Util.nullSafeI18n(message), cause, enableSuppression, writableStackTrace);
	}

	/**
	 * Creates a new SkyveException instance.
	 * @param message the message
	 * @param cause the cause
	 */
	public SkyveException(@Nonnull String message, @Nonnull Throwable cause) {
		super(Util.nullSafeI18n(message), cause);
	}

	/**
	 * Creates a new SkyveException instance.
	 * @param message the message
	 */
	public SkyveException(@Nonnull String message) {
		super(Util.nullSafeI18n(message));
	}

	/**
	 * Creates a new SkyveException instance.
	 * @param cause the cause
	 */
	public SkyveException(@Nonnull Throwable cause) {
		super(Util.nullSafeI18n(GENERIC_MESSAGE_KEY), cause);
	}
	
	/**
	 * Creates a new SkyveException instance.
	 * @param message the message
	 * @param cause the cause
	 * @param enableSuppression the enableSuppression
	 * @param writableStackTrace the writableStackTrace
	 * @param i18nValues the i18nValues
	 */
	public SkyveException(@Nonnull String message,
							@Nonnull Throwable cause,
							boolean enableSuppression,
							boolean writableStackTrace,
							String... i18nValues) {
		super(Util.nullSafeI18n(message, i18nValues), cause, enableSuppression, writableStackTrace);
	}

	/**
	 * Creates a new SkyveException instance.
	 * @param message the message
	 * @param cause the cause
	 * @param i18nValues the i18nValues
	 */
	public SkyveException(@Nonnull String message, @Nonnull Throwable cause, String... i18nValues) {
		super(Util.nullSafeI18n(message, i18nValues), cause);
	}

	/**
	 * Creates a new SkyveException instance.
	 * @param message the message
	 * @param i18nValues the i18nValues
	 */
	public SkyveException(@Nonnull String message, String... i18nValues) {
		super(Util.nullSafeI18n(message, i18nValues));
	}

	/**
	 * Creates a new SkyveException instance.
	 * @param message the message
	 * @param cause the cause
	 * @param enableSuppression the enableSuppression
	 * @param writableStackTrace the writableStackTrace
	 * @param i18n the i18n
	 */
	public SkyveException(@Nonnull String message,
							@Nonnull Throwable cause,
							boolean enableSuppression,
							boolean writableStackTrace,
							boolean i18n) {
		super(i18n ? Util.nullSafeI18n(message) : message, cause, enableSuppression, writableStackTrace);
	}

	/**
	 * Creates a new SkyveException instance.
	 * @param message the message
	 * @param cause the cause
	 * @param i18n the i18n
	 */
	public SkyveException(@Nonnull String message, @Nonnull Throwable cause, boolean i18n) {
		super(i18n ? Util.nullSafeI18n(message) : message, cause);
	}

	/**
	 * Creates a new SkyveException instance.
	 * @param message the message
	 * @param i18n the i18n
	 */
	public SkyveException(@Nonnull String message, boolean i18n) {
		super(i18n ? Util.nullSafeI18n(message) : message);
	}
}
