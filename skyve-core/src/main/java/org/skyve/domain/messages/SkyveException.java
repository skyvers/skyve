package org.skyve.domain.messages;

import org.skyve.util.Util;

import jakarta.annotation.Nonnull;

public abstract class SkyveException extends RuntimeException {
	private static final long serialVersionUID = 3326193539360595441L;

	private static final String GENERIC_MESSAGE_KEY = "exception.generic";
	
	public SkyveException() {
		// nothing to see here
	}

	public SkyveException(@Nonnull String message,
							@Nonnull Throwable cause,
							boolean enableSuppression,
							boolean writableStackTrace) {
		super(Util.nullSafeI18n(message), cause, enableSuppression, writableStackTrace);
	}

	public SkyveException(@Nonnull String message, @Nonnull Throwable cause) {
		super(Util.nullSafeI18n(message), cause);
	}

	public SkyveException(@Nonnull String message) {
		super(Util.nullSafeI18n(message));
	}

	public SkyveException(@Nonnull Throwable cause) {
		super(Util.nullSafeI18n(GENERIC_MESSAGE_KEY), cause);
	}
	
	public SkyveException(@Nonnull String message,
							@Nonnull Throwable cause,
							boolean enableSuppression,
							boolean writableStackTrace,
							String... i18nValues) {
		super(Util.nullSafeI18n(message, i18nValues), cause, enableSuppression, writableStackTrace);
	}

	public SkyveException(@Nonnull String message, @Nonnull Throwable cause, String... i18nValues) {
		super(Util.nullSafeI18n(message, i18nValues), cause);
	}

	public SkyveException(@Nonnull String message, String... i18nValues) {
		super(Util.nullSafeI18n(message, i18nValues));
	}

	public SkyveException(@Nonnull String message,
							@Nonnull Throwable cause,
							boolean enableSuppression,
							boolean writableStackTrace,
							boolean i18n) {
		super(i18n ? Util.nullSafeI18n(message) : message, cause, enableSuppression, writableStackTrace);
	}

	public SkyveException(@Nonnull String message, @Nonnull Throwable cause, boolean i18n) {
		super(i18n ? Util.nullSafeI18n(message) : message, cause);
	}

	public SkyveException(@Nonnull String message, boolean i18n) {
		super(i18n ? Util.nullSafeI18n(message) : message);
	}
}
