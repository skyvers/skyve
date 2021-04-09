package org.skyve.domain.messages;

import org.skyve.util.Util;

public abstract class SkyveException extends RuntimeException {
	private static final long serialVersionUID = 3326193539360595441L;

	private static final String GENERIC_MESSAGE_KEY = "exception.generic";
	
	public SkyveException() {
		// nothing to see here
	}

	public SkyveException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
		super(Util.i18n(message), cause, enableSuppression, writableStackTrace);
	}

	public SkyveException(String message, Throwable cause) {
		super(Util.i18n(message), cause);
	}

	public SkyveException(String message) {
		super(Util.i18n(message));
	}

	public SkyveException(Throwable cause) {
		super(Util.i18n(GENERIC_MESSAGE_KEY), cause);
	}
	
	public SkyveException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace, String... i18nValues) {
		super(Util.i18n(message, i18nValues), cause, enableSuppression, writableStackTrace);
	}

	public SkyveException(String message, Throwable cause, String... i18nValues) {
		super(Util.i18n(message, i18nValues), cause);
	}

	public SkyveException(String message, String... i18nValues) {
		super(Util.i18n(message, i18nValues));
	}

	public SkyveException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace, boolean i18n) {
		super(i18n ? Util.i18n(message) : message, cause, enableSuppression, writableStackTrace);
	}

	public SkyveException(String message, Throwable cause, boolean i18n) {
		super(i18n ? Util.i18n(message) : message, cause);
	}

	public SkyveException(String message, boolean i18n) {
		super(i18n ? Util.i18n(message) : message);
	}
}
