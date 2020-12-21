package org.skyve.domain.messages;

import java.util.Locale;

import org.skyve.CORE;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.user.User;
import org.skyve.util.Util;

public abstract class SkyveException extends RuntimeException {
	private static final long serialVersionUID = 3326193539360595441L;

	private static final String GENERIC_MESSAGE_KEY = "exception.generic";
	
	private static String i18n(String message, String... values) {
		User u = (UtilImpl.SKYVE_PERSISTENCE_CLASS == null) ? null : CORE.getUser();
		Locale l = (u == null) ? Locale.ENGLISH : u.getLocale();
		return Util.i18n(message, l, values);
	}
	
	public SkyveException() {
	}

	public SkyveException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
		super(i18n(message), cause, enableSuppression, writableStackTrace);
	}

	public SkyveException(String message, Throwable cause) {
		super(i18n(message), cause);
	}

	public SkyveException(String message) {
		super(i18n(message));
	}

	public SkyveException(Throwable cause) {
		super(i18n(GENERIC_MESSAGE_KEY), cause);
	}
	
	public SkyveException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace, String... i18nValues) {
		super(i18n(message, i18nValues), cause, enableSuppression, writableStackTrace);
	}

	public SkyveException(String message, Throwable cause, String... i18nValues) {
		super(i18n(message, i18nValues), cause);
	}

	public SkyveException(String message, String... i18nValues) {
		super(i18n(message, i18nValues));
	}

	public SkyveException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace, boolean i18n) {
		super(i18n ? i18n(message) : message, cause, enableSuppression, writableStackTrace);
	}

	public SkyveException(String message, Throwable cause, boolean i18n) {
		super(i18n ? i18n(message) : message, cause);
	}

	public SkyveException(String message, boolean i18n) {
		super(i18n ? i18n(message) : message);
	}
}
