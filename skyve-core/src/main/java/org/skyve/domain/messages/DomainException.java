package org.skyve.domain.messages;

/**
 * 
 */
public class DomainException extends SkyveException {
	private static final long serialVersionUID = -2523236450510857431L;

	protected DomainException() {
		// used in subclasses
	}

	public DomainException(String message) {
		super(message);
	}

	public DomainException(Throwable t) {
		super(t);
	}

	public DomainException(String message, Throwable t) {
		super(message, t);
	}

	public DomainException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
		super(message, cause, enableSuppression, writableStackTrace);
	}

	public DomainException(String message, String... i18nValues) {
		super(message, i18nValues);
	}

	public DomainException(String message, Throwable cause, String... i18nValues) {
		super(message, cause, i18nValues);
	}

	public DomainException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace, String... i18nValues) {
		super(message, cause, enableSuppression, writableStackTrace, i18nValues);
	}

	public DomainException(String message, boolean i18n) {
		super(message, i18n);
	}

	public DomainException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace, boolean i18n) {
		super(message, cause, enableSuppression, writableStackTrace, i18n);
	}

	public DomainException(String message, Throwable cause, boolean i18n) {
		super(message, cause, i18n);
	}
}
