package org.skyve.domain.messages;

public abstract class SkyveException extends RuntimeException {
	private static final long serialVersionUID = 3326193539360595441L;

	public SkyveException() {
	}

	public SkyveException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
		super(message, cause, enableSuppression, writableStackTrace);
	}

	public SkyveException(String message, Throwable cause) {
		super(message, cause);
	}

	public SkyveException(String message) {
		super(message);
	}

	public SkyveException(Throwable cause) {
		super(cause);
	}
}
