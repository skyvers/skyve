package org.skyve.domain.messages;

/**
 * 
 */
public class DomainException extends SkyveException {
	private static final long serialVersionUID = -2523236450510857431L;

	/**
	 * 
	 */
	protected DomainException() {
		// used in subclasses
	}

	/**
	 * 
	 * @param message
	 */
	public DomainException(String message) {
		super(message);
	}

	/**
	 * 
	 * @param t
	 */
	public DomainException(Throwable t) {
		super(t);
	}

	/**
	 * 
	 * @param message
	 * @param t
	 */
	public DomainException(String message, Throwable t) {
		super(message, t);
	}
}
