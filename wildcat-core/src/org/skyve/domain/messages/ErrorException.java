package org.skyve.domain.messages;

/**
 * 
 */
public interface ErrorException extends ErrorMessage {
	/**
	 * 
	 * @return
	 */
	public ValidationMessage getDelegate();
}
