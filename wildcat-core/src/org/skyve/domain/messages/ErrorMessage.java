package org.skyve.domain.messages;

import java.util.List;

/**
 * 
 */
public interface ErrorMessage {
	/**
	 * 
	 * @param binding
	 */
	public void addBinding(String binding);

	/**
	 * 
	 * @return
	 */
	public String getErrorMessage();

	/**
	 * 
	 * @return
	 */
	public Iterable<String> getBindings();

	/**
	 * 
	 * @param bindingPrefixWithDot
	 */
	public void setBindingPrefix(String bindingPrefixWithDot);

	/**
	 * 
	 * @return
	 */
	public List<ErrorMessage> getSubordinates();
}
