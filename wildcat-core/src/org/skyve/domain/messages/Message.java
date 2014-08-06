package org.skyve.domain.messages;

import java.util.ArrayList;
import java.util.List;

/**
 * 
 */
public class Message {
	private List<String> bindings = new ArrayList<>();
	private String errorMessage;

	/**
	 * 
	 * @param errorMessage
	 */
	public Message(String errorMessage) {
		this.errorMessage = errorMessage;
	}

	/**
	 * Convenience constructor for 1 binding.
	 * 
	 * @param binding
	 * @param errorMessage
	 */
	public Message(String binding, String errorMessage) {
		this.errorMessage = errorMessage;
		bindings.add(binding);
	}

	/**
	 * 
	 * @param bindings
	 * @param errorMessage
	 */
	public Message(String[] bindings, String errorMessage) {
		this.errorMessage = errorMessage;

		for (String binding : bindings) {
			this.bindings.add(binding);
		}
	}

	/**
	 * 
	 */
	public void setBindingPrefix(String bindingPrefixWithDot) {
		for (int i = 0, l = bindings.size(); i < l; i++) {
			String binding = bindings.remove(i);
			bindings.add(i, bindingPrefixWithDot + binding);
		}
	}

	/**
	 * 
	 */
	public void addBinding(String binding) {
		bindings.add(binding);
	}

	/**
	 * 
	 */
	public Iterable<String> getBindings() {
		return bindings;
	}

	/**
	 * 
	 */
	public String getErrorMessage() {
		return errorMessage;
	}

	/**
	 * 
	 */
	@Override
	public String toString() {
		StringBuilder result = new StringBuilder(64);
		result.append("Bindings = ");
		for (String binding : bindings) {
			result.append(binding).append(" : ");
		}
		result.append('\n');
		result.append("Error message = ").append(errorMessage).append('\n');

		return result.toString();
	}
}
