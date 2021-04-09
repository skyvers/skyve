package org.skyve.domain.messages;

import java.util.ArrayList;
import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.util.Binder;
import org.skyve.util.Util;

/**
 * 
 */
public class Message {
	private List<String> bindings = new ArrayList<>();
	private String text;

	/**
	 * Message constructor.
	 * @param text	The message text
	 */
	public Message(String text) {
		this.text = Util.i18n(text);
	}
	
	/**
	 * Formatted message constructor.
	 */
	public Message(String text, Bean... beans) {
		this.text = Binder.formatMessage(Util.i18n(text), beans);
	}

	/**
	 * Convenience constructor for 1 binding.
	 * 
	 * @param binding
	 * @param text
	 */
	public Message(String binding, String text) {
		this.text = Util.i18n(text);
		bindings.add(binding);
	}

	/**
	 * Formatted message convenience constructor for 1 binding.
	 */
	public Message(String binding, String text, Bean... beans) {
		this.text = Binder.formatMessage(Util.i18n(text), beans);
		bindings.add(binding);
	}

	/**
	 * Multiple binding constructor
	 * @param bindings
	 * @param text
	 */
	public Message(String[] bindings, String text) {
		this.text = Util.i18n(text);

		for (String binding : bindings) {
			this.bindings.add(binding);
		}
	}

	/**
	 * Multiple binding formatted message constructor.
	 */
	public Message(String[] bindings, String text, Bean... beans) {
		this.text = Binder.formatMessage(Util.i18n(text), beans);

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
	public String getText() {
		return text;
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
		result.append("Message = ").append(text).append('\n');

		return result.toString();
	}
}
