package org.skyve.domain.messages;

import java.util.ArrayList;
import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.util.Binder;
import org.skyve.util.Util;

import jakarta.annotation.Nonnull;

/**
 * Used to convey messages to views.
 */
public class Message {
	private List<String> bindings = new ArrayList<>();
	private String text;

	/**
	 * Message constructor.
	 * @param text	The message text
	 */
	public Message(@Nonnull String text) {
		this.text = Util.nullSafeI18n(text);
	}
	
	/**
	 * Formatted message constructor.
	 */
	public Message(@Nonnull String text, Bean... beans) {
		this.text = Binder.formatMessage(Util.nullSafeI18n(text), beans);
	}

	/**
	 * Convenience constructor for 1 binding.
	 * 
	 * @param binding
	 * @param text
	 */
	public Message(@Nonnull String binding, @Nonnull String text) {
		this.text = Util.nullSafeI18n(text);
		bindings.add(binding);
	}

	/**
	 * Formatted message convenience constructor for 1 binding.
	 */
	public Message(@Nonnull String binding, @Nonnull String text, Bean... beans) {
		this.text = Binder.formatMessage(Util.nullSafeI18n(text), beans);
		bindings.add(binding);
	}

	/**
	 * Multiple binding constructor
	 * @param bindings
	 * @param text
	 */
	public Message(@Nonnull String[] bindings, @Nonnull String text) {
		this.text = Util.nullSafeI18n(text);

		for (String binding : bindings) {
			this.bindings.add(binding);
		}
	}

	/**
	 * Multiple binding formatted message constructor.
	 */
	public Message(@Nonnull String[] bindings, @Nonnull String text, Bean... beans) {
		this.text = Binder.formatMessage(Util.nullSafeI18n(text), beans);

		for (String binding : bindings) {
			this.bindings.add(binding);
		}
	}

	/**
	 * Add a binding prefix with the dot onto each binding in the message.
	 */
	public void setBindingPrefix(@Nonnull String bindingPrefixWithDot) {
		for (int i = 0, l = bindings.size(); i < l; i++) {
			String binding = bindings.remove(i);
			bindings.add(i, bindingPrefixWithDot + binding);
		}
	}

	/**
	 * Add a binding onto the message.
	 */
	public void addBinding(@Nonnull String binding) {
		bindings.add(binding);
	}

	/**
	 * Get the bindings in the message.
	 */
	public @Nonnull Iterable<String> getBindings() {
		return bindings;
	}

	/**
	 * Get the message text.
	 */
	public @Nonnull String getText() {
		return text;
	}

	/**
	 * A loggable String representation.
	 */
	@Override
	public @Nonnull String toString() {
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
