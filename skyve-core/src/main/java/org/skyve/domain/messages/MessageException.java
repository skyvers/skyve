package org.skyve.domain.messages;

import java.util.List;

import jakarta.annotation.Nonnull;

/**
 * Implemented by exceptions that carry a list of user-facing {@link Message} instances.
 *
 * <p>The framework uses this interface to distinguish exceptions that have structured,
 * displayable content (field-level error messages) from plain exceptions that only have
 * a stack trace. Implementations such as {@link ValidationException} and
 * {@link ConversionException} populate the message list with one or more
 * {@link Message} objects that the UI renders as inline field errors and/or
 * notification banners.
 *
 * @see Message
 * @see ValidationException
 */
public interface MessageException {
	/**
	 * Returns the messages.
	 * @return the result
	 */
	public @Nonnull List<Message> getMessages();
}
