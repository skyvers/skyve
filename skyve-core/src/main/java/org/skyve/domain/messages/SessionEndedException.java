package org.skyve.domain.messages;

import java.util.Collections;
import java.util.List;
import java.util.Locale;

import org.skyve.util.Util;

import jakarta.annotation.Nonnull;
import jakarta.validation.constraints.NotNull;

/**
 * Thrown when an action is attempted after the user's HTTP session has expired.
 *
 * <p>The framework raises this when it detects that the Skyve session state (user,
 * persistence context, conversation) is no longer available on the current thread.
 * The message is localised using the request's {@link java.util.Locale}.
 *
 * @see ConversationEndedException
 */
@SuppressWarnings("java:S110") // This inheritance-depth warning is ridiculous for intentional framework hierarchies.
public class SessionEndedException extends DomainException implements MessageException {
	private static final long serialVersionUID = 2247724782906480914L;

	private static final String MESSAGE_KEY = "exception.sessionEnded";

	private List<Message> messages = null;

	/**
	 * Creates a new SessionEndedException instance.
	 * @param httpRequestLocale the httpRequestLocale
	 */
	public SessionEndedException(@NotNull Locale httpRequestLocale) {
		super(Util.nullSafeI18n(MESSAGE_KEY, httpRequestLocale), false);
		messages = Collections.singletonList(new Message(getMessage()));
	}

	/**
	 * Returns the messages.
	 * @return the result
	 */
	@Override
	public @Nonnull List<Message> getMessages() {
		return messages;
	}
}
