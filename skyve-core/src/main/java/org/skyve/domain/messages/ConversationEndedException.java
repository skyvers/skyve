package org.skyve.domain.messages;

import java.util.Collections;
import java.util.List;
import java.util.Locale;

import org.skyve.util.Util;

import jakarta.annotation.Nonnull;

/**
 * Thrown when an action is attempted on a Skyve conversation that has already ended.
 *
 * <p>Skyve conversations have a finite lifetime tied to the web session and user
 * navigation. If a user revisits an old page after the conversation has expired (e.g.
 * after a server restart, session timeout, or navigating away and back), this exception
 * is raised. The framework typically responds by redirecting the user to a re-entry
 * point.
 *
 * <p>The message is localised using the request's {@link java.util.Locale}.
 */
@SuppressWarnings("java:S110") // This inheritance-depth warning is ridiculous for intentional framework hierarchies.
public class ConversationEndedException extends DomainException implements MessageException {
	private static final long serialVersionUID = 7198466174424309573L;

	private static final String MESSAGE_KEY = "exception.conversationEnded";
	
	private List<Message> messages = null;

	/**
	 * Creates a new ConversationEndedException instance.
	 * @param httpRequestLocale the httpRequestLocale
	 */
	public ConversationEndedException(@Nonnull Locale httpRequestLocale) {
		super(Util.nullSafeI18n(MESSAGE_KEY, httpRequestLocale), false);
		messages = Collections.singletonList(new Message(getMessage()));
	}

	/**
	 * Returns the messages.
	 * @return the result
	 */
	@Override
	public List<Message> getMessages() {
		return messages;
	}
}
