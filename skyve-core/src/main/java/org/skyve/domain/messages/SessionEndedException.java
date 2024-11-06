package org.skyve.domain.messages;

import java.util.Collections;
import java.util.List;
import java.util.Locale;

import org.skyve.util.Util;

import jakarta.annotation.Nonnull;
import jakarta.validation.constraints.NotNull;

/**
 * 
 */
public class SessionEndedException extends DomainException implements MessageException {
	private static final long serialVersionUID = 2247724782906480914L;

	private static final String MESSAGE_KEY = "exception.sessionEnded";

	private List<Message> messages = null;

	public SessionEndedException(@NotNull Locale httpRequestLocale) {
		super(Util.nullSafeI18n(MESSAGE_KEY, httpRequestLocale), false);
		messages = Collections.singletonList(new Message(getMessage()));
	}

	@Override
	public @Nonnull List<Message> getMessages() {
		return messages;
	}
}
