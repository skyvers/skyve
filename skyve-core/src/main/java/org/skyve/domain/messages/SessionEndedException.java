package org.skyve.domain.messages;

import java.util.Collections;
import java.util.List;
import java.util.Locale;

import org.skyve.util.Util;

/**
 * 
 */
public class SessionEndedException extends DomainException implements MessageException {
	private static final long serialVersionUID = 2247724782906480914L;

	private static final String MESSAGE_KEY = "exception.sessionEnded";

	private List<Message> messages = null;

	public SessionEndedException(Locale httpRequestLocale) {
		super(Util.i18n(MESSAGE_KEY, httpRequestLocale), false);
		messages = Collections.singletonList(new Message(getMessage()));
	}

	@Override
	public List<Message> getMessages() {
		return messages;
	}
}
