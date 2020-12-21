package org.skyve.domain.messages;

import java.util.Collections;
import java.util.List;
import java.util.Locale;

import org.skyve.util.Util;

/**
 * 
 */
public class ConversationEndedException extends DomainException implements MessageException {
	private static final long serialVersionUID = 7198466174424309573L;

	private static final String MESSAGE_KEY = "exception.conversationEnded";
	
	private List<Message> messages = null;

	public ConversationEndedException(Locale httpRequestLocale) {
		super(Util.i18n(MESSAGE_KEY, httpRequestLocale), false);
		messages = Collections.singletonList(new Message(getMessage()));
	}

	@Override
	public List<Message> getMessages() {
		return messages;
	}
}
