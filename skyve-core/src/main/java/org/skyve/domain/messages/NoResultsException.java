package org.skyve.domain.messages;

import java.util.Collections;
import java.util.List;

public class NoResultsException extends DomainException implements MessageException {
	private static final long serialVersionUID = -9157432424169068442L;

	private static final String MESSAGE_KEY = "exception.noResults";
	
	private List<Message> messages = null;

	public NoResultsException() {
		super(MESSAGE_KEY);
		messages = Collections.singletonList(new Message(getMessage()));
	}

	@Override
	public List<Message> getMessages() {
		return messages;
	}
}
