package org.skyve.domain.messages;

import java.util.Collections;
import java.util.List;

public class TimeoutException extends DomainException implements MessageException {
	private static final long serialVersionUID = 1184348868443172562L;

	private static final String MESSAGE_KEY = "exception.timeout";
	
	private List<Message> messages = null;
	
	public TimeoutException() {
		super(MESSAGE_KEY);
		messages = Collections.singletonList(new Message(getMessage()));
	}

	public TimeoutException(Throwable t) {
		super(MESSAGE_KEY, t);
		messages = Collections.singletonList(new Message(getMessage()));
	}

	@Override
	public List<Message> getMessages() {
		return messages;
	}
}
