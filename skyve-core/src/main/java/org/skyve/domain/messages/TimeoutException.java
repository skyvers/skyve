package org.skyve.domain.messages;

import java.util.ArrayList;
import java.util.List;

public class TimeoutException extends DomainException implements MessageException {
	private static final long serialVersionUID = 1184348868443172562L;

	private static final String MESSAGE = "This operation has timed out.";
	private static final List<Message> MESSAGES = new ArrayList<>(1);
	static {
		MESSAGES.add(new Message(MESSAGE));
	}

	public TimeoutException() {
		super(MESSAGE);
	}

	public TimeoutException(Throwable t) {
		super(MESSAGE, t);
	}

	@Override
	public List<Message> getMessages() {
		return MESSAGES;
	}
}
