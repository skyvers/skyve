package org.skyve.domain.messages;

import java.util.ArrayList;
import java.util.List;

public class ManyResultsException extends DomainException implements MessageException {
	private static final long serialVersionUID = -9157432424169068442L;

	private static final String MESSAGE = "Many results were retrieved.  Please contact your system administrator";
	private static final List<Message> MESSAGES = new ArrayList<>(1);
	static {
		MESSAGES.add(new Message(MESSAGE));
	}

	public ManyResultsException() {
		super(MESSAGE);
	}

	@Override
	public List<Message> getMessages() {
		return MESSAGES;
	}
}
