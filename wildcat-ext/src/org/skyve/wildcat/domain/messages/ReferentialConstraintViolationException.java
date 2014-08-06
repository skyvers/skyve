package org.skyve.wildcat.domain.messages;

import java.util.ArrayList;
import java.util.List;

import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.MessageException;
import org.skyve.domain.messages.Message;

public class ReferentialConstraintViolationException extends DomainException implements MessageException {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 4458169944531617791L;

	private List<Message> messages = new ArrayList<>();
	
	public ReferentialConstraintViolationException(String message) {
		super(message);
		messages.add(new Message(message));
	}

	@Override
	public List<Message> getMessages() {
		return messages;
	}
}
