package org.skyve.domain.messages;

import java.util.ArrayList;
import java.util.List;

/**
 * 
 */
public class ValidationException extends DomainException implements MessageException {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -2033700648810378214L;

	protected List<Message> messages = new ArrayList<>();

	public ValidationException() {
	}
	
	/**
	 * 
	 * @param message
	 */
	public ValidationException(Message message) {
		messages.add(message);
	}

	@Override
	public List<Message> getMessages() {
		return messages;
	}

	@Override
	public String toString() {
		StringBuilder result = new StringBuilder(super.toString());
		for (Message message : messages) {
			result.append('\n').append(message.toString());
		}
		
		return result.toString();
	}
}
