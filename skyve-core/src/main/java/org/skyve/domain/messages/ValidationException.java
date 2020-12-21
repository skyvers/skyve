package org.skyve.domain.messages;

import java.util.ArrayList;
import java.util.List;

/**
 * 
 */
public class ValidationException extends DomainException implements MessageException {
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

	/**
	 * This is a convenience constructor.
	 *
	 * @param message A message describing the validation exception.
	 */
	public ValidationException(String message) {
		this(new Message(message));
	}

	/**
	 * This is a convenience constructor.
	 *
	 * @param binding The binding for the validation exception.
	 * @param message A message describing the validation exception.
	 */
	public ValidationException(String binding, String message) {
		this(new Message(binding, message));
	}

	/**
	 * @param messages A list of messages to add to the ValidationException.
	 */
	public ValidationException(List<Message> messages) {
		this.messages.addAll(messages);
	}

	@Override
	public List<Message> getMessages() {
		return messages;
	}

	@Override
	public String getMessage() {
		String superMessage = super.getMessage();
		StringBuilder result = new StringBuilder((superMessage == null) ? "" : superMessage);
		for (Message message : messages) {
			result.append('\n').append(message.toString());
		}
		
		return result.toString();
	}
}
