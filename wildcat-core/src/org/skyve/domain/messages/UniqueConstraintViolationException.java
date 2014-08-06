package org.skyve.domain.messages;

import java.util.ArrayList;
import java.util.List;

/**
 * 
 */
public class UniqueConstraintViolationException extends DomainException implements MessageException {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 2245888585799230814L;

	private String constraintName;
	private List<Message> messages = new ArrayList<>(1);
	
	/**
	 * 
	 * @param constraintName
	 * @param message
	 */
	public UniqueConstraintViolationException(String constraintName, String message) {
		super(message);
		messages.add(new Message(message));
		this.constraintName = constraintName;
	}

	/**
	 * 
	 * @param constraintName
	 * @param binding
	 * @param message
	 */
	public UniqueConstraintViolationException(String constraintName, String binding, String message) {
		super(message);
		messages.add(new Message(binding, message));
		this.constraintName = constraintName;
	}

	/**
	 * 
	 * @return
	 */
	public String getConstraintName() {
		return constraintName;
	}

	@Override
	public List<Message> getMessages() {
		return messages;
	}
}
