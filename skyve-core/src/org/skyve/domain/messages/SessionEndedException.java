package org.skyve.domain.messages;

import java.util.ArrayList;
import java.util.List;

/**
 * 
 */
public class SessionEndedException extends DomainException implements MessageException {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 2247724782906480914L;

	private static final String MESSAGE = "Your session has ended - press F5 key or the Refresh/Reload browser button.";
	private static final List<Message> MESSAGES = new ArrayList<>(1);
	static {
		MESSAGES.add(new Message(MESSAGE));
	}

	/**
	 * 
	 * @param constraintName
	 * @param message
	 */
	public SessionEndedException() {
		super(MESSAGE);
	}

	@Override
	public List<Message> getMessages() {
		return MESSAGES;
	}
}
