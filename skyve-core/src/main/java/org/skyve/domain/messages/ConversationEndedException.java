package org.skyve.domain.messages;

import java.util.ArrayList;
import java.util.List;

/**
 * 
 */
public class ConversationEndedException extends DomainException implements MessageException {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 7198466174424309573L;

	private static final String MESSAGE = "Your conversation has ended - press F5 key or the Refresh/Reload browser button.";
	private static final List<Message> MESSAGES = new ArrayList<>(1);
	static {
		MESSAGES.add(new Message(MESSAGE));
	}

	/**
	 * 
	 * @param constraintName
	 * @param message
	 */
	public ConversationEndedException() {
		super(MESSAGE);
	}

	@Override
	public List<Message> getMessages() {
		return MESSAGES;
	}
}
