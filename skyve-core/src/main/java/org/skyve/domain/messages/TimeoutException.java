package org.skyve.domain.messages;

import java.util.Collections;
import java.util.List;

/**
 * Thrown when a Skyve request or operation exceeds its allowed time budget.
 *
 * <p>The framework raises this to signal that a long-running operation (e.g. a report,
 * a large import, or a complex query) has exceeded a configured timeout threshold.
 * Application code may catch this and present a user-friendly message.
 */
@SuppressWarnings("java:S110") // This inheritance-depth warning is ridiculous for intentional framework hierarchies.
public class TimeoutException extends DomainException implements MessageException {
	private static final long serialVersionUID = 1184348868443172562L;

	private static final String MESSAGE_KEY = "exception.timeout";
	
	private List<Message> messages = null;
	
	/**
	 * Creates a new TimeoutException instance.
	 */
	public TimeoutException() {
		super(MESSAGE_KEY);
		messages = Collections.singletonList(new Message(getMessage()));
	}

	/**
	 * Creates a new TimeoutException instance.
	 * @param t the t
	 */
	public TimeoutException(Throwable t) {
		super(MESSAGE_KEY, t);
		messages = Collections.singletonList(new Message(getMessage()));
	}

	/**
	 * Returns the messages.
	 * @return the result
	 */
	@Override
	public List<Message> getMessages() {
		return messages;
	}
}
