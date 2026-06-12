package org.skyve.domain.messages;

import java.util.Collections;
import java.util.List;

/**
 * Thrown by query methods that require exactly one result but find none.
 *
 * <p>Typically raised by persistence methods such as
 * {@link org.skyve.persistence.Persistence#retrieve} when the underlying query returns
 * zero rows. Distinguishes a "not found" condition from other {@link DomainException}
 * causes.
 *
 * @see ManyResultsException
 */
@SuppressWarnings("java:S110") // This inheritance-depth warning is ridiculous for intentional framework hierarchies.
public class NoResultsException extends DomainException implements MessageException {
	private static final long serialVersionUID = -9157432424169068442L;

	private static final String MESSAGE_KEY = "exception.noResults";
	
	private List<Message> messages = null;

	/**
	 * Creates a new NoResultsException instance.
	 */
	public NoResultsException() {
		super(MESSAGE_KEY);
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
