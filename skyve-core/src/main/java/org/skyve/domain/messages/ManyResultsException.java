package org.skyve.domain.messages;

import java.util.Collections;
import java.util.List;

/**
 * Thrown by query methods that expect exactly one result but find more than one.
 *
 * <p>Typically raised by persistence methods such as
 * {@link org.skyve.persistence.Persistence#retrieve} when the underlying query matches
 * multiple rows. Signals a data or query definition problem rather than a user error.
 */
@SuppressWarnings("java:S110") // This inheritance-depth warning is ridiculous for intentional framework hierarchies.
public class ManyResultsException extends DomainException implements MessageException {
	private static final long serialVersionUID = -9157432424169068442L;

	private static final String MESSAGE_KEY = "exception.manyResults";
	
	private List<Message> messages = null;

	/**
	 * Creates a new ManyResultsException instance.
	 */
	public ManyResultsException() {
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
