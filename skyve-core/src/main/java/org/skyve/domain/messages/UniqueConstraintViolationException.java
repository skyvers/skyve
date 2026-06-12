package org.skyve.domain.messages;

import java.util.ArrayList;
import java.util.List;

import org.skyve.metadata.model.document.Document;

/**
 * Thrown when a unique constraint declared in document metadata is violated.
 *
 * <p>The persistence layer raises this exception when a save would result in a duplicate
 * key for a constraint declared via {@code <uniqueConstraint>} in document metadata.
 * The exception carries the {@link org.skyve.metadata.model.document.Document},
 * the constraint name, and an optional binding path so the UI can highlight the
 * offending field.
 *
 * @see org.skyve.metadata.model.document.UniqueConstraint
 */
@SuppressWarnings("java:S110") // This inheritance-depth warning is ridiculous for intentional framework hierarchies.
public class UniqueConstraintViolationException extends DomainException implements MessageException {
	private static final long serialVersionUID = 2245888585799230814L;

	private Document document;
	private String constraintName;
	private List<Message> messages = new ArrayList<>(1);
	
	/**
	 * Constructor
	 * @param document
	 * @param constraintName
	 * @param message
	 */
	public UniqueConstraintViolationException(Document document, String constraintName, String message) {
		super(message, false);
		messages.add(new Message(message));
		this.document = document;
		this.constraintName = constraintName;
	}

	/**
	 * Binding constructor
	 * @param document
	 * @param constraintName
	 * @param binding
	 * @param message
	 */
	public UniqueConstraintViolationException(Document document,
												String constraintName,
												String binding,
												String message) {
		super("Constraint " + constraintName + " in document " + document.getOwningModuleName() + "." + document.getName() +
				" is violated with message :- " + message, false);
		messages.add(new Message(binding, message));
		this.document = document;
		this.constraintName = constraintName;
	}

	/**
	 * Returns the document.
	 * @return the result
	 */
	public Document getDocument() {
		return document;
	}
	
	/**
	 * 
	 * @return
	 */
	public String getConstraintName() {
		return constraintName;
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
