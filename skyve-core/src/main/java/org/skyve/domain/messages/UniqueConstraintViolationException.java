package org.skyve.domain.messages;

import java.util.ArrayList;
import java.util.List;

import org.skyve.metadata.model.document.Document;

/**
 * 
 */
public class UniqueConstraintViolationException extends DomainException implements MessageException {
	private static final long serialVersionUID = 2245888585799230814L;

	private Document document;
	private String constraintName;
	private List<Message> messages = new ArrayList<>(1);
	
	/**
	 * 
	 * @param document
	 * @param constraintName
	 * @param message
	 */
	public UniqueConstraintViolationException(Document document, String constraintName, String message) {
		super(message);
		messages.add(new Message(message));
		this.document = document;
		this.constraintName = constraintName;
	}

	/**
	 * 
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
				" is violated with message :- " + message);
		messages.add(new Message(binding, message));
		this.document = document;
		this.constraintName = constraintName;
	}

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

	@Override
	public List<Message> getMessages() {
		return messages;
	}
}
