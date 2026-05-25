package org.skyve.domain.messages;

import java.util.ArrayList;
import java.util.List;

/**
 * Accumulates one or more field-level or cross-field validation errors for display in
 * the Skyve UI.
 *
 * <p>Thrown from {@link org.skyve.metadata.model.document.Bizlet#validate} and from
 * framework validation infrastructure to signal that a save or action should be
 * prevented. Unlike most exceptions, {@code ValidationException} is not intended to
 * halt processing immediately; the framework catches it, adds its messages to the view,
 * and presents all errors to the user in a single pass.
 *
 * <p>Typical usage in a Bizlet:
 * <pre>{@code
 * @Override
 * public void validate(Bean bean, ValidationException e) {
 *     MyDocument doc = (MyDocument) bean;
 *     if (doc.getStartDate().after(doc.getEndDate())) {
 *         e.getMessages().add(new Message("startDate", "Start date must be before end date"));
 *     }
 * }
 * }</pre>
 *
 * <p>Each {@link Message} in the exception can carry one or more bean binding paths.
 * The view highlights the corresponding input fields in red and shows the message text.
 *
 * @see Message
 * @see org.skyve.metadata.model.document.Bizlet#validate
 */
public class ValidationException extends DomainException implements MessageException {
	private static final long serialVersionUID = -2033700648810378214L;

	/**
	 * Executes ArrayList<>.
	 * @return the result
	 */
	protected List<Message> messages = new ArrayList<>();

	/**
	 * Creates a new ValidationException instance.
	 */
	public ValidationException() {
	}
	
	/**
	 * Constructor
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

	/**
	 * Returns the messages.
	 * @return the result
	 */
	@Override
	public List<Message> getMessages() {
		return messages;
	}

	/**
	 * Returns the message.
	 * @return the result
	 */
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
