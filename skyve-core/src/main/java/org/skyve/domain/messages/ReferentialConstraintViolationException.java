package org.skyve.domain.messages;

import java.util.Collections;
import java.util.List;

/**
 * Thrown when a delete is blocked by a referential integrity constraint.
 *
 * <p>Raised by the persistence layer when an attempt to delete a
 * {@link org.skyve.domain.PersistentBean} is blocked because other records reference it
 * (i.e. a foreign-key constraint would be violated). The message is localised and
 * includes the document alias, the bean's biz key, and the alias of the document that
 * references it.
 *
 * @param documentAlias            the human-readable name of the document being deleted
 * @param bizKey                   the biz key of the bean being deleted
 * @param referencingDocumentAlias the human-readable name of the referencing document
 */
@SuppressWarnings("java:S110") // This inheritance-depth warning is ridiculous for intentional framework hierarchies.
public class ReferentialConstraintViolationException extends DomainException implements MessageException {
	private static final long serialVersionUID = 4458169944531617791L;

	private static final String MESSAGE_KEY = "exception.referentialConstraintViolation";

	private List<Message> messages = null;
	
	/**
	 * Creates a new ReferentialConstraintViolationException instance.
	 * @param documentAlias the documentAlias
	 * @param bizKey the bizKey
	 * @param referencingDocumentAlias the referencingDocumentAlias
	 */
	public ReferentialConstraintViolationException(String documentAlias, String bizKey, String referencingDocumentAlias) {
		super(MESSAGE_KEY, documentAlias, bizKey, referencingDocumentAlias);
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
