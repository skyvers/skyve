package org.skyve.dialog;

import org.skyve.domain.messages.SkyveException;

import jakarta.annotation.Nonnull;

/**
 * Thrown by {@link Dialogs#ask(Question)} when the current request carries no answer to a question.
 *
 * <p>This is not an error. Each renderer looks for it in the cause chain of whatever it catches,
 * rolls the request back and sends the question to the client, which sends the request again with
 * the answer. It does not implement {@link org.skyve.domain.messages.MessageException}, so the
 * existing error paths never show it as an error message.
 *
 * <p>Code that catches exceptions around a call that can ask must let this exception through,
 * or wrap it with this exception as a cause.
 *
 * @since 10.0
 */
public class QuestionException extends SkyveException {
	/** The serialization version. */
	private static final long serialVersionUID = 6573327946420283096L;

	/** The question that has no answer. */
	private final @Nonnull Question question;
	/** The question's id, which the client sends back with its answer. */
	private final @Nonnull String id;

	/**
	 * Creates the exception for a question that has no answer.
	 *
	 * @param question	The question to ask.
	 * @param id	The question's id, which the client sends back with its answer.
	 */
	public QuestionException(@Nonnull Question question, @Nonnull String id) {
		super("Question " + question.getKey() + " has no answer yet");
		this.question = question;
		this.id = id;
	}

	/**
	 * Returns the question that was asked, with its key, resolved text and buttons.
	 *
	 * @return	The question; never {@code null}.
	 */
	public @Nonnull Question getQuestion() {
		return question;
	}

	/**
	 * Returns the question's id: a fingerprint of its key, text and buttons that the client sends back with its answer.
	 *
	 * @return	The id; never {@code null}.
	 */
	public @Nonnull String getId() {
		return id;
	}
}
