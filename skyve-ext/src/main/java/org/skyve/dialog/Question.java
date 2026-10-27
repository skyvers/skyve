package org.skyve.dialog;

import java.io.Serializable;
import java.util.List;

import jakarta.annotation.Nonnull;

/**
 * A question for the user, asked through {@link Dialogs#ask(Question)}.
 *
 * <p>A question has a key, a title, a message and a preset pair of buttons: OK and Cancel, or
 * Yes and No. The first button is positive and the second is negative. Unless
 * {@link #defaultButton(Button)} says otherwise, the negative button is the default.
 * {@link #cancelAborts()} makes the negative button abort the user's gesture instead of answering.
 *
 * <p>The key is supplied by the caller and must be the same on every run of the request, for example
 * {@code "<script name>#<statement number>"}. The title and message are plain text; renderers always
 * escape them, so they cannot carry markup.
 *
 * <p>Threading: <em>not thread-safe</em>. {@link #defaultButton(Button)} and {@link #cancelAborts()}
 * change this instance; build and ask a question on one thread.
 *
 * @since 10.0
 */
public final class Question implements Serializable {
	/** The serialisation version. */
	private static final long serialVersionUID = -3364813216434339591L;

	/**
	 * A button that can answer a question. Its {@link #name()} is the code that
	 * {@link Dialogs#ask(Question)} returns and the client sends back.
	 *
	 * @since 10.0
	 */
	public enum Button {
		/** The positive button of an OK/Cancel question. */
		OK,
		/** The negative button of an OK/Cancel question. */
		CANCEL,
		/** The positive button of a Yes/No question. */
		YES,
		/** The negative button of a Yes/No question. */
		NO
	}

	/** The caller's key, stable across reruns. */
	private final @Nonnull String key;
	/** The plain text title. */
	private final @Nonnull String title;
	/** The plain text message. */
	private final @Nonnull String message;
	/** The first button: OK or Yes. */
	private final @Nonnull Button positive;
	/** The second button: Cancel or No. */
	private final @Nonnull Button negative;
	/** The button with the focus when the question is shown; the negative button unless set. */
	private @Nonnull Button defaultButton;
	/** Whether the negative button aborts the gesture instead of answering. */
	private boolean negativeAborts = false;

	/**
	 * Creates a question whose negative button is the default and answers rather than aborts.
	 *
	 * @param key	The caller's key for the question; stable across reruns.
	 * @param title	The plain text title.
	 * @param message	The plain text message.
	 * @param positive	The first button.
	 * @param negative	The second button.
	 */
	private Question(@Nonnull String key,
						@Nonnull String title,
						@Nonnull String message,
						@Nonnull Button positive,
						@Nonnull Button negative) {
		this.key = key;
		this.title = title;
		this.message = message;
		this.positive = positive;
		this.negative = negative;
		defaultButton = negative;
	}

	/**
	 * Creates a question with OK and Cancel buttons.
	 *
	 * @param key	The caller's key for the question; must be the same on every run of the request.
	 * @param title	The plain text title.
	 * @param message	The plain text message.
	 * @return	A new question whose default button is Cancel and whose Cancel answers {@code CANCEL}.
	 */
	public static @Nonnull Question okCancel(@Nonnull String key, @Nonnull String title, @Nonnull String message) {
		return new Question(key, title, message, Button.OK, Button.CANCEL);
	}

	/**
	 * Creates a question with Yes and No buttons.
	 *
	 * @param key	The caller's key for the question; must be the same on every run of the request.
	 * @param title	The plain text title.
	 * @param message	The plain text message.
	 * @return	A new question whose default button is No and whose No answers {@code NO}.
	 */
	public static @Nonnull Question yesNo(@Nonnull String key, @Nonnull String title, @Nonnull String message) {
		return new Question(key, title, message, Button.YES, Button.NO);
	}

	/**
	 * Sets the button that has the focus when the question is shown.
	 *
	 * <p>Side effects: changes this question.
	 *
	 * @param button	One of this question's buttons.
	 * @return	This question.
	 * @throws IllegalArgumentException	if {@code button} is not one of this question's buttons.
	 */
	public @Nonnull Question defaultButton(@Nonnull Button button) {
		if ((button != positive) && (button != negative)) {
			throw new IllegalArgumentException("Button " + button + " is not a button of question " + key);
		}
		defaultButton = button;
		return this;
	}

	/**
	 * Makes the negative button (Cancel or No) abort the user's gesture.
	 *
	 * <p>Choosing an aborting button sends nothing to the server, so {@link Dialogs#ask(Question)}
	 * never returns its code. Without this, the negative button is an answer like any other.
	 *
	 * <p>Side effects: changes this question.
	 *
	 * @return	This question.
	 */
	public @Nonnull Question cancelAborts() {
		negativeAborts = true;
		return this;
	}

	/**
	 * Returns the caller's key for this question.
	 *
	 * @return	The key; never {@code null}.
	 */
	public @Nonnull String getKey() {
		return key;
	}

	/**
	 * Returns the title, as plain text.
	 *
	 * @return	The title; never {@code null}.
	 */
	public @Nonnull String getTitle() {
		return title;
	}

	/**
	 * Returns the message, as plain text.
	 *
	 * @return	The message; never {@code null}.
	 */
	public @Nonnull String getMessage() {
		return message;
	}

	/**
	 * Returns the buttons in the order they are shown: the positive button, then the negative button.
	 *
	 * @return	A new unmodifiable list of two buttons.
	 */
	public @Nonnull List<Button> getButtons() {
		return List.of(positive, negative);
	}

	/**
	 * Returns the button that has the focus when the question is shown.
	 *
	 * @return	The button set by {@link #defaultButton(Button)}, otherwise the negative button.
	 */
	public @Nonnull Button getDefaultButton() {
		return defaultButton;
	}

	/**
	 * Indicates whether choosing a button aborts the user's gesture instead of answering this question.
	 *
	 * @param button	The button to test.
	 * @return	{@code true} only for the negative button of a question marked with {@link #cancelAborts()}.
	 */
	public boolean aborts(@Nonnull Button button) {
		return negativeAborts && (button == negative);
	}
}
