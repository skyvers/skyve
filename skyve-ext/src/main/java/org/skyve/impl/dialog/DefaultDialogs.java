package org.skyve.impl.dialog;

import org.skyve.dialog.Dialogs;
import org.skyve.dialog.Question;
import org.skyve.dialog.Question.Button;
import org.skyve.dialog.QuestionException;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.WebContainer;
import org.skyve.util.JSON;
import org.skyve.util.OWASP;
import org.skyve.util.logging.SkyveLoggerFactory;
import org.slf4j.Logger;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.servlet.http.HttpServletRequest;

/**
 * Asks questions of the user of the current HTTP request, keeping the answers on the request.
 *
 * <p>A request can ask only when a renderer has marked it as askable, which each renderer does on the paths
 * that can rerun a request. The client sends the answers given so far in the
 * {@value DialogsProtocol#ANSWERS_PARAMETER_NAME} request parameter (see {@link DialogsProtocol}). A question's id
 * is a fingerprint of its key, title, message and buttons, so an answer is only used for the question it was
 * given to.
 *
 * <p>Threading: <em>thread-safe</em>. Instances hold no state; answers are read from, and cached on,
 * the request bound to the current thread by {@link WebContainer}.
 */
public class DefaultDialogs implements Dialogs {
	/**
	 * The request attribute caching the parsed answers, so that the parameter is parsed once per request.
	 */
	private static final String ANSWERS_ATTRIBUTE_NAME = "skyve.dialogs.answers";

	/**
	 * The most answers one gesture can collect. A question whose text changes on every run never matches
	 * its answers, and without this limit it would be asked forever.
	 */
	private static final int MAXIMUM_ANSWERS = 20;

	/** The answers of a request that sent none, or sent malformed ones. Empty, so never changed. */
	private static final Answer[] NO_ANSWERS = new Answer[0];

	/** Logs questions asked and malformed answers. */
	private static final Logger LOGGER = SkyveLoggerFactory.getLogger(DefaultDialogs.class);

	/**
	 * Creates the request-based dialogs. Instances hold no state, so any number can be created.
	 */
	public DefaultDialogs() {
		// nothing to see here
	}

	/**
	 * One answer sent by the client: the id of the question and the code of the chosen button.
	 *
	 * @param id	The question id; {@code null} only in malformed input.
	 * @param code	The button code; {@code null} only in malformed input.
	 */
	private record Answer(@Nullable String id, @Nullable String code) {
		// nothing to see here
	}

	/**
	 * Returns the code of the user's answer from the current request, or throws to ask the question.
	 *
	 * <p>An answer counts only when its id is this question's id and its code is one of this question's
	 * buttons that does not abort. Other answers are ignored.
	 *
	 * <p>Side effects: on the first call in a request, parses the {@value DialogsProtocol#ANSWERS_PARAMETER_NAME}
	 * parameter and caches the result as a request attribute. Malformed input is ignored as a whole, with one
	 * warning logged per request. When the question is asked, its key is logged in one line at info level.
	 *
	 * <p>Complexity: O(a) where a is the number of answers on the request.
	 *
	 * @throws QuestionException	if the request has no usable answer to the question.
	 * @throws IllegalStateException	if the current thread has no request, the request has not been marked
	 * 			askable, or the request already holds {@value #MAXIMUM_ANSWERS} answers and none of them answers
	 * 			this question.
	 */
	@Override
	public @Nonnull String ask(@Nonnull Question question) {
		HttpServletRequest request = DialogsProtocol.getAskableRequest();
		if (request == null) {
			throw new IllegalStateException("Question " + question.getKey() +
												" cannot be asked here: there is no interactive request that can ask it");
		}

		String id = id(question);
		Answer[] answers = answers(request);
		for (Answer answer : answers) {
			if (id.equals(answer.id())) {
				String code = answer.code();
				for (Button button : question.getButtons()) {
					String buttonCode = button.name();
					if (buttonCode.equals(code) && (! question.aborts(button))) {
						return buttonCode;
					}
				}
			}
		}

		if (answers.length >= MAXIMUM_ANSWERS) {
			throw new IllegalStateException("Question " + question.getKey() + " is unanswered after " +
												answers.length + " answers; its text may change on every run");
		}
		LOGGER.info("Question {} asked", question.getKey());
		throw new QuestionException(question, id);
	}

	/**
	 * Indicates whether the current thread has an HTTP request marked as able to ask.
	 *
	 * @return	{@code true} if {@link #ask(Question)} can ask the user.
	 */
	@Override
	public boolean canAsk() {
		return (DialogsProtocol.getAskableRequest() != null);
	}

	/**
	 * Computes a question's id from its key, title, message and button codes.
	 *
	 * <p>Each field is written as {@code <length>:<field>} so that no two different sets of fields
	 * produce the same text, then the text is identified with {@link BindUtil#identifier(String)}.
	 *
	 * @param question	The question.
	 * @return	The 27 character id.
	 */
	private static @Nonnull String id(@Nonnull Question question) {
		StringBuilder text = new StringBuilder(128);
		appendField(text, question.getKey());
		appendField(text, question.getTitle());
		appendField(text, question.getMessage());
		for (Button button : question.getButtons()) {
			appendField(text, button.name());
		}
		return BindUtil.identifier(text.toString());
	}

	/**
	 * Appends one field to the text identifying a question, prefixed with its length.
	 *
	 * @param text	The text being built.
	 * @param field	The field to append.
	 */
	private static void appendField(@Nonnull StringBuilder text, @Nonnull String field) {
		text.append(field.length()).append(':').append(field);
	}

	/**
	 * Returns the answers sent with a request, parsing them on the first call and caching them on the request.
	 *
	 * <p>Side effects: sets a request attribute; logs a warning when the parameter is malformed.
	 *
	 * @param request	The current request.
	 * @return	The answers, empty when there are none or the parameter is malformed; never {@code null}.
	 */
	private static @Nonnull Answer[] answers(@Nonnull HttpServletRequest request) {
		Answer[] result = (Answer[]) request.getAttribute(ANSWERS_ATTRIBUTE_NAME);
		if (result == null) {
			result = parse(UtilImpl.processStringValue(request.getParameter(DialogsProtocol.ANSWERS_PARAMETER_NAME)));
			request.setAttribute(ANSWERS_ATTRIBUTE_NAME, result);
		}
		return result;
	}

	/**
	 * Parses the answers parameter, ignoring it as a whole if it is malformed.
	 *
	 * <p>The parameter comes from the client, so bad input is logged and ignored rather than failing the
	 * request. The questions are then asked again.
	 *
	 * @param json	The parameter value, or {@code null} if it was not sent.
	 * @return	The answers, empty when there are none or the input is malformed; never {@code null}.
	 */
	private static @Nonnull Answer[] parse(@Nullable String json) {
		if (json == null) {
			return NO_ANSWERS;
		}

		Answer[] result = null;
		try {
			result = JSON.unmarshall(json, Answer[].class);
		}
		catch (Exception e) {
			LOGGER.warn("Ignoring the malformed {} request parameter: {}", DialogsProtocol.ANSWERS_PARAMETER_NAME, OWASP.sanitiseLog(e.getMessage()));
			return NO_ANSWERS;
		}
		if (result == null) {
			LOGGER.warn("Ignoring the {} request parameter as it is not an array", DialogsProtocol.ANSWERS_PARAMETER_NAME);
			return NO_ANSWERS;
		}
		for (Answer answer : result) {
			if ((answer == null) || (answer.id() == null) || (answer.code() == null)) {
				LOGGER.warn("Ignoring the {} request parameter as an answer has no id or code", DialogsProtocol.ANSWERS_PARAMETER_NAME);
				return NO_ANSWERS;
			}
		}
		return result;
	}
}
