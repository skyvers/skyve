package org.skyve.impl.dialog;

import java.util.List;

import org.skyve.dialog.Question;
import org.skyve.dialog.Question.Button;
import org.skyve.dialog.QuestionException;
import org.skyve.impl.web.HttpServletRequestResponse;
import org.skyve.impl.web.WebContainer;
import org.skyve.util.OWASP;
import org.skyve.util.Util;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.servlet.http.HttpServletRequest;

/**
 * The protocol of server-side questions shared by the renderers, their clients and the
 * {@link org.skyve.dialog.Dialogs} implementations.
 *
 * <ul>
 * <li>A renderer marks each request that its client can resend ({@link #markAskable()}), and unmarks it once a
 * question can no longer be resent, before rendering its response ({@link #unmarkAskable()}).</li>
 * <li>A {@code Dialogs} implementation asks only on a marked request ({@link #getAskableRequest()}).</li>
 * <li>The renderer sends a question to its client as {@link #toJSON(QuestionException)}; SmartClient sends it in
 * a response with status {@value #QUESTION_RESPONSE_STATUS}.</li>
 * <li>The client resends the request with the answers given so far in the {@value #ANSWERS_PARAMETER_NAME}
 * parameter, as a JSON array of {@code {"id": "...", "code": "OK"}} objects.</li>
 * </ul>
 *
 * <p>Threading: <em>thread-safe</em>. This class holds no state; it reads and marks the request bound to the
 * current thread by {@link WebContainer}.
 */
public final class DialogsProtocol {
	/**
	 * The request parameter holding the answers the client has collected for the current gesture.
	 */
	public static final String ANSWERS_PARAMETER_NAME = "_q";

	/**
	 * The request attribute that marks a request as able to ask questions; see {@link #markAskable()}.
	 */
	public static final String ASKABLE_ATTRIBUTE_NAME = "skyve.dialogs.askable";

	/**
	 * The status of a SmartClient response that asks a question. Clients treat it as a failure because it is
	 * negative, and it is outside SmartClient's own error codes.
	 */
	public static final int QUESTION_RESPONSE_STATUS = -50;

	private DialogsProtocol() {
		// no instances
	}

	/**
	 * Marks the current HTTP request, if there is one, as able to ask questions, as its client can resend it with
	 * the answers. Renderers call this at the start of each path that can rerun its request.
	 */
	public static void markAskable() {
		HttpServletRequestResponse requestResponse = WebContainer.getHttpServletRequestResponse();
		if (requestResponse != null) {
			requestResponse.getRequest().setAttribute(ASKABLE_ATTRIBUTE_NAME, Boolean.TRUE);
		}
	}

	/**
	 * Unmarks the current HTTP request, if there is one, as able to ask questions. Renderers call this once
	 * the request's work is done, before its response is rendered, so that a question asked from then on throws
	 * {@link IllegalStateException} instead of being resent.
	 */
	public static void unmarkAskable() {
		HttpServletRequestResponse requestResponse = WebContainer.getHttpServletRequestResponse();
		if (requestResponse != null) {
			requestResponse.getRequest().removeAttribute(ASKABLE_ATTRIBUTE_NAME);
		}
	}

	/**
	 * Returns the request bound to the current thread if it has been marked as able to ask.
	 *
	 * @return	The request, or {@code null} if there is no request or it cannot ask.
	 */
	public static @Nullable HttpServletRequest getAskableRequest() {
		HttpServletRequestResponse requestResponse = WebContainer.getHttpServletRequestResponse();
		if (requestResponse == null) {
			return null;
		}
		HttpServletRequest result = requestResponse.getRequest();
		return Boolean.TRUE.equals(result.getAttribute(ASKABLE_ATTRIBUTE_NAME)) ? result : null;
	}

	/**
	 * Writes a question as the JSON object a client needs to show it and answer it.
	 *
	 * <p>The object holds the question's {@code id}, {@code title} and {@code message}, and its
	 * {@code buttons} in order, each with its {@code code}, its {@code label}, whether it {@code aborts}
	 * and whether it is the {@code default}. Labels are translated with the {@code ui.dialog.*} keys for
	 * the current user's locale. Strings are escaped for JSON only; the title and message are plain text,
	 * and the client must escape them for its display.
	 *
	 * @param question	The question thrown by {@link org.skyve.dialog.Dialogs#ask(Question)}.
	 * @return	The JSON object text.
	 */
	public static @Nonnull String toJSON(@Nonnull QuestionException question) {
		Question q = question.getQuestion();
		Button defaultButton = q.getDefaultButton();
		StringBuilder result = new StringBuilder(256);
		result.append("{\"id\":\"").append(OWASP.escapeJsonString(question.getId()));
		result.append("\",\"title\":\"").append(OWASP.escapeJsonString(q.getTitle()));
		result.append("\",\"message\":\"").append(OWASP.escapeJsonString(q.getMessage()));
		result.append("\",\"buttons\":[");
		List<Button> buttons = q.getButtons();
		for (int i = 0, l = buttons.size(); i < l; i++) {
			Button button = buttons.get(i);
			if (i > 0) {
				result.append(',');
			}
			result.append("{\"code\":\"").append(button.name());
			result.append("\",\"label\":\"").append(OWASP.escapeJsonString(Util.nullSafeI18n(labelKey(button))));
			result.append("\",\"aborts\":").append(q.aborts(button));
			result.append(",\"default\":").append(button == defaultButton).append('}');
		}
		result.append("]}");
		return result.toString();
	}

	/**
	 * Returns the i18n key of a button's label.
	 *
	 * @param button	The button.
	 * @return	{@code ui.dialog.} followed by the button's code in lower case.
	 */
	private static @Nonnull String labelKey(@Nonnull Button button) {
		return switch (button) {
			case OK -> "ui.dialog.ok";
			case CANCEL -> "ui.dialog.cancel";
			case YES -> "ui.dialog.yes";
			case NO -> "ui.dialog.no";
		};
	}
}
