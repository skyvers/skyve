package org.skyve.impl.dialog;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.skyve.dialog.Question;
import org.skyve.dialog.Question.Button;
import org.skyve.dialog.QuestionException;
import org.skyve.impl.web.WebContainer;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * Tests question ids, the parsing and matching of answers, the answer limit and whether a request can ask.
 */
@SuppressWarnings("static-method")
class DefaultDialogsTest {
	private final DefaultDialogs dialogs = new DefaultDialogs();

	@AfterEach
	void clearRequest() {
		WebContainer.clear();
	}

	@Test
	void theIdIsTheSameForTheSameKeyTextAndButtons() {
		String first = idOf(Question.okCancel("key", "title", "message"));
		String second = idOf(Question.okCancel("key", "title", "message").defaultButton(Button.OK).cancelAborts());

		assertEquals(first, second);
		assertEquals(27, first.length());
	}

	@Test
	void theIdChangesWhenTheKeyTextOrButtonsChange() {
		String id = idOf(Question.okCancel("key", "title", "message"));

		assertAll(
				() -> assertNotEquals(id, idOf(Question.okCancel("key2", "title", "message")), "key"),
				() -> assertNotEquals(id, idOf(Question.okCancel("key", "title2", "message")), "title"),
				() -> assertNotEquals(id, idOf(Question.okCancel("key", "title", "message2")), "message"),
				() -> assertNotEquals(id, idOf(Question.yesNo("key", "title", "message")), "buttons"));
	}

	@Test
	void theLengthPrefixKeepsTheFieldsApart() {
		assertAll(
				() -> assertNotEquals(idOf(Question.okCancel("a|b", "c", "m")), idOf(Question.okCancel("a", "b|c", "m")), "separator in a field"),
				() -> assertNotEquals(idOf(Question.okCancel("ab", "c", "m")), idOf(Question.okCancel("a", "bc", "m")), "fields joined"),
				() -> assertNotEquals(idOf(Question.okCancel("k", "1:t", "m")), idOf(Question.okCancel("k1:", "t", "m")), "length digits in a field"));
	}

	@Test
	void aMatchingAnswerIsReturnedAndTheParameterIsParsedOncePerRequest() {
		Question question = Question.okCancel("key", "title", "message");
		String id = idOf(question);
		HttpServletRequest request = askableRequest(answers(id, "OK"));

		assertEquals("OK", dialogs.ask(question));
		assertTrue(dialogs.confirm(question));
		verify(request, times(1)).getParameter(DialogsProtocol.ANSWERS_PARAMETER_NAME);
	}

	@Test
	void theNegativeButtonIsAnAnswerWhenItDoesNotAbort() {
		Question okCancel = Question.okCancel("key", "title", "message");
		Question yesNo = Question.yesNo("key", "title", "message");
		askableRequest(answers(idOf(okCancel), "CANCEL", idOf(yesNo), "NO"));

		assertAll(
				() -> assertEquals("CANCEL", dialogs.ask(okCancel)),
				() -> assertFalse(dialogs.confirm(okCancel)),
				() -> assertEquals("NO", dialogs.ask(yesNo)),
				() -> assertFalse(dialogs.confirm(yesNo)));
	}

	@Test
	void yesConfirms() {
		Question question = Question.yesNo("key", "title", "message");
		askableRequest(answers(idOf(question), "YES"));

		assertTrue(dialogs.confirm(question));
	}

	@Test
	void anAnswerWithAnotherIdIsIgnored() {
		Question question = Question.okCancel("key", "title", "message");
		String staleId = idOf(Question.okCancel("key", "title", "an older message"));
		askableRequest(answers(staleId, "OK"));

		QuestionException e = assertThrows(QuestionException.class, () -> dialogs.ask(question));
		assertSame(question, e.getQuestion());
		assertEquals(idOf(question), e.getId());
	}

	@Test
	void anAnswerWithAButtonThatAbortsIsIgnored() {
		Question question = Question.okCancel("key", "title", "message").cancelAborts();
		askableRequest(answers(idOf(question), "CANCEL"));

		assertThrows(QuestionException.class, () -> dialogs.ask(question));
	}

	@Test
	void anAnswerWithACodeThatIsNotOneOfTheButtonsIsIgnored() {
		Question question = Question.okCancel("key", "title", "message");
		String id = idOf(question);

		assertAll(
				() -> assertAsked(question, answers(id, "YES")),
				() -> assertAsked(question, answers(id, "ok")),
				() -> assertAsked(question, answers(id, "")));
	}

	@Test
	void aMalformedParameterIsIgnoredAsAWhole() {
		Question question = Question.okCancel("key", "title", "message");
		String id = idOf(question);
		String valid = "{\"id\":\"" + id + "\",\"code\":\"OK\"}";

		assertAll(Stream.of("not json",
								"[" + valid,
								"{\"id\":\"" + id + "\",\"code\":\"OK\"}",
								"\"OK\"",
								"null",
								"[null]",
								"[" + valid + ",{\"id\":\"" + id + "\"}]",
								"[" + valid + ",{\"code\":\"OK\"}]",
								"[" + valid + ",{\"id\":null,\"code\":\"OK\"}]",
								"[" + valid + ",{\"id\":\"" + id + "\",\"code\":null}]",
								"[{\"id\":1,\"code\":\"OK\"}]",
								"[[\"" + id + "\",\"OK\"]]")
						.map(json -> (Executable) () -> assertAsked(question, json)));
	}

	@Test
	void aBlankParameterHasNoAnswers() {
		Question question = Question.okCancel("key", "title", "message");

		assertAll(
				() -> assertAsked(question, ""),
				() -> assertAsked(question, "  "));
	}

	@Test
	void aRequestWithTheMaximumAnswersFailsAQuestionItDoesNotAnswer() {
		Question question = Question.okCancel("key", "title", "message");
		askableRequest(answers(otherAnswers(20)));

		IllegalStateException e = assertThrows(IllegalStateException.class, () -> dialogs.ask(question));
		assertTrue(e.getMessage().contains("key"), e.getMessage());
	}

	@Test
	void aRequestWithOneFewerThanTheMaximumAnswersStillAsks() {
		Question question = Question.okCancel("key", "title", "message");
		askableRequest(answers(otherAnswers(19)));

		assertThrows(QuestionException.class, () -> dialogs.ask(question));
	}

	@Test
	void aRequestWithTheMaximumAnswersStillReturnsAnAnswerItHolds() {
		Question question = Question.okCancel("key", "title", "message");
		List<String> answers = otherAnswers(19);
		answers.add(idOf(question));
		answers.add("OK");
		askableRequest(answers(answers));

		assertEquals("OK", dialogs.ask(question));
	}

	@Test
	void aRequestCanAskOnlyWhenMarkedAskable() {
		Question question = Question.okCancel("key", "title", "message");

		assertFalse(dialogs.canAsk(), "no request");
		IllegalStateException noRequest = assertThrows(IllegalStateException.class, () -> dialogs.ask(question));
		assertTrue(noRequest.getMessage().contains("key"), noRequest.getMessage());

		HttpServletRequest request = request(null);
		WebContainer.setHttpServletRequestResponse(request, mock(HttpServletResponse.class));
		assertFalse(dialogs.canAsk(), "not askable");
		IllegalStateException notAskable = assertThrows(IllegalStateException.class, () -> dialogs.ask(question));
		assertTrue(notAskable.getMessage().contains("key"), notAskable.getMessage());

		request.setAttribute(DialogsProtocol.ASKABLE_ATTRIBUTE_NAME, Boolean.FALSE);
		assertFalse(dialogs.canAsk(), "askable false");

		request.setAttribute(DialogsProtocol.ASKABLE_ATTRIBUTE_NAME, Boolean.TRUE);
		assertTrue(dialogs.canAsk(), "askable");
	}

	/**
	 * Asserts that a question is asked rather than answered by a request with the given answers parameter.
	 */
	private void assertAsked(Question question, String answers) {
		askableRequest(answers);
		assertThrows(QuestionException.class, () -> dialogs.ask(question), answers);
	}

	/**
	 * Returns the id {@link DefaultDialogs#ask(Question)} gives a question.
	 * Side effect: binds a new askable request with no answers to the current thread.
	 */
	private String idOf(Question question) {
		askableRequest(null);
		return assertThrows(QuestionException.class, () -> dialogs.ask(question)).getId();
	}

	/**
	 * Binds a new askable request with the given answers parameter to the current thread.
	 */
	private static HttpServletRequest askableRequest(String answers) {
		HttpServletRequest result = request(answers);
		result.setAttribute(DialogsProtocol.ASKABLE_ATTRIBUTE_NAME, Boolean.TRUE);
		WebContainer.setHttpServletRequestResponse(result, mock(HttpServletResponse.class));
		return result;
	}

	/**
	 * Creates a mock request that keeps its attributes and has the given answers parameter.
	 */
	private static HttpServletRequest request(String answers) {
		HttpServletRequest result = mock(HttpServletRequest.class);
		Map<String, Object> attributes = new HashMap<>();
		when(result.getAttribute(anyString())).thenAnswer(invocation -> attributes.get(invocation.getArgument(0)));
		doAnswer(invocation -> attributes.put(invocation.getArgument(0), invocation.getArgument(1)))
				.when(result).setAttribute(anyString(), org.mockito.ArgumentMatchers.any());
		when(result.getParameter(DialogsProtocol.ANSWERS_PARAMETER_NAME)).thenReturn(answers);
		return result;
	}

	/**
	 * Returns ids and codes, alternately, for answers to questions other than those asked in these tests.
	 */
	private static List<String> otherAnswers(int count) {
		List<String> result = new ArrayList<>(count * 2);
		for (int i = 0; i < count; i++) {
			result.add("other" + i);
			result.add("OK");
		}
		return result;
	}

	/**
	 * Writes an answers parameter from ids and codes, alternately.
	 */
	private static String answers(String... idsAndCodes) {
		return answers(List.of(idsAndCodes));
	}

	/**
	 * Writes an answers parameter from ids and codes, alternately.
	 */
	private static String answers(List<String> idsAndCodes) {
		StringBuilder result = new StringBuilder("[");
		for (int i = 0, l = idsAndCodes.size(); i < l; i += 2) {
			if (i > 0) {
				result.append(',');
			}
			result.append("{\"id\":\"").append(idsAndCodes.get(i)).append("\",\"code\":\"").append(idsAndCodes.get(i + 1)).append("\"}");
		}
		return result.append(']').toString();
	}
}
