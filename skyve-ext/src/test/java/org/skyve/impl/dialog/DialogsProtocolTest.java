package org.skyve.impl.dialog;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.dialog.Question;
import org.skyve.dialog.Question.Button;
import org.skyve.dialog.QuestionException;
import org.skyve.impl.web.WebContainer;
import org.skyve.util.JSON;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * Tests marking a request as askable and the JSON a question is sent to the client as.
 */
@SuppressWarnings("static-method")
class DialogsProtocolTest {
	@AfterEach
	void clearRequest() {
		WebContainer.clear();
	}

	@Test
	void markingAndUnmarkingTheCurrentRequestDecidesWhetherItCanAsk() {
		HttpServletRequest request = request();
		WebContainer.setHttpServletRequestResponse(request, mock(HttpServletResponse.class));
		assertNull(DialogsProtocol.getAskableRequest(), "not marked");

		DialogsProtocol.markAskable();
		assertSame(request, DialogsProtocol.getAskableRequest(), "marked");
		assertEquals(Boolean.TRUE, request.getAttribute(DialogsProtocol.ASKABLE_ATTRIBUTE_NAME));

		DialogsProtocol.unmarkAskable();
		assertNull(DialogsProtocol.getAskableRequest(), "unmarked");
		assertNull(request.getAttribute(DialogsProtocol.ASKABLE_ATTRIBUTE_NAME));
	}

	@Test
	void onlyTrueMarksARequest() {
		HttpServletRequest request = request();
		WebContainer.setHttpServletRequestResponse(request, mock(HttpServletResponse.class));

		request.setAttribute(DialogsProtocol.ASKABLE_ATTRIBUTE_NAME, Boolean.FALSE);

		assertNull(DialogsProtocol.getAskableRequest());
	}

	@Test
	void withoutARequestNothingIsMarkedOrAskable() {
		DialogsProtocol.markAskable();
		DialogsProtocol.unmarkAskable();

		assertNull(DialogsProtocol.getAskableRequest());
	}

	@Test
	void theJSONHoldsTheQuestionAndItsButtonsWithTheTextEscapedForJSON() throws Exception {
		Question question = Question.yesNo("key", "A \"quoted\" title", "Line 1\nLine 2 \\ </script> <b>bold</b>")
										.defaultButton(Button.YES)
										.cancelAborts();

		@SuppressWarnings("unchecked")
		Map<String, Object> json = (Map<String, Object>) JSON.unmarshall(DialogsProtocol.toJSON(new QuestionException(question, "id")));

		assertEquals("id", json.get("id"));
		assertEquals("A \"quoted\" title", json.get("title"));
		assertEquals("Line 1\nLine 2 \\ </script> <b>bold</b>", json.get("message"));
		@SuppressWarnings("unchecked")
		List<Map<String, Object>> buttons = (List<Map<String, Object>>) json.get("buttons");
		assertEquals(2, buttons.size());
		assertEquals(Map.of("code", "YES", "label", "ui.dialog.yes", "aborts", Boolean.FALSE, "default", Boolean.TRUE), buttons.get(0));
		assertEquals(Map.of("code", "NO", "label", "ui.dialog.no", "aborts", Boolean.TRUE, "default", Boolean.FALSE), buttons.get(1));
	}

	@Test
	void theJSONLabelsOKAndCancel() {
		Question question = Question.okCancel("key", "title", "message");

		String json = DialogsProtocol.toJSON(new QuestionException(question, "id"));

		assertTrue(json.contains("{\"code\":\"OK\",\"label\":\"ui.dialog.ok\",\"aborts\":false,\"default\":false}"), json);
		assertTrue(json.contains("{\"code\":\"CANCEL\",\"label\":\"ui.dialog.cancel\",\"aborts\":false,\"default\":true}"), json);
	}

	/**
	 * Creates a mock request that keeps its attributes.
	 */
	private static HttpServletRequest request() {
		HttpServletRequest result = mock(HttpServletRequest.class);
		Map<String, Object> attributes = new HashMap<>();
		when(result.getAttribute(anyString())).thenAnswer(invocation -> attributes.get(invocation.getArgument(0)));
		doAnswer(invocation -> attributes.put(invocation.getArgument(0), invocation.getArgument(1)))
				.when(result).setAttribute(anyString(), any());
		doAnswer(invocation -> attributes.remove(invocation.getArgument(0)))
				.when(result).removeAttribute(anyString());
		return result;
	}
}
