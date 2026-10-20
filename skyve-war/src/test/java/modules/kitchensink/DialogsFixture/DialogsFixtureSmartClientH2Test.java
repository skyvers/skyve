package modules.kitchensink.DialogsFixture;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.dialog.DialogsProtocol;
import org.skyve.impl.dialog.DialogsStaticSingleton;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.WebContainer;
import org.skyve.impl.web.service.smartclient.SmartClientEditServlet;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.persistence.Persistence;
import org.skyve.web.WebContext;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.mock.web.MockHttpSession;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

import modules.kitchensink.domain.DialogsFixture;
import modules.kitchensink.domain.DialogsFixtureRow;
import modules.test.AbstractSkyveTest;

/**
 * Asks the dialogs fixture's questions through {@link SmartClientEditServlet}, as the SmartClient edit view does:
 * each request is answered with status -50 and the question, and the resend with the answers in {@code _q} carries on.
 */
@SuppressWarnings("java:S1192") // repeated test literals
class DialogsFixtureSmartClientH2Test extends AbstractSkyveTest {
	private static final ObjectMapper JSON_MAPPER = new ObjectMapper();
	private static final int QUESTION = -50;

	private boolean accessControl;

	/**
	 * Asks through the request, as the renderers do, rather than with the test answers the base class installs.
	 * Switches off access control, as the test user has no menus to grant access to the fixture's view.
	 */
	@BeforeEach
	void askThroughTheRequestWithoutAccessControl() {
		DialogsStaticSingleton.setDefault();
		accessControl = UtilImpl.ACCESS_CONTROL;
		UtilImpl.ACCESS_CONTROL = false;
	}

	@AfterEach
	void restore() {
		UtilImpl.ACCESS_CONTROL = accessControl;
		WebContainer.clear();
		// Each request commits and closes the thread's persistence, so give the base class one to clean up with
		AbstractPersistence.get().setUser(u);
	}

	@Test
	void aCustomActionAsksAndTheRerunCarriesOnWithTheAnswer() throws Exception {
		SmartClient client = new SmartClient(u);
		client.fetchNew();

		JsonNode asked = client.update("AskOnce", null, null);
		assertEquals(QUESTION, status(asked));
		JsonNode question = asked.get("question");
		assertEquals("Ask once", question.get("title").asText());
		assertEquals("Carry on with Ask once?", question.get("message").asText());
		assertEquals("This action needs a confirmation that this app cannot show: Carry on with Ask once?", asked.get("data").asText());
		assertButton(question.get("buttons").get(0), "OK", "OK", false, true);
		assertButton(question.get("buttons").get(1), "CANCEL", "Cancel", true, false);

		JsonNode answered = client.update("AskOnce", null, answers(question, "OK"));
		assertEquals(0, status(answered), answered::toString);
		assertEquals("Ask once answered OK", value(answered, DialogsFixture.outcomePropertyName));
	}

	@Test
	void twoQuestionsAreAskedInOrderAndBothAnswersReachTheCode() throws Exception {
		SmartClient client = new SmartClient(u);
		client.fetchNew();

		JsonNode first = client.update("AskTwice", null, null).get("question");
		assertEquals("First question: Yes or No?", first.get("message").asText());
		assertButton(first.get("buttons").get(0), "YES", "Yes", false, false);
		assertButton(first.get("buttons").get(1), "NO", "No", false, true);

		String answers = answers(first, "NO");
		JsonNode second = client.update("AskTwice", null, answers).get("question");
		assertEquals("You answered NO to the first question. Carry on?", second.get("message").asText());

		JsonNode answered = client.update("AskTwice", null, answers(answers, second, "OK"));
		assertEquals(0, status(answered), answered::toString);
		assertEquals("Ask twice answered NO then OK", value(answered, DialogsFixture.outcomePropertyName));
	}

	@Test
	void aNegativeButtonThatDoesNotAbortCarriesOn() throws Exception {
		SmartClient client = new SmartClient(u);
		client.fetchNew();

		JsonNode question = client.update("AskYesNo", null, null).get("question");
		JsonNode answered = client.update("AskYesNo", null, answers(question, "NO"));

		assertEquals(0, status(answered), answered::toString);
		assertEquals("Yes or No answered No", value(answered, DialogsFixture.outcomePropertyName));
	}

	@Test
	void aWrappedQuestionIsStillAsked() throws Exception {
		SmartClient client = new SmartClient(u);
		client.fetchNew();

		JsonNode asked = client.update("AskWrapped", null, null);
		assertEquals(QUESTION, status(asked), asked::toString);

		JsonNode answered = client.update("AskWrapped", null, answers(asked.get("question"), "OK"));
		assertEquals("Ask wrapped answered OK", value(answered, DialogsFixture.outcomePropertyName));
	}

	@Test
	void saveAsksFromPreSaveAndWritesOnlyOnceAnswered() throws Exception {
		SmartClient client = new SmartClient(u);
		client.fetchNew();
		client.set(DialogsFixture.askOnSavePropertyName, Boolean.TRUE);

		JsonNode asked = client.update("Save", null, null);
		assertEquals(QUESTION, status(asked), asked::toString);
		assertEquals("Save this dialogs fixture?", asked.get("question").get("message").asText());
		assertEquals(0, count());

		JsonNode answered = client.update("Save", null, answers(asked.get("question"), "OK"));
		assertEquals(0, status(answered), answered::toString);
		assertEquals(1, count());
		assertEquals("preSave answered OK", retrieveOnly().getOutcome());
	}

	@Test
	void okAsksFromPreSaveAndWritesOnlyOnceAnswered() throws Exception {
		SmartClient client = new SmartClient(u);
		client.fetchNew();
		client.set(DialogsFixture.askOnSavePropertyName, Boolean.TRUE);

		JsonNode asked = client.update("OK", null, null);
		assertEquals(QUESTION, status(asked), asked::toString);
		assertEquals(0, count());

		JsonNode answered = client.update("OK", null, answers(asked.get("question"), "OK"));
		assertEquals(0, status(answered), answered::toString);
		assertEquals(1, count());
	}

	@Test
	void deleteAsksFromPreDeleteAndDeletesOnlyOnceAnswered() throws Exception {
		DialogsFixture fixture = persistedFixture(Boolean.TRUE);
		SmartClient client = new SmartClient(u);
		client.fetch(fixture.getBizId());

		JsonNode asked = client.remove(null);
		assertEquals(QUESTION, status(asked), asked::toString);
		assertEquals("Delete this dialogs fixture?", asked.get("question").get("message").asText());
		assertEquals(1, count());

		JsonNode answered = client.remove(answers(asked.get("question"), "OK"));
		assertEquals(0, status(answered), answered::toString);
		assertEquals(0, count());
	}

	@Test
	void aRerenderAsksWithTheNewValueAndAStaleAnswerIsAskedAgain() throws Exception {
		SmartClient client = new SmartClient(u);
		client.fetchNew();
		client.set(DialogsFixture.textPropertyName, "abc");

		JsonNode question = client.update(null, DialogsFixture.textPropertyName, null).get("question");
		assertEquals("You changed the text to abc. Carry on?", question.get("message").asText());

		// The user changes the text again before answering, so the answer is for the old text
		client.set(DialogsFixture.textPropertyName, "abd");
		JsonNode askedAgain = client.update(null, DialogsFixture.textPropertyName, answers(question, "OK"));
		assertEquals(QUESTION, status(askedAgain), askedAgain::toString);
		JsonNode newQuestion = askedAgain.get("question");
		assertEquals("You changed the text to abd. Carry on?", newQuestion.get("message").asText());
		assertNotEquals(question.get("id").asText(), newQuestion.get("id").asText());

		JsonNode answered = client.update(null, DialogsFixture.textPropertyName, answers(newQuestion, "OK"));
		assertEquals(0, status(answered), answered::toString);
		assertEquals("Text change to abd answered OK", value(answered, DialogsFixture.outcomePropertyName));
	}

	@Test
	void zoomOutAsksFromPreExecute() throws Exception {
		DialogsFixture fixture = persistedFixture(Boolean.FALSE);
		DialogsFixtureRow row = fixture.getRows().get(0);
		SmartClient client = new SmartClient(u);
		client.fetch(fixture.getBizId());
		String rowBinding = DialogsFixture.rowsPropertyName + "ElementById(" + row.getBizId() + ")";

		JsonNode asked = client.zoomOut(rowBinding, row, null);
		assertEquals(QUESTION, status(asked), asked::toString);
		assertEquals("ZoomOut this row?", asked.get("question").get("message").asText());

		JsonNode answered = client.zoomOut(rowBinding, row, answers(asked.get("question"), "OK"));
		assertEquals(0, status(answered), answered::toString);
	}

	@Test
	void anAbortLeavesTheDatabaseAndTheCachedConversationAsTheyWere() throws Exception {
		SmartClient client = new SmartClient(u);
		client.fetchNew();
		Bean before = client.cachedBean();
		client.set(DialogsFixture.textPropertyName, "changed");

		JsonNode asked = client.update("SaveThenAsk", null, null);
		assertEquals(QUESTION, status(asked), asked::toString);

		// The user cancels, so nothing more is sent
		assertEquals(0, count());
		Bean after = client.cachedBean();
		assertTrue(after.isNotPersisted());
		assertNull(((DialogsFixture) after).getText());
		assertEquals(before.getBizId(), after.getBizId());
	}

	@Test
	void aSaveThenAQuestionThenOKWritesOnceWithOneOptimisticLockIncrement() throws Exception {
		DialogsFixture fixture = persistedFixture(Boolean.FALSE);
		int versionBefore = fixture.getBizVersion().intValue();
		SmartClient client = new SmartClient(u);
		client.fetch(fixture.getBizId());
		client.set(DialogsFixture.textPropertyName, "saved then asked");

		JsonNode asked = client.update("SaveThenAsk", null, null);
		assertEquals(QUESTION, status(asked), asked::toString);
		assertEquals(versionBefore, retrieveOnly().getBizVersion().intValue());

		JsonNode answered = client.update("SaveThenAsk", null, answers(asked.get("question"), "OK"));
		assertEquals(0, status(answered), answered::toString);
		DialogsFixture saved = retrieveOnly();
		assertEquals(versionBefore + 1, saved.getBizVersion().intValue());
		assertEquals("saved then asked", saved.getText());
		assertEquals(1, count());
	}

	@Test
	void aQuestionWhoseTextChangesOnEveryRunFailsAfterTheMaximumAnswers() throws Exception {
		SmartClient client = new SmartClient(u);
		client.fetchNew();

		String answers = null;
		int questions = 0;
		JsonNode response = client.update("AskEveryRun", null, answers);
		while (status(response) == QUESTION) {
			questions++;
			assertTrue(questions <= 20, "asked more than 20 times");
			answers = answers(answers, response.get("question"), "OK");
			response = client.update("AskEveryRun", null, answers);
		}

		assertEquals(20, questions);
		assertEquals(-1, status(response), response::toString);
		assertFalse(response.has("question"));
	}

	@Test
	void aQuestionDuringAFetchIsAnError() throws Exception {
		SmartClient client = new SmartClient(u);
		client.fetchNew();

		// A form load cannot ask, so an action run by a fetch fails instead of asking
		JsonNode response = client.request("fetch", "AskOnce", null, null, null, null);

		assertEquals(-1, status(response), response::toString);
		assertEquals(0, response.get("totalRows").asInt());
		assertFalse(response.has("question"));
	}

	@Test
	void theResponseIsRenderedOnceTheRequestCannotAsk() throws Exception {
		SmartClient client = new SmartClient(u);
		client.fetchNew();
		DialogsFixture fetched = (DialogsFixture) client.cachedBean();
		RenderWatchingFixture watching = new RenderWatchingFixture();
		watching.setBizId(fetched.getBizId());
		watching.setBizCustomer(fetched.getBizCustomer());
		watching.setBizDataGroupId(fetched.getBizDataGroupId());
		watching.setBizUserId(fetched.getBizUserId());
		client.cacheBean(watching);
		RenderWatchingFixture.CAN_ASK_WHEN_READ.clear();

		// Changing Action Text rerenders without asking, and rendering the response reads the outcome last
		JsonNode response = client.update(null, DialogsFixture.actionTextPropertyName, null);

		assertEquals(0, status(response), response::toString);
		List<Boolean> canAsk = RenderWatchingFixture.CAN_ASK_WHEN_READ;
		assertFalse(canAsk.isEmpty());
		// The conversation is committed after rendering, so a question asked while rendering must not be resent
		assertEquals(Boolean.FALSE, canAsk.get(canAsk.size() - 1), canAsk::toString);
	}

	@Test
	void aQuestionDoesNotReplaceTheCSRFToken() throws Exception {
		SmartClient client = new SmartClient(u);
		client.fetchNew();
		Integer token = client.csrf;
		String sessionId = client.session.getId();
		assertTrue(StateUtil.checkToken(sessionId, token));

		JsonNode asked = client.update("AskOnce", null, null);
		assertEquals(QUESTION, status(asked), asked::toString);
		Integer notRegistered = Integer.valueOf(client.lastResponse.getHeader("X-CSRF-TOKEN"));
		assertNotEquals(token, notRegistered);
		assertTrue(StateUtil.checkToken(sessionId, token));
		assertFalse(StateUtil.checkToken(sessionId, notRegistered));

		JsonNode answered = client.update("AskOnce", null, answers(asked.get("question"), "OK"));
		assertEquals(0, status(answered), answered::toString);
		assertFalse(StateUtil.checkToken(sessionId, token));
		assertTrue(StateUtil.checkToken(sessionId, client.csrf));
	}

	/**
	 * Saves a fixture with one row, outside any request, and commits it.
	 */
	private DialogsFixture persistedFixture(Boolean askOnDelete) {
		DialogsFixture result = DialogsFixture.newInstance();
		result.setAskOnDelete(askOnDelete);
		DialogsFixtureRow row = DialogsFixtureRow.newInstance();
		row.setName("row");
		row.setParent(result);
		result.getRows().add(row);
		result = p.save(result);
		p.commit(false);
		p.begin();
		return result;
	}

	/**
	 * Counts the fixtures in the database with a new persistence, as each request closes the thread's persistence.
	 */
	private int count() {
		Persistence persistence = freshPersistence();
		try {
			Number result = persistence.newDocumentQuery(DialogsFixture.MODULE_NAME, DialogsFixture.DOCUMENT_NAME)
											.addAggregateProjection(AggregateFunction.Count, Bean.DOCUMENT_ID, "count")
											.scalarResult(Number.class);
			assertNotNull(result);
			return result.intValue();
		}
		finally {
			persistence.commit(true);
		}
	}

	/**
	 * Retrieves the only fixture in the database with a new persistence.
	 */
	private DialogsFixture retrieveOnly() {
		Persistence persistence = freshPersistence();
		try {
			DialogsFixture result = persistence.newDocumentQuery(DialogsFixture.MODULE_NAME, DialogsFixture.DOCUMENT_NAME).beanResult();
			assertNotNull(result);
			return result;
		}
		finally {
			persistence.commit(true);
		}
	}

	private AbstractPersistence freshPersistence() {
		AbstractPersistence result = AbstractPersistence.get();
		result.setUser(u);
		result.evictAllCached();
		result.begin();
		return result;
	}

	private static String value(JsonNode response, String binding) {
		return response.get("data").get(binding).asText();
	}

	private static int status(JsonNode response) {
		return response.get("status").asInt();
	}

	private static void assertButton(JsonNode button, String code, String label, boolean aborts, boolean isDefault) {
		assertEquals(code, button.get("code").asText());
		assertEquals(label, button.get("label").asText());
		assertEquals(Boolean.valueOf(aborts), Boolean.valueOf(button.get("aborts").asBoolean()), code + " aborts");
		assertEquals(Boolean.valueOf(isDefault), Boolean.valueOf(button.get("default").asBoolean()), code + " default");
	}

	private static String answers(JsonNode question, String code) throws Exception {
		return answers(null, question, code);
	}

	/**
	 * Adds an answer to the answers sent so far, as the client does.
	 */
	private static String answers(String answers, JsonNode question, String code) throws Exception {
		ArrayNode result = (answers == null) ? JSON_MAPPER.createArrayNode() : (ArrayNode) JSON_MAPPER.readTree(answers);
		result.addObject().put("id", question.get("id").asText()).put("code", code);
		return JSON_MAPPER.writeValueAsString(result);
	}

	/**
	 * Records whether a question could be asked each time its outcome is read.
	 */
	private static final class RenderWatchingFixture extends DialogsFixture {
		private static final long serialVersionUID = 1L;
		private static final List<Boolean> CAN_ASK_WHEN_READ = new ArrayList<>();

		@Override
		public String getOutcome() {
			CAN_ASK_WHEN_READ.add(Boolean.valueOf(EXT.getDialogs().canAsk()));
			return super.getOutcome();
		}
	}

	/**
	 * Plays the part of the SmartClient edit view: it keeps the form values, the conversation id and the CSRF token,
	 * and sends them with each request as {@code editView.js} does.
	 */
	private static final class SmartClient {
		private final SmartClientEditServlet servlet = new SmartClientEditServlet() {
			private static final long serialVersionUID = 1L;
		};
		private final MockHttpSession session = new MockHttpSession();
		private ObjectNode values;
		private String webId;
		private Integer csrf;
		private MockHttpServletResponse lastResponse;
		private String document = DialogsFixture.DOCUMENT_NAME;

		private SmartClient(User user) {
			session.setAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME, user);
			StateUtil.addSession(user.getId(), session);
		}

		private void fetchNew() throws Exception {
			fetch(null);
		}

		private void fetch(String bizId) throws Exception {
			JsonNode response = request("fetch", null, null, null, bizId, null);
			assertEquals(0, status(response), response::toString);
			values = (ObjectNode) response.get("data").get(0);
			webId = values.get(AbstractWebContext.CONTEXT_NAME).asText();
		}

		private void set(String binding, Object value) {
			if (value instanceof Boolean b) {
				values.put(binding, b.booleanValue());
			}
			else {
				values.put(binding, (String) value);
			}
		}

		private JsonNode update(String action, String source, String answers) throws Exception {
			JsonNode result = request("update", action, source, null, values.get(Bean.DOCUMENT_ID).asText(), answers);
			if (status(result) >= 0) {
				values = (ObjectNode) result.get("data");
			}
			return result;
		}

		private JsonNode remove(String answers) throws Exception {
			return request("remove", null, null, null, values.get(Bean.DOCUMENT_ID).asText(), answers);
		}

		private JsonNode zoomOut(String binding, DialogsFixtureRow row, String answers) throws Exception {
			ObjectNode rowValues = JSON_MAPPER.createObjectNode();
			rowValues.put(Bean.DOCUMENT_ID, row.getBizId());
			rowValues.put(DialogsFixtureRow.namePropertyName, row.getName());
			ObjectNode formValues = values;
			try {
				values = rowValues;
				document = DialogsFixtureRow.DOCUMENT_NAME;
				return request("update", "ZoomOut", null, binding, row.getBizId(), answers);
			}
			finally {
				values = formValues;
				document = DialogsFixture.DOCUMENT_NAME;
			}
		}

		/**
		 * Reads the bean from the cached conversation, as the next request would.
		 */
		private Bean cachedBean() {
			MockHttpServletRequest request = newRequest();
			AbstractWebContext webContext = StateUtil.getCachedConversation(webId, request);
			assertNotNull(webContext);
			return webContext.getCurrentBean();
		}

		/**
		 * Replaces the bean in the cached conversation, as the servlet caches it at the end of a request.
		 */
		private void cacheBean(Bean bean) {
			AbstractWebContext webContext = StateUtil.getCachedConversation(webId, newRequest());
			assertNotNull(webContext);
			AbstractPersistence persistence = webContext.getConversation();
			persistence.setForThread();
			persistence.begin();
			webContext.setCurrentBean(bean);
			StateUtil.commitAndCacheConversation(webContext);
		}

		private MockHttpServletRequest newRequest() {
			MockHttpServletRequest result = new MockHttpServletRequest("POST", "/smartedit");
			result.setSession(session);
			result.addHeader("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/141.0.0.0 Safari/537.36");
			return result;
		}

		private JsonNode request(String operation, String action, String source, String binding, String bizId, String answers)
		throws Exception {
			MockHttpServletRequest request = newRequest();
			request.addParameter("_operationType", operation);
			request.addParameter(AbstractWebContext.MODULE_NAME, DialogsFixture.MODULE_NAME);
			request.addParameter(AbstractWebContext.DOCUMENT_NAME, document);
			request.addParameter("_ecnt", "0");
			request.addParameter("_ccnt", "0");
			if (webId != null) {
				request.addParameter(AbstractWebContext.CONTEXT_NAME, webId);
			}
			if (csrf != null) {
				request.addParameter(AbstractWebContext.CSRF_TOKEN_NAME, csrf.toString());
			}
			if (action != null) {
				request.addParameter(AbstractWebContext.ACTION_NAME, action);
			}
			if (source != null) {
				request.addParameter(AbstractWebContext.SOURCE_NAME, source);
			}
			if (binding != null) {
				request.addParameter(AbstractWebContext.BINDING_NAME, binding);
			}
			if (bizId != null) {
				request.addParameter(Bean.DOCUMENT_ID, bizId);
			}
			if (values != null) {
				ObjectNode bean = values.deepCopy();
				bean.remove(AbstractWebContext.CONTEXT_NAME);
				request.addParameter("bean", JSON_MAPPER.writeValueAsString(bean));
			}
			if (answers != null) {
				request.addParameter(DialogsProtocol.ANSWERS_PARAMETER_NAME, answers);
			}

			MockHttpServletResponse response = new MockHttpServletResponse();
			WebContainer.setHttpServletRequestResponse(request, response);
			try {
				servlet.service(request, response);
			}
			finally {
				WebContainer.clear();
			}
			lastResponse = response;
			JsonNode result = JSON_MAPPER.readTree(response.getContentAsString()).get("response");
			if (status(result) >= 0) {
				csrf = Integer.valueOf(response.getHeader("X-CSRF-TOKEN"));
			}
			return result;
		}
	}
}
