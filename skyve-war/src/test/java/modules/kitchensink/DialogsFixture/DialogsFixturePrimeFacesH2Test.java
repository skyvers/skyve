package modules.kitchensink.DialogsFixture;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.exception.ExceptionUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.primefaces.context.PrimeRequestContext;
import org.skyve.EXT;
import org.skyve.dialog.Question;
import org.skyve.dialog.QuestionException;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.ConversationEndedException;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.dialog.DialogsProtocol;
import org.skyve.impl.dialog.DialogsStaticSingleton;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.WebContainer;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.FacesUtil;
import org.skyve.impl.web.faces.SkyveFacesPhaseListener;
import org.skyve.impl.web.faces.actions.DeleteAction;
import org.skyve.impl.web.faces.actions.ExecuteActionAction;
import org.skyve.impl.web.faces.actions.RemoveAction;
import org.skyve.impl.web.faces.actions.RerenderAction;
import org.skyve.impl.web.faces.actions.SaveAction;
import org.skyve.impl.web.faces.actions.ZoomOutAction;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.persistence.Persistence;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import jakarta.el.ELContext;
import jakarta.faces.FacesException;
import jakarta.el.ELResolver;
import jakarta.faces.application.FacesMessage;
import jakarta.faces.component.UIViewRoot;
import jakarta.faces.context.ExternalContext;
import jakarta.faces.context.FacesContext;
import jakarta.faces.context.PartialViewContext;
import jakarta.faces.event.PhaseEvent;
import jakarta.faces.event.PhaseId;
import modules.kitchensink.domain.DialogsFixture;
import modules.kitchensink.domain.DialogsFixtureRow;
import modules.test.AbstractSkyveTest;

/**
 * Asks the dialogs fixture's questions through the PrimeFaces actions and phase listener, with a mock Faces context
 * standing in for the Faces lifecycle of one AJAX request.
 */
@SuppressWarnings("java:S1192") // repeated test literals
class DialogsFixturePrimeFacesH2Test extends AbstractSkyveTest {
	private static final ObjectMapper JSON_MAPPER = new ObjectMapper();
	private static final String ASK = "SKYVE.PF.ask(";
	private static final String MANAGED_BEAN_NAME = "skyve";

	/**
	 * Reaches the protected {@link FacesContext#setCurrentInstance(FacesContext)}.
	 */
	private abstract static class FacesContextBridge extends FacesContext {
		static void setCurrent(FacesContext context) {
			setCurrentInstance(context);
		}
	}

	private boolean accessControl;
	private MockHttpServletRequest request;
	private FacesContext fc;
	private List<FacesMessage> messages;
	private List<String> renderIds;
	private FacesView view;
	private AbstractWebContext webContext;

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
		FacesContextBridge.setCurrent(null);
		WebContainer.clear();
		// The phase listener commits and closes the thread's persistence, so give the base class one to clean up with
		AbstractPersistence.get().setUser(u);
	}

	@Test
	void anActionThatAsksRollsBackClearsItsMessagesAndRenderingAndQueuesTheQuestion() throws Exception {
		startRequest(DialogsFixture.newInstance(), null);
		messages.add(new FacesMessage("A growl from earlier in the run"));
		renderIds.add("form:outcome");

		new ExecuteActionAction(view, "SaveThenAsk", null, null).execute();

		assertTrue(messages.isEmpty(), messages::toString);
		assertTrue(renderIds.isEmpty(), renderIds::toString);
		assertTrue(FacesAction.isQuestionAsked(fc));
		JsonNode question = askedQuestion();
		assertEquals("The fixture has been saved. Keep the save?", question.get("message").asText());
		assertEquals("OK", question.get("buttons").get(0).get("label").asText());
		// The action saved before it asked, and the save is rolled back
		p.commit(false);
		assertEquals(0, count());
	}

	@Test
	void anAnsweredActionCarriesOnWithoutAQuestion() throws Exception {
		DialogsFixture fixture = DialogsFixture.newInstance();
		startRequest(fixture, null);
		new ExecuteActionAction(view, "AskOnce", null, null).execute();
		String answers = answers(askedQuestion(), "OK");

		startRequest(fixture, answers);
		new ExecuteActionAction(view, "AskOnce", null, null).execute();

		assertFalse(FacesAction.isQuestionAsked(fc));
		assertTrue(scripts().isEmpty(), scripts()::toString);
		assertEquals("Ask once answered OK", ((DialogsFixture) view.getBean()).getOutcome());
	}

	@Test
	void aWrappedQuestionIsStillAsked() throws Exception {
		startRequest(DialogsFixture.newInstance(), null);

		new ExecuteActionAction(view, "AskWrapped", null, null).execute();

		assertEquals("Carry on with Ask wrapped?", askedQuestion().get("message").asText());
		assertTrue(messages.isEmpty(), messages::toString);
	}

	@Test
	void saveAsksFromPreSaveAndSavesOnceAnswered() throws Exception {
		DialogsFixture fixture = DialogsFixture.newInstance();
		fixture.setAskOnSave(Boolean.TRUE);
		startRequest(fixture, null);

		new SaveAction(view, false).execute();
		JsonNode question = askedQuestion();
		assertEquals("Save this dialogs fixture?", question.get("message").asText());
		p.commit(false);
		assertEquals(0, count());

		p.begin();
		startRequest(fixture, answers(question, "OK"));
		new SaveAction(view, false).execute();
		assertFalse(FacesAction.isQuestionAsked(fc));
		p.commit(false);
		assertEquals(1, count());
	}

	@Test
	void deleteAsksFromPreDeleteAndDeletesOnceAnswered() throws Exception {
		DialogsFixture fixture = persistedFixture(Boolean.TRUE);
		startRequest(fixture, null);

		new DeleteAction(view).execute();
		JsonNode question = askedQuestion();
		assertEquals("Delete this dialogs fixture?", question.get("message").asText());
		p.commit(false);
		assertEquals(1, count());

		p.begin();
		startRequest(p.retrieve(DialogsFixture.MODULE_NAME, DialogsFixture.DOCUMENT_NAME, fixture.getBizId()), answers(question, "OK"));
		new DeleteAction(view).execute();
		assertFalse(FacesAction.isQuestionAsked(fc));
		p.commit(false);
		assertEquals(0, count());
	}

	@Test
	void aRerenderAsksWithTheNewValue() throws Exception {
		DialogsFixture fixture = DialogsFixture.newInstance();
		fixture.setText("xyz");
		startRequest(fixture, null);

		new RerenderAction(view, DialogsFixture.textPropertyName, false).execute();

		assertEquals("You changed the text to xyz. Carry on?", askedQuestion().get("message").asText());
	}

	@Test
	void zoomOutAsksFromPreExecute() throws Exception {
		DialogsFixture fixture = persistedFixture(Boolean.FALSE);
		startRequest(fixture, null);
		zoomIntoRow(fixture);

		new ZoomOutAction(view).execute();

		assertEquals("ZoomOut this row?", askedQuestion().get("message").asText());
	}

	@Test
	void removeOfAZoomedRowAsksFromPreExecute() throws Exception {
		DialogsFixture fixture = persistedFixture(Boolean.FALSE);
		startRequest(fixture, null);
		zoomIntoRow(fixture);

		new RemoveAction(view, null, null, null).execute();

		assertEquals("Remove this row?", askedQuestion().get("message").asText());
		assertEquals(1, fixture.getRows().size());
	}

	@Test
	void onlyTheFirstQuestionOfActionsRunByAnotherActionIsQueued() throws Exception {
		DialogsFixture fixture = persistedFixture(Boolean.FALSE);
		startRequest(fixture, null);
		String rowId = fixture.getRows().get(0).getBizId();

		// Removing a row inline runs its removed handlers through their own actions, and both ask
		new RemoveAction(view, DialogsFixture.rowsPropertyName, rowId, List.of("AskOnce", "AskYesNo")).execute();

		assertEquals(1, scripts().size(), scripts()::toString);
		assertEquals("Carry on with Ask once?", askedQuestion().get("message").asText());
	}

	@Test
	void onceARemovedHandlerAsksTheRemainingHandlersDoNotRun() throws Exception {
		DialogsFixture fixture = persistedFixture(Boolean.FALSE);
		startRequest(fixture, null);
		new ExecuteActionAction(view, "AskYesNo", null, null).execute();
		String yes = answers(askedQuestion(), "YES");

		startRequest(fixture, yes);
		String rowId = fixture.getRows().get(0).getBizId();
		new RemoveAction(view, DialogsFixture.rowsPropertyName, rowId, List.of("AskOnce", "AskYesNo")).execute();

		assertEquals("Carry on with Ask once?", askedQuestion().get("message").asText());
		// AskYesNo is answered, so it would have carried on and set the outcome had it run
		assertNull(((DialogsFixture) view.getBean()).getOutcome());
	}

	@Test
	void theRequestCannotAskOnceTheResponseRenders() {
		startRequest(DialogsFixture.newInstance(), null);
		request.setAttribute(DialogsProtocol.ASKABLE_ATTRIBUTE_NAME, Boolean.TRUE);

		beforePhase(PhaseId.INVOKE_APPLICATION);
		assertTrue(EXT.getDialogs().canAsk());

		beforePhase(PhaseId.RENDER_RESPONSE);
		assertFalse(EXT.getDialogs().canAsk());
		Question question = Question.okCancel("render", "Render", "Can rendering ask?");
		assertThrows(IllegalStateException.class, () -> EXT.getDialogs().ask(question));
	}

	@Test
	void anActionThatCannotAskReportsAnError() {
		startRequest(DialogsFixture.newInstance(), null);

		// Like the load and lookup actions, this action does not mark the request as able to ask
		new FacesAction<Void>() {
			@Override
			public Void callback() throws Exception {
				EXT.getDialogs().ask(Question.okCancel("load", "Load", "Can a form load ask?"));
				return null;
			}
		}.execute();

		assertFalse(FacesAction.isQuestionAsked(fc));
		assertTrue(scripts().isEmpty(), scripts()::toString);
		assertEquals(1, messages.size());
		assertEquals(FacesMessage.SEVERITY_ERROR, messages.get(0).getSeverity());
	}

	@Test
	void okThatAsksDoesNotLeaveThePage() throws Exception {
		DialogsFixture fixture = DialogsFixture.newInstance();
		fixture.setAskOnSave(Boolean.TRUE);
		startRequest(fixture, null);

		view.ok();

		askedQuestion();
		assertTrue(messages.isEmpty(), messages::toString);
	}

	@Test
	void okThatIsAnsweredLeavesThePage() throws Exception {
		DialogsFixture fixture = DialogsFixture.newInstance();
		fixture.setAskOnSave(Boolean.TRUE);
		startRequest(fixture, null);
		view.ok();
		String answers = answers(askedQuestion(), "OK");

		startRequest(fixture, answers);
		view.ok();

		assertEquals(List.of("SKYVE.PF.popHistory(true)"), scripts());
	}

	@Test
	void saveThatAsksDoesNotSaySaved() throws Exception {
		DialogsFixture fixture = DialogsFixture.newInstance();
		fixture.setAskOnSave(Boolean.TRUE);
		startRequest(fixture, null);

		view.save();

		askedQuestion();
		assertTrue(messages.isEmpty(), messages::toString);
	}

	@Test
	void deleteThatAsksDoesNotLeaveThePage() throws Exception {
		DialogsFixture fixture = persistedFixture(Boolean.TRUE);
		startRequest(fixture, null);

		view.delete();

		askedQuestion();
		assertTrue(messages.isEmpty(), messages::toString);
	}

	@Test
	void saveWithoutAQuestionSaysSaved() {
		startRequest(DialogsFixture.newInstance(), null);

		view.save();

		assertEquals(List.of("Saved"), messages.stream().map(FacesMessage::getSummary).toList());
	}

	@Test
	void anEarlierMessageStillStopsOKSaveAndDeleteFromSucceeding() {
		DialogsFixture fixture = persistedFixture(Boolean.FALSE);
		FacesMessage earlier = new FacesMessage(FacesMessage.SEVERITY_WARN, "Earlier", "Earlier");

		startRequest(DialogsFixture.newInstance(), null);
		messages.add(earlier);
		view.ok();
		assertTrue(scripts().isEmpty(), scripts()::toString);

		startRequest(DialogsFixture.newInstance(), null);
		messages.add(earlier);
		view.save();
		assertEquals(List.of(earlier), messages);

		startRequest(fixture, null);
		messages.add(earlier);
		view.delete();
		assertTrue(scripts().isEmpty(), scripts()::toString);
	}

	@Test
	void deleteThatIsAnsweredLeavesThePage() throws Exception {
		DialogsFixture fixture = persistedFixture(Boolean.TRUE);
		startRequest(fixture, null);
		view.delete();
		String answers = answers(askedQuestion(), "OK");

		startRequest(p.retrieve(DialogsFixture.MODULE_NAME, DialogsFixture.DOCUMENT_NAME, fixture.getBizId()), answers);
		view.delete();

		assertEquals(List.of("SKYVE.PF.popHistory(true)"), scripts());
	}

	@Test
	void aMessageExceptionIsStillReportedAsMessages() {
		startRequest(DialogsFixture.newInstance(), null);

		new FacesAction<Void>() {
			@Override
			public Void callback() throws Exception {
				throw new ValidationException(new Message("Not valid"));
			}
		}.execute();

		assertFalse(FacesAction.isQuestionAsked(fc));
		assertEquals(List.of("Not valid"), messages.stream().map(FacesMessage::getSummary).toList());
	}

	@Test
	void withoutARequestTheAskableActionsCannotAsk() {
		DialogsFixture fixture = DialogsFixture.newInstance();
		fixture.setAskOnSave(Boolean.TRUE);
		startRequest(fixture, null);
		WebContainer.clear();

		new SaveAction(view, false).execute();
		new ExecuteActionAction(view, "AskOnce", null, null).execute();

		assertFalse(FacesAction.isQuestionAsked(fc));
		assertEquals(2, messages.size(), messages::toString);
		assertEquals(FacesMessage.SEVERITY_ERROR, messages.get(0).getSeverity());
		assertEquals(FacesMessage.SEVERITY_ERROR, messages.get(1).getSeverity());
	}

	@Test
	void thePhaseListenerCachesTheConversationAfterAnInformationMessageButNotAfterAnError() {
		startRequest(DialogsFixture.newInstance(), null);
		messages.add(new FacesMessage(FacesMessage.SEVERITY_INFO, "Information", "Information"));
		afterRenderResponse();
		assertNotNull(StateUtil.getCachedConversation(webContext.getWebId(), null));

		// The phase listener closed the request's persistence, so start the next request with a new one
		AbstractPersistence persistence = AbstractPersistence.get();
		persistence.setUser(u);
		persistence.begin();
		startRequest(DialogsFixture.newInstance(), null);
		messages.add(new FacesMessage(FacesMessage.SEVERITY_ERROR, "Error", "Error"));
		afterRenderResponse();
		String webId = webContext.getWebId();
		assertThrows(ConversationEndedException.class, () -> StateUtil.getCachedConversation(webId, null));
	}

	@Test
	void postRenderCannotAskAQuestion() {
		startRequest(DialogsFixture.newInstance(), null);
		request.setAttribute(DialogsProtocol.ASKABLE_ATTRIBUTE_NAME, Boolean.TRUE);
		Bizlet<Bean> bizlet = mock(Bizlet.class);
		doAnswer(invocation -> EXT.getDialogs().ask(Question.okCancel("postRender", "Post render", "Can postRender ask?")))
				.when(bizlet).postRender(any(), any());
		view.setPostRender(bizlet, webContext.getCurrentBean());

		FacesException e = assertThrows(FacesException.class, this::afterRenderResponse);

		assertNotNull(ExceptionUtils.throwableOfType(e, IllegalStateException.class));
		assertNull(ExceptionUtils.throwableOfType(e, QuestionException.class));
		assertNull(request.getAttribute(DialogsProtocol.ASKABLE_ATTRIBUTE_NAME));
	}

	@Test
	void postRenderStillRunsWithoutARequest() {
		startRequest(DialogsFixture.newInstance(), null);
		WebContainer.clear();
		Bizlet<Bean> bizlet = mock(Bizlet.class);
		Bean bean = webContext.getCurrentBean();
		view.setPostRender(bizlet, bean);

		afterRenderResponse();

		verify(bizlet).postRender(bean, webContext);
	}

	@Test
	void afterAQuestionThePhaseListenerSkipsPostRenderAndTheSaveButDehydrates() {
		startRequest(DialogsFixture.newInstance(), null);
		Bizlet<Bean> bizlet = mock(Bizlet.class);
		view.setPostRender(bizlet, webContext.getCurrentBean());
		new ExecuteActionAction(view, "AskOnce", null, null).execute();

		afterRenderResponse();

		verify(bizlet, never()).postRender(any(), any());
		String webId = webContext.getWebId();
		assertThrows(ConversationEndedException.class, () -> StateUtil.getCachedConversation(webId, null));
		assertNull(view.getWebContext());
		assertEquals(webContext.getWebId(), view.getDehydratedWebId());
	}

	@Test
	void withoutAQuestionThePhaseListenerPostRendersAndSaves() {
		startRequest(DialogsFixture.newInstance(), null);
		Bizlet<Bean> bizlet = mock(Bizlet.class);
		Bean bean = webContext.getCurrentBean();
		view.setPostRender(bizlet, bean);

		afterRenderResponse();

		verify(bizlet).postRender(bean, webContext);
		assertNotNull(StateUtil.getCachedConversation(webContext.getWebId(), null));
		assertNull(view.getWebContext());
	}

	/**
	 * Starts a mock Faces AJAX request on the fixture, with the given answers.
	 */
	private void startRequest(Bean bean, String answers) {
		request = new MockHttpServletRequest("POST", "/kitchensink/DialogsFixture.xhtml");
		request.addHeader("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/141.0.0.0 Safari/537.36");
		request.getSession(true);
		if (answers != null) {
			request.addParameter(DialogsProtocol.ANSWERS_PARAMETER_NAME, answers);
		}
		WebContainer.setHttpServletRequestResponse(request, new MockHttpServletResponse());

		messages = new ArrayList<>();
		renderIds = new ArrayList<>();
		PartialViewContext partial = mock(PartialViewContext.class);
		when(partial.getRenderIds()).thenReturn(renderIds);
		when(Boolean.valueOf(partial.isAjaxRequest())).thenReturn(Boolean.TRUE);
		ExternalContext external = mock(ExternalContext.class);
		when(external.getRequest()).thenReturn(request);
		UIViewRoot viewRoot = mock(UIViewRoot.class);
		Map<String, Object> viewRootAttributes = new HashMap<>();
		viewRootAttributes.put(FacesUtil.MANAGED_BEAN_NAME_KEY, MANAGED_BEAN_NAME);
		when(viewRoot.getAttributes()).thenReturn(viewRootAttributes);

		fc = mock(FacesContext.class);
		Map<Object, Object> attributes = new HashMap<>();
		when(fc.getAttributes()).thenReturn(attributes);
		when(fc.getPartialViewContext()).thenReturn(partial);
		when(fc.getExternalContext()).thenReturn(external);
		when(fc.getViewRoot()).thenReturn(viewRoot);
		when(fc.getMessages()).thenAnswer(invocation -> messages.iterator());
		when(fc.getMessageList()).thenAnswer(invocation -> List.copyOf(messages));
		doAnswer(invocation -> Boolean.valueOf(messages.add(invocation.getArgument(1)))).when(fc).addMessage(any(), any(FacesMessage.class));
		doAnswer(invocation -> Boolean.valueOf(messages.add(invocation.getArgument(1)))).when(fc).addMessage(isNull(), any(FacesMessage.class));
		when(fc.getMaximumSeverity()).thenAnswer(invocation -> {
			FacesMessage.Severity result = null;
			for (FacesMessage message : messages) {
				if ((result == null) || (message.getSeverity().getOrdinal() > result.getOrdinal())) {
					result = message.getSeverity();
				}
			}
			return result;
		});
		FacesContextBridge.setCurrent(fc);

		view = new FacesView();
		webContext = mockWebContext();
		webContext.setConversation(AbstractPersistence.get());
		webContext.setCurrentBean(bean);
		view.setWebContext(webContext);
	}

	/**
	 * Zooms the view into the first row of the fixture.
	 */
	private void zoomIntoRow(DialogsFixture fixture) {
		String binding = DialogsFixture.rowsPropertyName + "ElementById(" + fixture.getRows().get(0).getBizId() + ")";
		view.getZoomInBindings().push(binding);
		view.setViewBinding(binding);
	}

	/**
	 * Runs the phase listener before the given phase.
	 */
	private void beforePhase(PhaseId phaseId) {
		PhaseEvent event = mock(PhaseEvent.class);
		when(event.getFacesContext()).thenReturn(fc);
		when(event.getPhaseId()).thenReturn(phaseId);

		new SkyveFacesPhaseListener().beforePhase(event);
	}

	/**
	 * Runs the phase listener after the response is rendered, resolving the view through the EL context.
	 */
	private void afterRenderResponse() {
		ELResolver resolver = mock(ELResolver.class);
		when(resolver.getValue(any(), isNull(), eq(MANAGED_BEAN_NAME))).thenReturn(view);
		ELContext elContext = mock(ELContext.class);
		when(elContext.getELResolver()).thenReturn(resolver);
		when(fc.getELContext()).thenReturn(elContext);
		PhaseEvent event = mock(PhaseEvent.class);
		when(event.getFacesContext()).thenReturn(fc);
		when(event.getPhaseId()).thenReturn(PhaseId.RENDER_RESPONSE);

		new SkyveFacesPhaseListener().afterPhase(event);
	}

	private List<String> scripts() {
		return PrimeRequestContext.getCurrentInstance(fc).getScriptsToExecute();
	}

	/**
	 * Returns the question in the one {@code SKYVE.PF.ask} script queued.
	 */
	private JsonNode askedQuestion() throws Exception {
		List<String> scripts = scripts();
		assertEquals(1, scripts.size(), scripts::toString);
		String script = scripts.get(0);
		assertTrue(script.startsWith(ASK) && script.endsWith(")"), script);
		return JSON_MAPPER.readTree(script.substring(ASK.length(), script.length() - 1));
	}

	private static String answers(JsonNode question, String code) throws Exception {
		return JSON_MAPPER.writeValueAsString(List.of(Map.of("id", question.get("id").asText(), "code", code)));
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
	 * Counts the fixtures in the database in a new transaction.
	 */
	private int count() {
		Persistence persistence = p;
		persistence.begin();
		Number result = persistence.newDocumentQuery(DialogsFixture.MODULE_NAME, DialogsFixture.DOCUMENT_NAME)
										.addAggregateProjection(AggregateFunction.Count, Bean.DOCUMENT_ID, "count")
										.scalarResult(Number.class);
		assertNotNull(result);
		return result.intValue();
	}
}
