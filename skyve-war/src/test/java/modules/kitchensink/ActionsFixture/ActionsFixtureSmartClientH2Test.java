package modules.kitchensink.ActionsFixture;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.DownloadServlet;
import org.skyve.impl.web.UserAgent;
import org.skyve.impl.web.WebContainer;
import org.skyve.impl.web.service.smartclient.SmartClientEditServlet;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.web.WebContext;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.mock.web.MockHttpSession;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;

import modules.admin.domain.MonitoringDashboard;
import modules.admin.domain.UserProxy;
import modules.kitchensink.domain.ActionsFixture;
import modules.kitchensink.domain.ActionsFixtureRow;
import modules.kitchensink.domain.DialogsFixture;
import modules.kitchensink.domain.DialogsFixtureRow;
import modules.test.AbstractSkyveTest;

/**
 * Requests the actions fixture's actions through {@link SmartClientEditServlet}, as the SmartClient edit view does:
 * an action the view offers a reachable trigger for runs, and any other is refused with status -51, without running,
 * saving or changing the conversation. A prepared download is then requested from {@link DownloadServlet}, which
 * serves only the download the conversation prepared, even though preparing it disabled Download Once.
 */
@SuppressWarnings("java:S1192") // repeated test literals
class ActionsFixtureSmartClientH2Test extends AbstractSkyveTest {
	private static final ObjectMapper JSON_MAPPER = new ObjectMapper();
	/** SmartClientEditServlet.UNAVAILABLE_RESPONSE_STATUS, which is private. */
	private static final int UNAVAILABLE = -51;
	private static final String UNAVAILABLE_MESSAGE = "This action is no longer available.";
	static final String USER_AGENT = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/141.0.0.0 Safari/537.36";
	static final String DOWNLOAD_ONCE = "DownloadOnce";
	/** What {@code DownloadOnce} downloads. */
	private static final String DOWNLOAD_CONTENT = "Actions Fixture";

	private boolean accessControl;

	/**
	 * Switches off access control, as the test user has no menus to grant access to the fixture's view.
	 */
	@BeforeEach
	void withoutAccessControl() {
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
	void aToolbarActionRunsWhileEnabledAndIsRefusedOnceDisabled() throws Exception {
		SmartClient client = SmartClient.fixture(u);
		client.fetchNew();

		client.rerenderWith(ActionsFixture.disableToolbarActionPropertyName, Boolean.TRUE);
		assertRefusedAndNotRun(client, client.update("RunFromToolbar"));

		client.rerenderWith(ActionsFixture.disableToolbarActionPropertyName, Boolean.FALSE);
		assertRan(client.update("RunFromToolbar"), "Run From Toolbar ran");
	}

	@Test
	void anEventTargetRunsWithoutAButtonAndIsRefusedOnceItsFieldIsHidden() throws Exception {
		SmartClient client = SmartClient.fixture(u);
		client.fetchNew();

		client.rerenderWith(ActionsFixture.hideEventTextPropertyName, Boolean.TRUE);
		assertRefusedAndNotRun(client, client.update("RunFromEvent"));

		client.rerenderWith(ActionsFixture.hideEventTextPropertyName, Boolean.FALSE);
		assertRan(client.update("RunFromEvent"), "Run From Event ran");
	}

	@Test
	void anActionWithNoTriggerIsRefused() throws Exception {
		SmartClient client = SmartClient.fixture(u);
		client.fetchNew();

		assertRefusedAndNotRun(client, client.update("RunWithoutTrigger"));
	}

	@Test
	void aFormButtonActionIsRefusedOnceItsFormIsDisabled() throws Exception {
		SmartClient client = SmartClient.fixture(u);
		client.fetchNew();

		client.rerenderWith(ActionsFixture.disableFormPropertyName, Boolean.TRUE);
		assertRefusedAndNotRun(client, client.update("RunFromFormButton"));

		client.rerenderWith(ActionsFixture.disableFormPropertyName, Boolean.FALSE);
		assertRan(client.update("RunFromFormButton"), "Run From Form Button ran");
	}

	@Test
	void aTabButtonActionIsRefusedOnceItsTabIsDisabled() throws Exception {
		SmartClient client = SmartClient.fixture(u);
		client.fetchNew();

		client.rerenderWith(ActionsFixture.disableTabPropertyName, Boolean.TRUE);
		assertRefusedAndNotRun(client, client.update("RunFromTabButton"));

		client.rerenderWith(ActionsFixture.disableTabPropertyName, Boolean.FALSE);
		assertRan(client.update("RunFromTabButton"), "Run From Tab Button ran");
	}

	@Test
	void aSharedClassRunsWhileEitherNamedActionIsAvailable() throws Exception {
		SmartClient client = SmartClient.fixture(u);
		client.fetchNew();

		// The client requests a custom action by its class
		client.rerenderWith(ActionsFixture.disableSharedFirstPropertyName, Boolean.TRUE);
		client.rerenderWith(ActionsFixture.disableSharedSecondPropertyName, Boolean.TRUE);
		assertRefusedAndNotRun(client, client.update("RunShared"));

		client.rerenderWith(ActionsFixture.disableSharedSecondPropertyName, Boolean.FALSE);
		assertRan(client.update("RunShared"), "Run Shared ran");

		client = SmartClient.fixture(u);
		client.fetchNew();
		client.rerenderWith(ActionsFixture.disableSharedSecondPropertyName, Boolean.TRUE);
		assertRan(client.update("RunShared"), "Run Shared ran");

		// A request by an action's name, which differs from its class, is refused rather than looked up as a class
		client = SmartClient.fixture(u);
		client.fetchNew();
		assertRefusedAndNotRun(client, client.update("RunSharedFirst"));
	}

	@Test
	void okAndSaveAreRefusedWhileSaveDisabledIsTrue() throws Exception {
		SmartClient client = SmartClient.fixture(u);
		client.fetchNew();
		client.rerenderWith(ActionsFixture.disableSavePropertyName, Boolean.TRUE);

		assertRefused(client.update("Save"));
		assertRefused(client.update("OK"));
		assertEquals(0, count(ActionsFixture.MODULE_NAME, ActionsFixture.DOCUMENT_NAME));

		client.rerenderWith(ActionsFixture.disableSavePropertyName, Boolean.FALSE);
		JsonNode okayed = client.update("OK");
		assertEquals(0, status(okayed), okayed::toString);
		assertEquals(1, count(ActionsFixture.MODULE_NAME, ActionsFixture.DOCUMENT_NAME));
		JsonNode saved = client.update("Save");
		assertEquals(0, status(saved), saved::toString);
		assertEquals(1, count(ActionsFixture.MODULE_NAME, ActionsFixture.DOCUMENT_NAME));
	}

	@Test
	void deleteIsRefusedWhenItsConditionIsTrue() throws Exception {
		ActionsFixture undeletable = persistedFixture(true, false);
		ActionsFixture deletable = persistedFixture(false, false);

		SmartClient client = SmartClient.fixture(u);
		client.fetch(undeletable.getBizId());
		assertRefused(client.remove(ActionsFixture.DOCUMENT_NAME));
		assertEquals(2, count(ActionsFixture.MODULE_NAME, ActionsFixture.DOCUMENT_NAME));

		client = SmartClient.fixture(u);
		client.fetch(deletable.getBizId());
		JsonNode deleted = client.remove(ActionsFixture.DOCUMENT_NAME);
		assertEquals(0, status(deleted), deleted::toString);
		assertEquals(1, count(ActionsFixture.MODULE_NAME, ActionsFixture.DOCUMENT_NAME));
	}

	@Test
	void aDeleteWithNoConversationDeletesNothingAndIsNotRefused() throws Exception {
		ActionsFixture fixture = persistedFixture(true, false);
		SmartClient client = SmartClient.fixture(u);

		// No conversation, so there is no bean to delete: the request deletes nothing, as before the check
		JsonNode response = client.request("remove", null, null, null, null, ActionsFixture.DOCUMENT_NAME, fixture.getBizId());

		assertEquals(0, status(response), response::toString);
		assertEquals(1, count(ActionsFixture.MODULE_NAME, ActionsFixture.DOCUMENT_NAME));
	}

	@Test
	void aRowLinkActionRunsForItsRowAndIsRefusedForARowThatHidesTheLink() throws Exception {
		ActionsFixture fixture = persistedFixture(false, false);
		ActionsFixtureRow shown = row(fixture, "shown");
		ActionsFixtureRow hidden = row(fixture, "hidden");
		SmartClient client = SmartClient.fixture(u);
		client.fetch(fixture.getBizId());

		JsonNode ran = client.update("RunFromRow", null, ActionsFixture.rowsPropertyName, ActionsFixtureRow.DOCUMENT_NAME, shown.getBizId());
		assertEquals(0, status(ran), ran::toString);
		assertEquals("Run From Row ran", row(ran, shown.getBizId()).get(ActionsFixtureRow.outcomePropertyName).asText());

		JsonNode refused = client.update("RunFromRow", null, ActionsFixture.rowsPropertyName, ActionsFixtureRow.DOCUMENT_NAME, hidden.getBizId());
		assertRefused(refused);
		ActionsFixture cached = (ActionsFixture) client.cachedBean();
		assertNull(cached.getRowsElementById(hidden.getBizId()).getOutcome());
	}

	@Test
	void aRowLinkRendersACallThatTheServerAccepts() throws Exception {
		ActionsFixture fixture = persistedFixture(false, false);
		ActionsFixtureRow shown = row(fixture, "shown");
		SmartClient client = SmartClient.fixture(u);
		client.fetch(fixture.getBizId());

		// The rendered link, as SmartClient puts it in the row's grid cell
		String link = rowLink(client.values, shown.getBizId());
		assertTrue(link.startsWith("<a href=\""), link);
		String href = link.substring("<a href=\"".length(), link.indexOf('"', "<a href=\"".length()));

		// The confirmation's quotes and ampersand stay inside the href, and its braces are not read as bindings
		assertTrue(href.startsWith("javascript:isc.ask('Run \\x26#34;Run From Row\\x26#34; \\x26amp; record {outcome}?',function(b){if(b){"), href);
		assertTrue(href.endsWith("}},{title:isc.i18nMessages.editView_Confirm})"), href);

		// The link calls doAction with the row action's class, not its name, validating first as the action does not say otherwise
		Matcher call = Pattern.compile("\\._view\\.doAction\\('([^']*)',(true|false),'([^']*)','([^']*)','([^']*)','([^']*)'\\)").matcher(href);
		assertTrue(call.find(), href);
		assertEquals("RunFromRow", call.group(1));
		assertEquals("true", call.group(2));
		assertEquals(ActionsFixture.rowsPropertyName, call.group(3));
		assertEquals(ActionsFixtureRow.MODULE_NAME, call.group(4));
		assertEquals(ActionsFixtureRow.DOCUMENT_NAME, call.group(5));
		assertEquals(shown.getBizId(), call.group(6));

		// Sending what the link sends runs the row action
		JsonNode ran = client.update(call.group(1), null, call.group(3), call.group(5), call.group(6));
		assertEquals(0, status(ran), ran::toString);
		assertEquals("Run From Row ran", row(ran, shown.getBizId()).get(ActionsFixtureRow.outcomePropertyName).asText());

		// Sending the action's name, as the link did before, is refused as no action has that class
		JsonNode byName = client.update("runFromRowLink", null, call.group(3), call.group(5), call.group(6));
		assertRefused(byName);
	}

	@Test
	void aRowLinkIsCheckedInTheRowBeansOwnViewNotTheRequestedDocuments() throws Exception {
		ActionsFixture fixture = persistedFixture(false, false);
		SmartClient client = SmartClient.fixture(u);
		client.fetch(fixture.getBizId());

		// The forged document's view declares no Run From Row, but the row's own view does, so the check passes
		// and the request fails afterwards, as the forged document has no such action
		JsonNode response = client.update("RunFromRow", null, ActionsFixture.rowsPropertyName, DialogsFixtureRow.DOCUMENT_NAME, row(fixture, "shown").getBizId());

		assertEquals(-1, status(response), response::toString);
	}

	@Test
	void aZoomedInRowIsCheckedAgainstTheRowView() throws Exception {
		// The fixture disables its own OK and offers Run From Toolbar, so each outcome below holds only for the row view
		ActionsFixture fixture = persistedFixture(false, true);
		ActionsFixtureRow shown = row(fixture, "shown");
		SmartClient client = SmartClient.fixture(u);
		client.fetch(fixture.getBizId());

		// Run From Row is out of the row view's action panel and nothing in that view triggers it
		assertRefused(client.zoomed("RunFromRow", shown));
		assertNull(((ActionsFixture) client.cachedBean()).getRowsElementById(shown.getBizId()).getOutcome());
		// Run From Toolbar is the fixture's, not the row's
		assertRefused(client.zoomed("RunFromToolbar", shown));

		// OK is one of the row view's defaults
		JsonNode okayed = client.zoomed("OK", shown);
		assertEquals(0, status(okayed), okayed::toString);
	}

	@Test
	void aDownloadIsPreparedWhileAvailableAndRefusedOnceItDisabledItself() throws Exception {
		SmartClient client = SmartClient.fixture(u);
		client.fetchNew();

		JsonNode prepared = client.update("DownloadOnce");
		assertEquals(0, status(prepared), prepared::toString);
		assertTrue(prepared.get("data").has("_redirectUrl"), prepared::toString);
		assertTrue(prepared.get("data").get(ActionsFixture.downloadedPropertyName).asBoolean());

		// Another action replaces the outcome, which a second prepare would set back
		assertRan(client.update("RunFromToolbar"), "Run From Toolbar ran");
		assertRefused(client.update("DownloadOnce"));
		assertEquals("Run From Toolbar ran", ((ActionsFixture) client.cachedBean()).getOutcome());
	}

	@Test
	void aDownloadThatTheConversationDidNotPrepareIsRefused() throws Exception {
		SmartClient client = SmartClient.fixture(u);
		client.fetchNew();

		Map<String, String> url = Map.of(AbstractWebContext.RESOURCE_FILE_NAME, DOWNLOAD_ONCE,
											AbstractWebContext.DOCUMENT_NAME, ActionsFixture.MODULE_NAME + '.' + ActionsFixture.DOCUMENT_NAME,
											AbstractWebContext.CONTEXT_NAME, client.webId);
		assertDownloadRefused(download(client.session, "GET", url, null));

		// HEAD gets the refusal's headers only
		MockHttpServletResponse head = download(client.session, "HEAD", url, null);
		assertTrue(head.getContentType().startsWith("text/html"), head::getContentType);
		assertEquals("", head.getContentAsString());
	}

	@Test
	void theSelfDisablingDownloadIsServedAfterItsPrepareDisabledIt() throws Exception {
		SmartClient client = SmartClient.fixture(u);
		client.fetchNew();

		Map<String, String> url = prepareDownloadOnce(client);

		// Preparing it disabled Download Once, so a check of the view now would refuse it
		assertTrue(((ActionsFixture) client.cachedBean()).isAlreadyDownloaded());
		assertDownloaded(download(client.session, "GET", url, null));
	}

	@Test
	void aHeadThenAGetThenARangedGetAreAllServed() throws Exception {
		SmartClient client = SmartClient.fixture(u);
		client.fetchNew();
		Map<String, String> url = prepareDownloadOnce(client);

		MockHttpServletResponse head = download(client.session, "HEAD", url, null);
		assertEquals(200, head.getStatus());
		assertTrue(head.getContentType().startsWith("text/plain"), head::getContentType);
		assertEquals("attachment; filename=\"actions.txt\"", head.getHeader("Content-Disposition"));
		assertEquals(DOWNLOAD_CONTENT.length(), head.getContentLength());
		assertEquals("", head.getContentAsString());

		assertDownloaded(download(client.session, "GET", url, null));
		// Download Once is not file backed, so the range is ignored and it is served whole
		assertDownloaded(download(client.session, "GET", url, "bytes=0-6"));
	}

	@Test
	void aDownloadGrantServesOnlyTheBindingAndBizIdItWasRecordedWith() throws Exception {
		SmartClient client = SmartClient.fixture(u);
		client.fetchNew();
		Map<String, String> url = prepareDownloadOnce(client);

		// A binding that the prepared URL did not carry; without the grant this one serves the download for no bean
		assertDownloadRefused(download(client.session, "GET", with(url, AbstractWebContext.BINDING_NAME, ActionsFixture.rowsPropertyName), null));
		// An empty binding is not an absent one
		assertDownloadRefused(download(client.session, "GET", with(url, AbstractWebContext.BINDING_NAME, ""), null));
		// A bizId, which the URL carries only for a grid row; without the grant this one is served, as the bizId is ignored
		assertDownloadRefused(download(client.session, "GET", with(url, Bean.DOCUMENT_ID, client.formBizId()), null));

		assertDownloaded(download(client.session, "GET", url, null));
	}

	@Test
	void aRerenderFetchWithZoomOutIsNeverRefused() throws Exception {
		ActionsFixture fixture = persistedFixture(true, false);
		SmartClient client = SmartClient.fixture(u);
		client.fetch(fixture.getBizId());
		client.rerenderWith(ActionsFixture.disableSavePropertyName, Boolean.TRUE);

		// editView.js rerender() fetches the bean again with the ZoomOut action
		JsonNode response = client.request("fetch", "ZoomOut", null, null, null, ActionsFixture.DOCUMENT_NAME, fixture.getBizId());

		assertEquals(0, status(response), response::toString);
	}

	@Test
	void aRefusalLeavesTheConversationAndTheCSRFTokenAsTheyWere() throws Exception {
		SmartClient client = SmartClient.fixture(u);
		client.fetchNew();
		Integer token = client.csrf;
		String sessionId = client.session.getId();
		client.set(ActionsFixture.eventTextPropertyName, "posted with a refused action");

		JsonNode refused = client.update("RunWithoutTrigger");

		assertRefused(refused);
		assertFalse(refused.has("errors"));
		// The posted values were not applied to the cached conversation
		ActionsFixture cached = (ActionsFixture) client.cachedBean();
		assertNull(cached.getEventText());
		assertNull(cached.getOutcome());
		// The token was not replaced, so the client carries on with it
		assertTrue(StateUtil.checkToken(sessionId, token));
		assertRan(client.update("RunFromToolbar"), "Run From Toolbar ran");
	}

	@Test
	void monitoringDashboardSaveStillRunsTheBuiltInSaveThroughItsCustomAction() throws Exception {
		// Its custom actions Save and Refresh have the class Save, which the client sends as the built-in Save
		SmartClient client = new SmartClient(u, MonitoringDashboard.MODULE_NAME, MonitoringDashboard.DOCUMENT_NAME);
		client.fetchNew();

		JsonNode saved = client.update("Save");

		assertEquals(0, status(saved), saved::toString);
		assertEquals(1, count(MonitoringDashboard.MODULE_NAME, MonitoringDashboard.DOCUMENT_NAME));
	}

	@Test
	void aCustomActionMerelyNamedSaveDoesNotTakeOverTheBuiltInSave() throws Exception {
		// No view declares such an action, so add one to DialogsFixture's views for this test only: it is named Save but
		// has another class and no trigger, while <defaults/> offers the built-in Save
		SmartClient client = new SmartClient(u, DialogsFixture.MODULE_NAME, DialogsFixture.DOCUMENT_NAME);
		String uxui = UserAgent.getSelection(client.newRequest()).getUxUi().getName();
		Document document = c.getModule(DialogsFixture.MODULE_NAME).getDocument(c, DialogsFixture.DOCUMENT_NAME);
		ActionImpl namedSave = new ActionImpl();
		namedSave.setName(ImplicitActionName.Save.name());
		namedSave.setResourceName("AskOnce");
		namedSave.setInActionPanel(Boolean.FALSE);
		// The create view falls back to the edit view; mutating it is safe as each test gets a new DefaultRepository
		List<ViewImpl> views = List.of((ViewImpl) document.getView(uxui, c, ViewType.edit.toString()),
										(ViewImpl) document.getView(uxui, c, ViewType.create.toString()));
		try {
			for (ViewImpl view : views) {
				view.putAction(namedSave);
			}
			client.fetchNew();
			JsonNode saved = client.update(ImplicitActionName.Save.name());

			assertEquals(0, status(saved), saved::toString);
			assertEquals(1, count(DialogsFixture.MODULE_NAME, DialogsFixture.DOCUMENT_NAME));
		}
		finally {
			for (ViewImpl view : views) {
				view.getActions().remove(namedSave);
			}
		}
	}

	@Test
	void adminUserNewIsACustomActionCheckedInTheCreateView() throws Exception {
		// Only OK, Save, ZoomOut and Print are built-ins to the servlet, so New is the create view's custom action
		SmartClient client = new SmartClient(u, modules.admin.domain.User.MODULE_NAME, modules.admin.domain.User.DOCUMENT_NAME);
		client.fetchNew();
		client.set(modules.admin.domain.User.searchContactNamePropertyName, "Actions Fixture Contact");
		client.set(modules.admin.domain.User.searchEmailPropertyName, "actions.fixture@skyve.org");

		JsonNode response = client.update("New");

		assertEquals(0, status(response), response::toString);
	}

	@Test
	void aDeleteOfANewUserIsRefusedAsItsCreateViewOffersNoDelete() throws Exception {
		SmartClient client = new SmartClient(u, modules.admin.domain.User.MODULE_NAME, modules.admin.domain.User.DOCUMENT_NAME);
		client.fetchNew();

		assertRefused(client.remove(modules.admin.domain.User.DOCUMENT_NAME));
	}

	@Test
	void aDeleteIsCheckedInTheBeansOwnViewNotTheRequestedSupertypes() throws Exception {
		// admin.User extends UserProxy, whose default view offers Delete; the new user's create view does not
		SmartClient client = new SmartClient(u, modules.admin.domain.User.MODULE_NAME, modules.admin.domain.User.DOCUMENT_NAME);
		client.fetchNew();

		assertRefused(client.remove(UserProxy.DOCUMENT_NAME));
	}

	/**
	 * Asserts that a response is a refusal, with status -51 and the refusal message.
	 */
	private static void assertRefused(JsonNode response) {
		assertEquals(UNAVAILABLE, status(response), response::toString);
		assertEquals(UNAVAILABLE_MESSAGE, response.get("data").asText());
	}

	/**
	 * Asserts that a response is a refusal and that the action did not record its outcome in the conversation.
	 */
	private static void assertRefusedAndNotRun(SmartClient client, JsonNode response) {
		assertRefused(response);
		assertNull(((ActionsFixture) client.cachedBean()).getOutcome());
	}

	/**
	 * Asserts that a response is a success with the action's outcome.
	 */
	private static void assertRan(JsonNode response, String outcome) {
		assertEquals(0, status(response), response::toString);
		assertEquals(outcome, response.get("data").get(ActionsFixture.outcomePropertyName).asText());
	}

	/**
	 * Saves a fixture with a row that shows its link and a row that hides it, outside any request, and commits it.
	 */
	private ActionsFixture persistedFixture(boolean disableDelete, boolean disableSave) {
		ActionsFixture result = ActionsFixture.newInstance();
		result.setDisableDelete(Boolean.valueOf(disableDelete));
		result.setDisableSave(Boolean.valueOf(disableSave));
		ActionsFixtureRow shown = ActionsFixtureRow.newInstance();
		shown.setName("shown");
		shown.setParent(result);
		result.getRows().add(shown);
		ActionsFixtureRow hidden = ActionsFixtureRow.newInstance();
		hidden.setName("hidden");
		hidden.setHideLink(Boolean.TRUE);
		hidden.setParent(result);
		result.getRows().add(hidden);
		result = p.save(result);
		p.commit(false);
		p.begin();
		return result;
	}

	/**
	 * Counts a document's beans in the database with a new persistence, as each request closes the thread's persistence.
	 */
	private int count(String moduleName, String documentName) {
		AbstractPersistence persistence = AbstractPersistence.get();
		persistence.setUser(u);
		persistence.evictAllCached();
		persistence.begin();
		try {
			Number result = persistence.newDocumentQuery(moduleName, documentName)
											.addAggregateProjection(AggregateFunction.Count, Bean.DOCUMENT_ID, "count")
											.scalarResult(Number.class);
			assertNotNull(result);
			return result.intValue();
		}
		finally {
			persistence.commit(true);
		}
	}

	private static ActionsFixtureRow row(ActionsFixture fixture, String name) {
		for (ActionsFixtureRow row : fixture.getRows()) {
			if (name.equals(row.getName())) {
				return row;
			}
		}
		throw new AssertionError("No row " + name);
	}

	private static JsonNode row(JsonNode response, String bizId) {
		for (JsonNode row : response.get("data").get(ActionsFixture.rowsPropertyName)) {
			if (bizId.equals(row.get(Bean.DOCUMENT_ID).asText())) {
				return row;
			}
		}
		throw new AssertionError("No row " + bizId + " in " + response);
	}

	/**
	 * Returns the rendered markup of a row's action link from the fetched form values.
	 */
	private static String rowLink(JsonNode values, String bizId) {
		JsonNode row = row(JSON_MAPPER.createObjectNode().set("data", values), bizId);
		for (Map.Entry<String, JsonNode> field : row.properties()) {
			String value = field.getValue().asText();
			if (value.contains("._view.doAction(")) {
				return value;
			}
		}
		throw new AssertionError("No action link in row " + row);
	}

	private static int status(JsonNode response) {
		return response.get("status").asInt();
	}

	/**
	 * Requests Download Once, as its toolbar button does.
	 *
	 * @return	The parameters of the download URL that the client is sent to.
	 */
	private static Map<String, String> prepareDownloadOnce(SmartClient client) throws Exception {
		JsonNode response = client.update(DOWNLOAD_ONCE);
		assertEquals(0, status(response), response::toString);
		Map<String, String> result = downloadParameters(response.get("data").get("_redirectUrl").asText());
		assertEquals(DOWNLOAD_ONCE, result.get(AbstractWebContext.RESOURCE_FILE_NAME));
		return result;
	}

	/**
	 * Returns the request parameters of a download URL, which {@code WebUtil.getDownloadActionUrl} does not encode.
	 */
	static Map<String, String> downloadParameters(String url) {
		Map<String, String> result = new HashMap<>();
		for (String parameter : url.substring(url.indexOf('?') + 1).split("&")) {
			int equals = parameter.indexOf('=');
			result.put(parameter.substring(0, equals), parameter.substring(equals + 1));
		}
		return result;
	}

	/**
	 * Returns a copy of the download parameters with one parameter replaced or added.
	 */
	private static Map<String, String> with(Map<String, String> parameters, String name, String value) {
		Map<String, String> result = new HashMap<>(parameters);
		result.put(name, value);
		return result;
	}

	/**
	 * Requests a download from the download servlet, as the browser does when sent to the download URL.
	 *
	 * @param session	The user's session.
	 * @param method	GET or HEAD.
	 * @param parameters	The request parameters.
	 * @param range	The Range header, or null for none.
	 */
	static MockHttpServletResponse download(MockHttpSession session, String method, Map<String, String> parameters, String range)
	throws Exception {
		MockHttpServletRequest request = new MockHttpServletRequest(method, "/download");
		request.setSession(session);
		request.addHeader("User-Agent", USER_AGENT);
		for (Map.Entry<String, String> parameter : parameters.entrySet()) {
			request.addParameter(parameter.getKey(), parameter.getValue());
		}
		if (range != null) {
			request.addHeader("Range", range);
		}

		MockHttpServletResponse response = new MockHttpServletResponse();
		WebContainer.setHttpServletRequestResponse(request, response);
		try {
			new DownloadServlet().service(request, response);
		}
		finally {
			WebContainer.clear();
		}
		return response;
	}

	/**
	 * Asserts that Download Once was served whole.
	 */
	static void assertDownloaded(MockHttpServletResponse response) throws Exception {
		assertEquals(200, response.getStatus());
		assertTrue(response.getContentType().startsWith("text/plain"), response::getContentType);
		assertEquals(DOWNLOAD_CONTENT, response.getContentAsString());
	}

	/**
	 * Asserts that a download was refused with the page showing the refusal message.
	 */
	static void assertDownloadRefused(MockHttpServletResponse response) throws Exception {
		assertTrue(response.getContentType().startsWith("text/html"), response::getContentType);
		assertEquals("<html><head/><body><h3>" + UNAVAILABLE_MESSAGE + "</h3></body></html>", response.getContentAsString());
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
		private final String module;
		private final String document;
		private ObjectNode values;
		private String webId;
		private Integer csrf;

		private SmartClient(User user, String module, String document) {
			this.module = module;
			this.document = document;
			session.setAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME, user);
			StateUtil.addSession(user.getId(), session);
		}

		private static SmartClient fixture(User user) {
			return new SmartClient(user, ActionsFixture.MODULE_NAME, ActionsFixture.DOCUMENT_NAME);
		}

		private void fetchNew() throws Exception {
			fetch(null);
		}

		private void fetch(String bizId) throws Exception {
			JsonNode response = request("fetch", null, null, null, null, document, bizId);
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

		/**
		 * Sets a checkbox, whose change handler rerenders the view, so the conversation holds the new value.
		 */
		private void rerenderWith(String binding, Boolean value) throws Exception {
			set(binding, value);
			JsonNode response = update(null, binding, null, document, formBizId());
			assertEquals(0, status(response), response::toString);
		}

		private JsonNode update(String action) throws Exception {
			return update(action, null, null, document, formBizId());
		}

		/**
		 * Sends the form values with an update, as {@code saveInstance} and {@code doAction} do; a row link sends its
		 * grid binding, the row's document and the row's bizId.
		 */
		private JsonNode update(String action, String source, String gridBinding, String documentName, String bizId)
		throws Exception {
			JsonNode result = request("update", action, source, null, gridBinding, documentName, bizId);
			if (status(result) >= 0) {
				values = (ObjectNode) result.get("data");
			}
			return result;
		}

		/**
		 * Requests an action of a row's view while zoomed in on the row, sending the row's values.
		 */
		private JsonNode zoomed(String action, ActionsFixtureRow row) throws Exception {
			ObjectNode formValues = values;
			try {
				values = JSON_MAPPER.createObjectNode();
				values.put(Bean.DOCUMENT_ID, row.getBizId());
				values.put(ActionsFixtureRow.namePropertyName, row.getName());
				String binding = ActionsFixture.rowsPropertyName + "ElementById(" + row.getBizId() + ")";
				return request("update", action, null, binding, null, ActionsFixtureRow.DOCUMENT_NAME, row.getBizId());
			}
			finally {
				values = formValues;
			}
		}

		private JsonNode remove(String documentName) throws Exception {
			return request("remove", null, null, null, null, documentName, formBizId());
		}

		private String formBizId() {
			return values.get(Bean.DOCUMENT_ID).asText();
		}

		/**
		 * Reads the bean from the cached conversation, as the next request would.
		 */
		private Bean cachedBean() {
			AbstractWebContext webContext = StateUtil.getCachedConversation(webId, newRequest());
			assertNotNull(webContext);
			return webContext.getCurrentBean();
		}

		private MockHttpServletRequest newRequest() {
			MockHttpServletRequest result = new MockHttpServletRequest("POST", "/smartedit");
			result.setSession(session);
			result.addHeader("User-Agent", USER_AGENT);
			return result;
		}

		@SuppressWarnings("java:S107") // one parameter per request parameter the client sends
		private JsonNode request(String operation,
									String action,
									String source,
									String formBinding,
									String gridBinding,
									String documentName,
									String bizId)
		throws Exception {
			MockHttpServletRequest request = newRequest();
			request.addParameter("_operationType", operation);
			request.addParameter(AbstractWebContext.MODULE_NAME, module);
			request.addParameter(AbstractWebContext.DOCUMENT_NAME, documentName);
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
			if (formBinding != null) {
				request.addParameter(AbstractWebContext.BINDING_NAME, formBinding);
			}
			if (gridBinding != null) {
				request.addParameter(AbstractWebContext.GRID_BINDING_NAME, gridBinding);
			}
			if (bizId != null) {
				request.addParameter(Bean.DOCUMENT_ID, bizId);
			}
			if (values != null) {
				ObjectNode bean = values.deepCopy();
				bean.remove(AbstractWebContext.CONTEXT_NAME);
				request.addParameter("bean", JSON_MAPPER.writeValueAsString(bean));
			}

			MockHttpServletResponse response = new MockHttpServletResponse();
			WebContainer.setHttpServletRequestResponse(request, response);
			try {
				servlet.service(request, response);
			}
			finally {
				WebContainer.clear();
			}
			JsonNode result = JSON_MAPPER.readTree(response.getContentAsString()).get("response");
			if (status(result) >= 0) {
				csrf = Integer.valueOf(response.getHeader("X-CSRF-TOKEN"));
			}
			return result;
		}
	}
}
