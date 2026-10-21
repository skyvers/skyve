package modules.kitchensink.ActionsFixture;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.lang.reflect.Field;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.primefaces.event.FileUploadEvent;
import org.primefaces.model.file.UploadedFile;
import org.skyve.EXT;
import org.skyve.bizport.BizPortWorkbook;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.UploadException.Problem;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.BizportExportServlet;
import org.skyve.impl.web.faces.views.AbstractUploadView;
import org.skyve.impl.web.faces.views.BizportImportView;
import org.skyve.impl.web.faces.views.ContentUploadView;
import org.skyve.impl.web.service.smartclient.SmartClientWebContext;
import org.skyve.web.WebContext;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.mock.web.MockHttpSession;

import jakarta.faces.application.FacesMessage;
import jakarta.faces.context.ExternalContext;
import jakarta.faces.context.FacesContext;
import modules.kitchensink.domain.ActionsFixture;
import modules.kitchensink.domain.ActionsFixtureRow;
import modules.test.AbstractSkyveTest;

/**
 * Requests the actions fixture's export, upload and import actions through {@link BizportExportServlet},
 * {@link ContentUploadView} and {@link BizportImportView}, as the SmartClient client does once it has saved the form:
 * an action whose toolbar button is enabled runs, and one whose button is disabled, or that the bean's own view does
 * not offer, is refused with the localised refusal message and does not run. An export from a view zoomed in on a
 * fixture row is checked against the row's view. The fixture is always created, so a new admin user stands in for a
 * bean not yet created: its edit view offers Generate Password and its create view does not, so a request for it is
 * refused only when the create view is checked. A mock Faces context stands in for the Faces lifecycle of the upload
 * and import pages.
 */
@SuppressWarnings("java:S1192") // repeated test literals
class ActionsFixtureBizPortAndUploadH2Test extends AbstractSkyveTest {
	private static final String UNAVAILABLE_MESSAGE = "This action is no longer available.";
	private static final String UNAVAILABLE_PAGE = "<html><head/><body><h3>" + UNAVAILABLE_MESSAGE + "</h3></body></html>";
	private static final String USER_AGENT = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/141.0.0.0 Safari/537.36";
	/** An action of the admin user's edit view that its create view does not offer. */
	private static final String EDIT_VIEW_ONLY_ACTION = "GeneratePassword";

	/**
	 * Reaches the protected {@link FacesContext#setCurrentInstance(FacesContext)}.
	 */
	private abstract static class FacesContextBridge extends FacesContext {
		static void setCurrent(FacesContext context) {
			setCurrentInstance(context);
		}
	}

	private final MockHttpSession session = new MockHttpSession();
	private final List<FacesMessage> messages = new ArrayList<>();

	@BeforeEach
	void startSession() {
		session.setAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME, u);
		StateUtil.addSession(u.getId(), session);
	}

	@AfterEach
	void restore() {
		FacesContextBridge.setCurrent(null);
		// The export servlet commits and closes the thread's persistence, so give the base class one to clean up with
		AbstractPersistence.get().setUser(u);
	}

	@Test
	void anExportRunsWhileItsButtonIsEnabled() throws Exception {
		String webId = cacheConversation(persistedFixture());

		MockHttpServletResponse response = exportFixture(webId);

		assertEquals("attachment; filename=\"ActionsExport-export.xls\"", response.getHeader("Content-Disposition"));
		assertTrue(response.getContentAsByteArray().length > 0);
	}

	@Test
	void anExportIsRefusedOnceItsButtonIsDisabled() throws Exception {
		ActionsFixture fixture = ActionsFixture.newInstance();
		fixture.setDisableExport(Boolean.TRUE);
		String webId = cacheConversation(fixture);

		MockHttpServletResponse response = exportFixture(webId);

		assertNull(response.getHeader("Content-Disposition"));
		assertTrue(response.getContentType().startsWith("text/html"), response.getContentType());
		assertEquals(UNAVAILABLE_PAGE, response.getContentAsString(StandardCharsets.UTF_8));
	}

	@Test
	void anExportIsCheckedInTheConversationBeansOwnViewNotTheRequestedDocuments() throws Exception {
		// The requested fixture's view offers an enabled export, but the new user's views offer none
		String webId = cacheConversation(modules.admin.domain.User.newInstance());

		MockHttpServletResponse response = exportFixture(webId);

		assertNull(response.getHeader("Content-Disposition"));
		assertEquals(UNAVAILABLE_PAGE, response.getContentAsString(StandardCharsets.UTF_8));
	}

	@Test
	void anExportIsCheckedInTheCreateViewOfABeanNotYetCreated() throws Exception {
		String webId = cacheConversation(modules.admin.domain.User.newInstance());

		// Were the edit view checked, the check would pass and the request would fail as Generate Password is no export
		MockHttpServletResponse response = export(webId,
													modules.admin.domain.User.MODULE_NAME + '.' + modules.admin.domain.User.DOCUMENT_NAME,
													EDIT_VIEW_ONLY_ACTION,
													null);

		assertEquals(UNAVAILABLE_PAGE, response.getContentAsString(StandardCharsets.UTF_8));
	}

	@Test
	void anExportFromAZoomedInRowIsAllowedByTheRowsView() throws Exception {
		// The fixture's view does not offer Export From Row; the row's view, which the user is zoomed in on, does
		ActionsFixture fixture = persistedFixtureWithARow();
		String webId = cacheConversation(fixture);

		MockHttpServletResponse response = export(webId,
													ActionsFixtureRow.MODULE_NAME + '.' + ActionsFixtureRow.DOCUMENT_NAME,
													"ExportFromRow",
													rowBinding(fixture));

		assertEquals("attachment; filename=\"ExportFromRow-export.xls\"", response.getHeader("Content-Disposition"));
	}

	@Test
	void anExportFromAZoomedInRowIsRefusedWhenTheRowsViewDoesNotOfferIt() throws Exception {
		// The fixture's view offers an enabled export, but the row's view, which the user is zoomed in on, does not
		ActionsFixture fixture = persistedFixtureWithARow();
		String webId = cacheConversation(fixture);

		MockHttpServletResponse response = export(webId,
													ActionsFixture.MODULE_NAME + '.' + ActionsFixture.DOCUMENT_NAME,
													"ActionsExport",
													rowBinding(fixture));

		assertNull(response.getHeader("Content-Disposition"));
		assertEquals(UNAVAILABLE_PAGE, response.getContentAsString(StandardCharsets.UTF_8));
	}

	@Test
	void anUploadRunsWhileItsButtonIsEnabled() throws Exception {
		String webId = cacheConversation(ActionsFixture.newInstance());
		ContentUploadView view = actionUploadView(webId, "ActionsUpload");

		view.handleFileUpload(upload("upload.txt", "uploaded".getBytes(StandardCharsets.UTF_8)));

		assertEquals(List.of("Successful"), messages.stream().map(FacesMessage::getSummary).toList());
		assertEquals("Actions Upload ran", cachedFixture(webId).getOutcome());
	}

	@Test
	void anUploadIsRefusedOnceItsButtonIsDisabled() throws Exception {
		ActionsFixture fixture = persistedFixture();
		fixture.setDisableUpload(Boolean.TRUE);
		String webId = cacheConversation(fixture);
		ContentUploadView view = actionUploadView(webId, "ActionsUpload");

		view.handleFileUpload(upload("upload.txt", "uploaded".getBytes(StandardCharsets.UTF_8)));

		assertEquals(1, messages.size(), messages::toString);
		assertEquals("Failure", messages.get(0).getSummary());
		assertEquals(UNAVAILABLE_MESSAGE, messages.get(0).getDetail());
		assertNull(cachedFixture(webId).getOutcome());
	}

	@Test
	void anUploadIsCheckedInTheCreateViewOfABeanNotYetCreated() throws Exception {
		String webId = cacheConversation(modules.admin.domain.User.newInstance());
		ContentUploadView view = actionUploadView(webId, EDIT_VIEW_ONLY_ACTION);

		// Were the edit view checked, the check would pass and the upload would fail as Generate Password is no upload
		view.handleFileUpload(upload("upload.txt", "uploaded".getBytes(StandardCharsets.UTF_8)));

		assertEquals(1, messages.size(), messages::toString);
		assertEquals(UNAVAILABLE_MESSAGE, messages.get(0).getDetail());
	}

	@Test
	void anImportRunsWhileItsButtonIsEnabled() throws Exception {
		String webId = cacheConversation(persistedFixture());
		BizportImportView view = importView(webId, "ActionsImport");

		view.handleFileUpload(upload("import.xls", workbook()));

		assertEquals(List.of("Actions Import ran"), view.getProblems().stream().map(Problem::getWhat).toList());
		assertEquals(List.of("Successful"), messages.stream().map(FacesMessage::getSummary).toList());
	}

	@Test
	void anImportIsRefusedOnceItsButtonIsDisabled() throws Exception {
		ActionsFixture fixture = ActionsFixture.newInstance();
		fixture.setDisableImport(Boolean.TRUE);
		String webId = cacheConversation(fixture);
		BizportImportView view = importView(webId, "ActionsImport");

		view.handleFileUpload(upload("import.xls", workbook()));

		assertTrue(view.getProblems().isEmpty(), view.getProblems()::toString);
		assertEquals(1, messages.size(), messages::toString);
		assertEquals(FacesMessage.SEVERITY_ERROR, messages.get(0).getSeverity());
		assertEquals("Failure", messages.get(0).getSummary());
		assertEquals(UNAVAILABLE_MESSAGE, messages.get(0).getDetail());
	}

	@Test
	void anImportIsCheckedInTheCreateViewOfABeanNotYetCreated() throws Exception {
		String webId = cacheConversation(modules.admin.domain.User.newInstance());
		BizportImportView view = importView(webId, EDIT_VIEW_ONLY_ACTION);

		// Were the edit view checked, the check would pass and the import would fail as Generate Password is no import
		view.handleFileUpload(upload("import.xls", workbook()));

		assertEquals(1, messages.size(), messages::toString);
		assertEquals(UNAVAILABLE_MESSAGE, messages.get(0).getDetail());
	}

	/**
	 * Saves a fixture outside any request, and commits it.
	 */
	private ActionsFixture persistedFixture() {
		ActionsFixture result = p.save(ActionsFixture.newInstance());
		p.commit(false);
		p.begin();
		return result;
	}

	/**
	 * Saves a fixture with one row outside any request, and commits it.
	 */
	private ActionsFixture persistedFixtureWithARow() {
		ActionsFixture result = ActionsFixture.newInstance();
		ActionsFixtureRow row = ActionsFixtureRow.newInstance();
		row.setName("row");
		row.setParent(result);
		result.getRows().add(row);
		result = p.save(result);
		p.commit(false);
		p.begin();
		return result;
	}

	/**
	 * Returns the form binding of a view zoomed in on the fixture's row, as the client sends it.
	 */
	private static String rowBinding(ActionsFixture fixture) {
		return ActionsFixture.rowsPropertyName + "ElementById(" + fixture.getRows().get(0).getBizId() + ")";
	}

	/**
	 * Caches a conversation on the bean, as the client's save before each of these requests leaves it.
	 *
	 * @return	The conversation's web id, which the client sends with the request.
	 */
	private String cacheConversation(Bean bean) {
		SmartClientWebContext webContext = new SmartClientWebContext(UUID.randomUUID().toString(), request());
		webContext.setConversation(AbstractPersistence.get());
		webContext.setCurrentBean(bean);
		webContext.cacheConversationAndCycleTransaction();
		return webContext.getWebId();
	}

	/**
	 * Reads the fixture from the cached conversation, as the next request would.
	 */
	private ActionsFixture cachedFixture(String webId) {
		AbstractWebContext webContext = StateUtil.getCachedConversation(webId, request());
		assertNotNull(webContext);
		return (ActionsFixture) webContext.getCurrentBean();
	}

	/**
	 * Requests the fixture's export as its BizExport button does in a view that is not zoomed in.
	 */
	private MockHttpServletResponse exportFixture(String webId) throws Exception {
		return export(webId, ActionsFixture.MODULE_NAME + '.' + ActionsFixture.DOCUMENT_NAME, "ActionsExport", null);
	}

	/**
	 * Requests an export as the SmartClient BizExport button does, with the view's form binding when it is zoomed in.
	 */
	private MockHttpServletResponse export(String webId, String document, String action, String formBinding) throws Exception {
		MockHttpServletRequest request = request();
		request.setMethod("GET");
		request.setRequestURI("/bizexport.xls");
		request.addParameter(AbstractWebContext.CONTEXT_NAME, webId);
		request.addParameter(AbstractWebContext.DOCUMENT_NAME, document);
		request.addParameter(AbstractWebContext.RESOURCE_FILE_NAME, action);
		if (formBinding != null) {
			request.addParameter(AbstractWebContext.BINDING_NAME, formBinding);
		}
		MockHttpServletResponse result = new MockHttpServletResponse();
		new BizportExportServlet().doGet(request, result);
		return result;
	}

	/**
	 * Starts a mock Faces request on the upload page for an action, and returns the page bean once constructed.
	 */
	private ContentUploadView actionUploadView(String webId, String action) throws Exception {
		startFacesRequest();
		ContentUploadView result = new ContentUploadView();
		setField(AbstractUploadView.class, result, "context", webId);
		setField(ContentUploadView.class, result, "uploadKind", "action");
		setField(ContentUploadView.class, result, "action", action);
		result.postConstruct();
		return result;
	}

	/**
	 * Starts a mock Faces request on the import page for an action, and returns the page bean once constructed.
	 */
	private BizportImportView importView(String webId, String action) throws Exception {
		startFacesRequest();
		BizportImportView result = new BizportImportView();
		setField(AbstractUploadView.class, result, "context", webId);
		setField(BizportImportView.class, result, "action", action);
		result.postConstruct();
		return result;
	}

	/**
	 * Starts a mock Faces request on an upload page, whose messages are collected in {@link #messages}.
	 */
	private void startFacesRequest() {
		ExternalContext external = mock(ExternalContext.class);
		when(external.getRequest()).thenReturn(request());
		when(external.getSession(false)).thenReturn(session);
		FacesContext fc = mock(FacesContext.class);
		when(fc.getExternalContext()).thenReturn(external);
		doAnswer(invocation -> Boolean.valueOf(messages.add(invocation.getArgument(1)))).when(fc).addMessage(isNull(), any(FacesMessage.class));
		FacesContextBridge.setCurrent(fc);
	}

	private MockHttpServletRequest request() {
		MockHttpServletRequest result = new MockHttpServletRequest("POST", "/upload.xhtml");
		result.setSession(session);
		result.addHeader("User-Agent", USER_AGENT);
		return result;
	}

	@SuppressWarnings("resource") // a ByteArrayInputStream holds no resource to close
	private static FileUploadEvent upload(String fileName, byte[] content) throws Exception {
		UploadedFile file = mock(UploadedFile.class);
		when(file.getFileName()).thenReturn(fileName);
		when(Long.valueOf(file.getSize())).thenReturn(Long.valueOf(content.length));
		when(file.getInputStream()).thenReturn(new ByteArrayInputStream(content));
		FileUploadEvent result = mock(FileUploadEvent.class);
		when(result.getFile()).thenReturn(file);
		return result;
	}

	/**
	 * Returns an empty BizPort workbook in the xls format, as the fixture's export produces.
	 */
	private static byte[] workbook() throws Exception {
		BizPortWorkbook workbook = EXT.newBizPortWorkbook(false);
		workbook.materialise();
		ByteArrayOutputStream result = new ByteArrayOutputStream();
		workbook.write(result);
		return result.toByteArray();
	}

	/**
	 * Sets a request parameter field, as JSF injects it into the request-scoped page bean.
	 */
	private static void setField(Class<?> owner, Object target, String name, Object value) throws Exception {
		Field field = owner.getDeclaredField(name);
		field.setAccessible(true);
		field.set(target, value);
	}
}
