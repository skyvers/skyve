package modules.kitchensink.ActionsFixture;

import static modules.kitchensink.ActionsFixture.ActionsFixtureSmartClientH2Test.DOWNLOAD_ONCE;
import static modules.kitchensink.ActionsFixture.ActionsFixtureSmartClientH2Test.USER_AGENT;
import static modules.kitchensink.ActionsFixture.ActionsFixtureSmartClientH2Test.assertDownloadRefused;
import static modules.kitchensink.ActionsFixture.ActionsFixtureSmartClientH2Test.assertDownloaded;
import static modules.kitchensink.ActionsFixture.ActionsFixtureSmartClientH2Test.download;
import static modules.kitchensink.ActionsFixture.ActionsFixtureSmartClientH2Test.downloadParameters;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.primefaces.context.PrimeRequestContext;
import org.skyve.domain.Bean;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.impl.web.DownloadServlet;
import org.skyve.impl.web.WebContainer;
import org.skyve.impl.web.faces.FacesUtil;
import org.skyve.impl.web.faces.FacesWebContext;
import org.skyve.impl.web.faces.actions.ExecuteDownloadAction;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.web.WebContext;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.mock.web.MockHttpSession;

import jakarta.faces.application.FacesMessage;
import jakarta.faces.component.UIViewRoot;
import jakarta.faces.context.ExternalContext;
import jakarta.faces.context.FacesContext;
import jakarta.faces.context.PartialViewContext;
import modules.kitchensink.domain.ActionsFixture;
import modules.test.AbstractSkyveTest;

/**
 * Prepares the actions fixture's Download Once through the PrimeFaces {@link ExecuteDownloadAction}, with a mock Faces
 * context standing in for the Faces lifecycle of one AJAX request, and then requests it from {@link DownloadServlet},
 * which serves only the download the conversation prepared. The SmartClient prepare step is tested in
 * {@link ActionsFixtureSmartClientH2Test}, whose download helpers this test shares.
 */
class ActionsFixturePrimeFacesH2Test extends AbstractSkyveTest {
	private static final String ASSIGN = "window.location.assign('";

	/**
	 * Reaches the protected {@link FacesContext#setCurrentInstance(FacesContext)}.
	 */
	private abstract static class FacesContextBridge extends FacesContext {
		static void setCurrent(FacesContext context) {
			setCurrentInstance(context);
		}
	}

	private final MockHttpSession session = new MockHttpSession();
	private boolean accessControl;

	/**
	 * Starts the user's session, and switches off access control, as the test user has no menus to grant access to
	 * the fixture's view.
	 */
	@BeforeEach
	void startSessionWithoutAccessControl() {
		session.setAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME, u);
		StateUtil.addSession(u.getId(), session);
		accessControl = UtilImpl.ACCESS_CONTROL;
		UtilImpl.ACCESS_CONTROL = false;
	}

	@AfterEach
	void restore() {
		UtilImpl.ACCESS_CONTROL = accessControl;
		FacesContextBridge.setCurrent(null);
		WebContainer.clear();
		// The download request commits and closes the thread's persistence, so give the base class one to clean up with
		AbstractPersistence.get().setUser(u);
	}

	@Test
	void aPrimeFacesPrepareRecordsTheGrantThatServesItsDownload() throws Exception {
		List<FacesMessage> messages = new ArrayList<>();
		FacesContext fc = startFacesRequest(messages);
		AbstractWebContext webContext = new FacesWebContext();
		webContext.setConversation(AbstractPersistence.get());
		webContext.setCurrentBean(ActionsFixture.newInstance());
		FacesView view = new FacesView();
		view.setWebContext(webContext);

		new ExecuteDownloadAction(view, DOWNLOAD_ONCE, null, null).execute();

		assertEquals(List.of(), messages);
		assertTrue(((ActionsFixture) webContext.getCurrentBean()).isAlreadyDownloaded());
		List<String> scripts = PrimeRequestContext.getCurrentInstance(fc).getScriptsToExecute();
		assertEquals(1, scripts.size(), scripts::toString);
		String script = scripts.get(0);
		assertTrue(script.startsWith(ASSIGN) && script.endsWith("')"), script);
		Map<String, String> url = downloadParameters(script.substring(ASSIGN.length(), script.length() - 2));

		// The phase listener caches the conversation once the response is rendered
		StateUtil.commitAndCacheConversation(webContext);
		assertDownloaded(download(session, "GET", url, null));
		Map<String, String> withBizId = new HashMap<>(url);
		withBizId.put(Bean.DOCUMENT_ID, webContext.getCurrentBean().getBizId());
		assertDownloadRefused(download(session, "GET", withBizId, null));
	}

	/**
	 * Installs a mock Faces context standing in for the Faces lifecycle of one AJAX request in the user's session.
	 *
	 * @param messages	Collects the messages the request adds.
	 * @return	The Faces context.
	 */
	private FacesContext startFacesRequest(List<FacesMessage> messages) {
		MockHttpServletRequest request = new MockHttpServletRequest("POST", "/kitchensink/ActionsFixture.xhtml");
		request.setSession(session);
		request.addHeader("User-Agent", USER_AGENT);
		WebContainer.setHttpServletRequestResponse(request, new MockHttpServletResponse());

		PartialViewContext partial = mock(PartialViewContext.class);
		when(partial.getRenderIds()).thenReturn(new ArrayList<>());
		when(Boolean.valueOf(partial.isAjaxRequest())).thenReturn(Boolean.TRUE);
		ExternalContext external = mock(ExternalContext.class);
		when(external.getRequest()).thenReturn(request);
		UIViewRoot viewRoot = mock(UIViewRoot.class);
		Map<String, Object> viewRootAttributes = new HashMap<>();
		viewRootAttributes.put(FacesUtil.MANAGED_BEAN_NAME_KEY, "skyve");
		when(viewRoot.getAttributes()).thenReturn(viewRootAttributes);

		FacesContext result = mock(FacesContext.class);
		when(result.getAttributes()).thenReturn(new HashMap<>());
		when(result.getPartialViewContext()).thenReturn(partial);
		when(result.getExternalContext()).thenReturn(external);
		when(result.getViewRoot()).thenReturn(viewRoot);
		when(result.getMessages()).thenAnswer(invocation -> messages.iterator());
		doAnswer(invocation -> Boolean.valueOf(messages.add(invocation.getArgument(1)))).when(result).addMessage(any(), any(FacesMessage.class));
		doAnswer(invocation -> Boolean.valueOf(messages.add(invocation.getArgument(1)))).when(result).addMessage(isNull(), any(FacesMessage.class));
		FacesContextBridge.setCurrent(result);
		return result;
	}
}
