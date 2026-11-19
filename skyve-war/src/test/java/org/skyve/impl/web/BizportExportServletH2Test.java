package org.skyve.impl.web;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.nio.charset.StandardCharsets;
import java.util.UUID;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.web.service.smartclient.SmartClientWebContext;
import org.skyve.web.WebContext;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.mock.web.MockHttpSession;

import modules.test.AbstractSkyveTest;
import modules.test.domain.AllAttributesPersistent;

/**
 * Requests an export from a cached conversation as the SmartClient BizExport button does.
 */
class BizportExportServletH2Test extends AbstractSkyveTest {
	private final MockHttpSession session = new MockHttpSession();

	@BeforeEach
	void startSession() {
		session.setAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME, u);
		StateUtil.addSession(u.getId(), session);
	}

	@Test
	void anExportFromACachedConversationWithoutPersistenceWritesTheGenericError() throws Exception {
		String webId = cacheConversationWithoutPersistence();
		MockHttpServletRequest request = request();
		request.addParameter(AbstractWebContext.CONTEXT_NAME, webId);
		request.addParameter(AbstractWebContext.DOCUMENT_NAME, AllAttributesPersistent.MODULE_NAME + '.' + AllAttributesPersistent.DOCUMENT_NAME);
		request.addParameter(AbstractWebContext.RESOURCE_FILE_NAME, "Export");
		MockHttpServletResponse response = new MockHttpServletResponse();

		new BizportExportServlet().doGet(request, response);

		assertNull(response.getHeader("Content-Disposition"));
		assertEquals("<html><head/><body><h3>An error occured whilst processing your report.</body></html>",
						response.getContentAsString(StandardCharsets.UTF_8));
	}

	/**
	 * Caches a conversation on a new bean that has no persistence, which a cached conversation always has.
	 *
	 * @return	The conversation's web id, which the client sends with the request.
	 */
	private String cacheConversationWithoutPersistence() {
		SmartClientWebContext webContext = new SmartClientWebContext(UUID.randomUUID().toString(), request());
		webContext.setCurrentBean(AllAttributesPersistent.newInstance());
		StateUtil.commitAndCacheConversation(webContext);
		return webContext.getWebId();
	}

	private MockHttpServletRequest request() {
		MockHttpServletRequest result = new MockHttpServletRequest("GET", "/bizexport.xls");
		result.setSession(session);
		return result;
	}
}
