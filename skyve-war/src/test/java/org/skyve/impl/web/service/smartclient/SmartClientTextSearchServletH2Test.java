package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.security.Principal;
import java.util.Collections;
import java.util.List;
import java.util.Locale;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.content.SearchResult;
import org.skyve.content.SearchResults;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.content.NoOpContentManager;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.web.WebContext;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import modules.test.AbstractSkyveTest;

/**
 * Searches content through a content manager whose results cannot all be resolved to a bean.
 */
class SmartClientTextSearchServletH2Test extends AbstractSkyveTest {
	private static final String UNRESOLVABLE_BIZ_ID = "unresolvable-search-result";

	@AfterEach
	void restore() {
		AbstractContentManager.IMPLEMENTATION_CLASS = NoOpContentManager.class;
		// The servlet commits and closes the thread's persistence, so give the base class one to clean up with
		AbstractPersistence.get().setUser(u);
	}

	@Test
	@SuppressWarnings("resource") // the servlet closes the response writer
	void aSearchResultWithoutAModuleIsSkipped() throws Exception {
		AbstractContentManager.IMPLEMENTATION_CLASS = UnresolvableResultContentManager.class;
		HttpSession session = mock(HttpSession.class);
		when(session.getId()).thenReturn("smart-client-text-search-session-" + System.nanoTime());
		when(session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME)).thenReturn(u);
		StateUtil.addSession(u.getId(), session);
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getParameterNames()).thenReturn(Collections.enumeration(List.of("query")));
		when(request.getParameter("query")).thenReturn("anything");
		when(request.getSession()).thenReturn(session);
		when(request.getSession(false)).thenReturn(session);
		when(request.getUserPrincipal()).thenReturn((Principal) null);
		when(request.getLocale()).thenReturn(Locale.ENGLISH);
		HttpServletResponse response = mock(HttpServletResponse.class);
		StringWriter sink = new StringWriter();
		when(response.getWriter()).thenReturn(new PrintWriter(sink, true));

		new SmartClientTextSearchServlet().doGet(request, response);

		String body = sink.toString();
		assertTrue(body.contains("\"status\":0,\"startRow\":0,\"endRow\":0,\"totalRows\":0"), body);
		assertFalse(body.contains(UNRESOLVABLE_BIZ_ID), body);
	}

	/**
	 * Finds one result that has a document and a bizId but no module.
	 */
	public static class UnresolvableResultContentManager extends NoOpContentManager {
		@Override
		public @Nonnull SearchResults google(@Nullable String search, int maxResults) {
			SearchResult result = new SearchResult();
			result.setDocumentName("AllAttributesPersistent");
			result.setBizId(UNRESOLVABLE_BIZ_ID);
			SearchResults results = new SearchResults();
			results.getResults().add(result);
			return results;
		}
	}
}
