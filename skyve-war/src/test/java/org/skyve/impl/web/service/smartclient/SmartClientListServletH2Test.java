package org.skyve.impl.web.service.smartclient;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.Method;
import java.security.Principal;
import java.util.Collections;
import java.util.Enumeration;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;

import org.skyve.EXT;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.impl.web.SortParameterImpl;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.snapshot.SmartClientFilterOperator;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import org.skyve.web.SortParameter;
import org.skyve.web.WebContext;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import modules.test.AbstractSkyveTest;
import modules.test.domain.AllAttributesPersistent;

class SmartClientListServletH2Test extends AbstractSkyveTest {
	private static final String DATASOURCE = "test_AllAttributesPersistent";

	@AfterEach
	void restorePersistenceUser() {
		AbstractPersistence.get().setUser(u);
	}

	@Test
	void fetchDefaultQueryRowsWithSimpleCriteria() throws Exception {
		AllAttributesPersistent bean = saveUncommittedBean("list-fetch");
		SortedMap<String, Object> parameters = new TreeMap<>();
		parameters.put("text", "list-fetch");

		String body = invokeFetch(0,
				25,
				SmartClientFilterOperator.substring,
				null,
				null,
				null,
				false,
				parameters);

		assertSuccessfulFetch(body);
		assertTrue(body.contains(bean.getBizId()));
	}

	@Test
	void fetchDefaultQueryRowsWithAdvancedCriteria() throws Exception {
		AllAttributesPersistent bean = saveUncommittedBean("list-advanced");
		String criteria = "{\"fieldName\":\"text\",\"operator\":\"iContains\",\"value\":\"list-advanced\"}";

		String body = invokeFetch(0,
				10,
				null,
				new String[] { criteria },
				null,
				null,
				false,
				new TreeMap<>(Map.of("operator", "and")));

		assertSuccessfulFetch(body);
		assertTrue(body.contains(bean.getBizId()));
	}

	@Test
	void fetchDefaultQueryRowsWithSortAndPagedWindow() throws Exception {
		AllAttributesPersistent first = saveUncommittedBean("list-sort-a");
		AllAttributesPersistent second = saveUncommittedBean("list-sort-b");
		SortParameterImpl sort = new SortParameterImpl();
		sort.setBy("text");
		sort.setDirection(SortDirection.descending);

		String body = invokeFetch(0,
				1,
				SmartClientFilterOperator.substring,
				null,
				new SortParameter[] { sort },
				null,
				false,
				new TreeMap<>(Map.of("text", "list-sort")));

		assertSuccessfulFetch(body);
		assertTrue(body.contains("\"endRow\":1"), body);
		assertTrue(body.contains(second.getBizId()), body);
		assertTrue(!body.contains(first.getBizId()), body);
	}

	@Test
	void fetchResetsStartRowWhenRequestedPageIsBeyondFilteredRows() throws Exception {
		AllAttributesPersistent bean = saveUncommittedBean("list-page-reset");
		SortedMap<String, Object> parameters = new TreeMap<>();
		parameters.put("text", "list-page-reset");

		String body = invokeFetch(99,
				100,
				SmartClientFilterOperator.substring,
				null,
				null,
				null,
				false,
				parameters);

		assertSuccessfulFetch(body);
		assertTrue(body.contains("\"startRow\":0"), body);
		assertTrue(body.contains("\"endRow\":1"), body);
		assertTrue(body.contains("\"totalRows\":1"), body);
		assertTrue(!body.contains(bean.getBizId()), body);
	}

	@Test
	void doGetReturnsEmptyResponseWhenOperationTypeMissing() throws Exception {
		CapturedResponse response = service(newRequest(u).param("_dataSource", DATASOURCE)).doGet();

		assertTrue(response.body().contains("{}"));
	}

	@Test
	void doPostDelegatesToEmptyResponseWhenOperationTypeMissing() throws Exception {
		CapturedResponse response = service(newRequest(u).param("_dataSource", DATASOURCE)).doPost();

		assertTrue(response.body().contains("{}"));
	}

	@Test
	void doGetReturnsSmartClientErrorForMissingDatasource() throws Exception {
		CapturedResponse response = service(newRequest(u).param("_operationType", Operation.fetch.toString())).doGet();

		assertTrue(response.body().contains("\"status\":-1"));
		assertTrue(response.body().contains("\"totalRows\":0"));
	}

	@Test
	void doPostDelegatesToSmartClientErrorForMissingDatasource() throws Exception {
		CapturedResponse response = service(newRequest(u).param("_operationType", Operation.fetch.toString())).doPost();

		assertTrue(response.body().contains("\"status\":-1"));
		assertTrue(response.body().contains("\"totalRows\":0"));
	}

	private AllAttributesPersistent saveUncommittedBean(String textPrefix) {
		AllAttributesPersistent bean = new DataBuilder().fixture(FixtureType.crud)
				.build(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
		bean.setText(textPrefix + '-' + System.nanoTime());
		bean = p.save(bean);
		return bean;
	}

	private String invokeFetch(int startRow,
			int endRow,
			SmartClientFilterOperator operator,
			String[] criteria,
			SortParameter[] sortParameters,
			AggregateFunction summaryType,
			boolean includeExtraSummaryRow,
			SortedMap<String, Object> parameters)
			throws Exception {
		ListModel<Bean> model = EXT.newListModel(m.getDocumentDefaultQuery(c, AllAttributesPersistent.DOCUMENT_NAME));
		StringWriter sink = new StringWriter();
		try (PrintWriter writer = new PrintWriter(sink, true)) {
			Method fetch = SmartClientListServlet.class.getDeclaredMethod("fetch",
					org.skyve.metadata.module.Module.class,
					org.skyve.metadata.model.document.Document.class,
					int.class,
					int.class,
					SmartClientFilterOperator.class,
					String[].class,
					SortParameter[].class,
					AggregateFunction.class,
					boolean.class,
					String.class,
					SortedMap.class,
					AbstractPersistence.class,
					PrintWriter.class,
					ListModel.class);
			fetch.setAccessible(true);
			fetch.invoke(null,
					m,
					aapd,
					Integer.valueOf(startRow),
					Integer.valueOf(endRow),
					operator,
					criteria,
					sortParameters,
					summaryType,
					Boolean.valueOf(includeExtraSummaryRow),
					null,
					parameters,
					p,
					writer,
					model);
		}
		return sink.toString();
	}

	private static void assertSuccessfulFetch(String body) {
		assertTrue(body.contains("\"status\":0"), body);
		assertTrue(body.contains("\"data\":"), body);
		assertTrue(body.contains("\"totalRows\":"), body);
	}

	private static ServletHarness service(RequestBuilder request) throws IOException {
		CapturedResponse response = new CapturedResponse();
		return new ServletHarness(request.build(), response);
	}

	private static RequestBuilder newRequest(User user) {
		return new RequestBuilder(user);
	}

	private static final class ServletHarness {
		private final TestableSmartClientListServlet servlet = new TestableSmartClientListServlet();
		private final HttpServletRequest request;
		private final CapturedResponse response;

		private ServletHarness(HttpServletRequest request, CapturedResponse response) {
			this.request = request;
			this.response = response;
		}

		private CapturedResponse doGet() throws ServletException, IOException {
			servlet.doGet(request, response.response);
			return response;
		}

		private CapturedResponse doPost() throws ServletException, IOException {
			servlet.doPost(request, response.response);
			return response;
		}
	}

	private static final class TestableSmartClientListServlet extends SmartClientListServlet {
		private static final long serialVersionUID = 1L;

		@Override
		public void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
			super.doGet(request, response);
		}

		@Override
		public void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
			super.doPost(request, response);
		}
	}

	private static final class RequestBuilder {
		private final Map<String, String[]> parameters = new LinkedHashMap<>();
		private final HttpSession session = mock(HttpSession.class);

		private RequestBuilder(User user) {
			when(session.getId()).thenReturn("smart-client-list-session-" + System.nanoTime());
			when(session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME)).thenReturn(user);
			StateUtil.addSession(user.getId(), session);
		}

		private RequestBuilder param(String name, String value) {
			parameters.put(name, new String[] { value });
			return this;
		}

		private HttpServletRequest build() {
			HttpServletRequest request = mock(HttpServletRequest.class);
			when(request.getParameterNames()).thenReturn(parameterNames());
			when(request.getParameterMap()).thenReturn(Collections.unmodifiableMap(parameters));
			when(request.getSession()).thenReturn(session);
			when(request.getSession(false)).thenReturn(session);
			when(request.getUserPrincipal()).thenReturn((Principal) null);
			when(request.getLocale()).thenReturn(java.util.Locale.ENGLISH);
			when(request.getHeader("User-Agent")).thenReturn("Mozilla/5.0");
			for (Map.Entry<String, String[]> entry : parameters.entrySet()) {
				when(request.getParameter(entry.getKey())).thenReturn(entry.getValue()[0]);
				when(request.getParameterValues(entry.getKey())).thenReturn(entry.getValue());
			}
			return request;
		}

		private Enumeration<String> parameterNames() {
			return Collections.enumeration(List.copyOf(parameters.keySet()));
		}
	}

	private static final class CapturedResponse {
		private final StringWriter sink = new StringWriter();
		private final HttpServletResponse response = mock(HttpServletResponse.class);

		@SuppressWarnings("resource")
		private CapturedResponse() throws IOException {
			when(response.getWriter()).thenReturn(new PrintWriter(sink, true));
		}

		private String body() {
			return sink.toString();
		}
	}
}
