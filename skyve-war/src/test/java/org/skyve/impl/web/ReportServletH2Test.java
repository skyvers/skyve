package org.skyve.impl.web;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.security.Principal;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.report.ReportFormat;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import org.skyve.web.WebContext;

import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.WriteListener;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import net.sf.jasperreports.jakarta.servlets.BaseHttpServlet;
import net.sf.jasperreports.engine.JasperPrint;
import modules.test.AbstractSkyveTest;
import modules.test.domain.AllAttributesPersistent;

@SuppressWarnings({"static-method", "boxing", "resource"})
class ReportServletH2Test extends AbstractSkyveTest {
	@AfterEach
	void restorePersistenceUser() {
		AbstractPersistence.get().setUser(u);
	}

	@Test
	void doExportStreamsCsvForDefaultDocumentQuery() throws Exception {
		saveReportBean("report-export");

		CapturedOutput output = new CapturedOutput();
		HttpServletRequest request = exportRequest(null, "text");
		HttpServletResponse response = exportResponse(output);

		invokeDoExport(request, response);

		assertTrue(output.bytes().length > 0);
	}

	@Test
	void doExportAppliesSimpleCriteriaForDocumentQuery() throws Exception {
		saveReportBean("report-simple");

		CapturedOutput output = new CapturedOutput();
		HttpServletRequest request = exportRequest("\"criteria\":{\"text\":\"report-simple\"},", "text");
		HttpServletResponse response = exportResponse(output);

		invokeDoExport(request, response);

		assertTrue(output.bytes().length > 0);
	}

	@Test
	void doExportAppliesAdvancedCriteriaForDocumentQuery() throws Exception {
		saveReportBean("report-advanced");

		CapturedOutput output = new CapturedOutput();
		HttpServletRequest request = exportRequest(
				"\"criteria\":{\"operator\":\"and\",\"criteria\":[{\"fieldName\":\"text\",\"operator\":\"iContains\",\"value\":\"report-advanced\"}]},",
				"text");
		HttpServletResponse response = exportResponse(output);

		invokeDoExport(request, response);

		assertTrue(output.bytes().length > 0);
	}

	@Test
	void doExportWritesErrorForNonExistentColumn() throws Exception {
		saveReportBean("report-error");

		CapturedOutput output = new CapturedOutput();
		HttpServletRequest request = exportRequest(null, "missingColumn");
		HttpServletResponse response = exportResponse(output);

		invokeDoExport(request, response);

		assertTrue(output.bytes().length > 0);
		verify(response).setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
	}

	@Test
	void doExportWritesHtmlMessageWhenValuesParameterMissing() throws Exception {
		CapturedOutput output = new CapturedOutput();
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = exportResponse(output);
		when(request.getParameter("values")).thenReturn(null);

		invokeDoExport(request, response);

		String body = output.text();
		assertTrue(body.contains("Missing Report Parameters"), body);
		assertTrue(body.contains("There are no report parameters"), body);
	}

	@Test
	void doGetDispatchesAuthenticatedExportRequest() throws Exception {
		CapturedOutput output = new CapturedOutput();
		HttpServletRequest request = authenticatedExportRequest();
		HttpServletResponse response = exportResponse(output);

		new ReportServlet().doGet(request, response);

		String body = output.text();
		assertTrue(body.contains("Missing Report Parameters"), body);
		assertTrue(body.contains("There are no report parameters"), body);
	}

	@Test
	void doPostDelegatesToAuthenticatedExportRequest() throws Exception {
		CapturedOutput output = new CapturedOutput();
		HttpServletRequest request = authenticatedExportRequest();
		HttpServletResponse response = exportResponse(output);

		new ReportServlet().doPost(request, response);

		String body = output.text();
		assertTrue(body.contains("Missing Report Parameters"), body);
		assertTrue(body.contains("There are no report parameters"), body);
	}

	@Test
	void pumpOutReportFormatStreamsAllSupportedFormats() throws Exception {
		byte[] bytes = "abc".getBytes(java.nio.charset.StandardCharsets.UTF_8);
		for (ReportFormat format : ReportFormat.values()) {
			CapturedOutput output = new CapturedOutput();
			HttpSession session = mock(HttpSession.class);
			HttpServletResponse response = exportResponse(output);

			invokePumpOutReportFormat(bytes, new JasperPrint(), format, session, response);

			assertArrayEquals(bytes, output.bytes(), format.name());
			verify(response).setContentLength(bytes.length);
		}
	}

	@Test
	void pumpOutReportFormatStoresHtmlPrintInSessionAndSetsInlineDisposition() throws Exception {
		byte[] bytes = "abc".getBytes(java.nio.charset.StandardCharsets.UTF_8);
		CapturedOutput output = new CapturedOutput();
		HttpSession session = mock(HttpSession.class);
		HttpServletResponse response = exportResponse(output);
		JasperPrint jasperPrint = new JasperPrint();

		invokePumpOutReportFormat(bytes, jasperPrint, ReportFormat.html, session, response);

		verify(response).setHeader("Content-Disposition", "inline; filename=\"all-attributes.html\"");
		verify(session).setAttribute(BaseHttpServlet.DEFAULT_JASPER_PRINT_SESSION_ATTRIBUTE, jasperPrint);
		assertArrayEquals(bytes, output.bytes());
	}

	@Test
	void redirectToErrorPageSendsEncodedErrorUrl() throws Exception {
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.encodeRedirectURL(anyString())).thenAnswer(invocation -> invocation.getArgument(0));

		invokeRedirectToErrorPage(response, "REF-1");

		verify(response).sendRedirect(anyString());
	}

	@Test
	void redirectToErrorPageWritesFallbackErrorWhenRedirectFails() throws Exception {
		CapturedOutput output = new CapturedOutput();
		HttpServletResponse response = exportResponse(output);
		when(response.encodeRedirectURL(anyString())).thenAnswer(invocation -> invocation.getArgument(0));
		when(response.isCommitted()).thenReturn(false);
		org.mockito.Mockito.doThrow(new IOException("redirect failed")).when(response).sendRedirect(anyString());

		invokeRedirectToErrorPage(response, "REF-2");

		assertTrue(output.text().contains("REF-2"), output.text());
		verify(response).reset();
		verify(response).setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
	}

	@Test
	void writeReportErrorDoesNothingWhenResponseAlreadyCommitted() throws Exception {
		CapturedOutput output = new CapturedOutput();
		HttpServletResponse response = exportResponse(output);
		when(response.isCommitted()).thenReturn(true);

		invokeWriteReportError(response, output.stream(), "REF-3");

		assertEquals(0, output.bytes().length);
		verify(response, never()).reset();
	}

	@Test
	void getParametersReturnsOnlySanitisedReportParameters() throws Exception {
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getParameterMap()).thenReturn(Map.of(
				AbstractWebContext.MODULE_NAME, new String[] {"test"},
				AbstractWebContext.DOCUMENT_NAME, new String[] {"AllAttributesPersistent"},
				"custom", new String[] {"<b>value</b>"},
				"blank", new String[] {""}));
		when(request.getParameter(AbstractWebContext.MODULE_NAME)).thenReturn("test");
		when(request.getParameter(AbstractWebContext.DOCUMENT_NAME)).thenReturn("AllAttributesPersistent");
		when(request.getParameter("custom")).thenReturn("<b>value</b>");
		when(request.getParameter("blank")).thenReturn("");

		Map<String, Object> parameters = invokeGetParameters(request);

		assertFalse(parameters.containsKey(AbstractWebContext.MODULE_NAME));
		assertFalse(parameters.containsKey(AbstractWebContext.DOCUMENT_NAME));
		assertEquals("value", parameters.get("custom"));
		assertTrue(parameters.containsKey("blank"));
		assertNull(parameters.get("blank"));
	}

	private AllAttributesPersistent saveReportBean(String prefix) throws Exception {
		AllAttributesPersistent bean = new DataBuilder().fixture(FixtureType.crud)
														.build(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
		bean.setText(prefix + "-" + System.nanoTime());
		return p.save(bean);
	}

	private static HttpServletRequest exportRequest(String criteriaJson, String columnName) {
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpSession session = mock(HttpSession.class);
		when(request.getSession()).thenReturn(session);
		when(request.getParameter("values")).thenReturn("""
				{"ds":"test_AllAttributesPersistent","reportFormat":"csv","style":"tabular","width":595,"height":842,
				"isPaginated":false,"isPretty":false,"showSummary":false,"top":10,"bottom":10,"left":10,"right":10,
				%s
				"columns":[{"name":"%s","line":0,"title":"Text","width":100,"align":"left"}],
				"fileNameNoSuffix":"all-attributes"}
				""".formatted((criteriaJson == null) ? "" : criteriaJson, columnName));
		return request;
	}

	private static HttpServletResponse exportResponse(CapturedOutput output) throws IOException {
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.getOutputStream()).thenReturn(output.stream());
		return response;
	}

	private HttpServletRequest authenticatedExportRequest() {
		HttpSession session = mock(HttpSession.class);
		when(session.getId()).thenReturn("report-session-" + System.nanoTime());
		when(session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME)).thenReturn(u);
		StateUtil.addSession(u.getId(), session);

		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getSession()).thenReturn(session);
		when(request.getSession(false)).thenReturn(session);
		when(request.getUserPrincipal()).thenReturn((Principal) null);
		when(request.getLocale()).thenReturn(java.util.Locale.ENGLISH);
		when(request.getServletPath()).thenReturn(ReportServlet.EXPORT_PATH);
		when(request.getParameter("values")).thenReturn(null);
		return request;
	}

	private static void invokeDoExport(HttpServletRequest request, HttpServletResponse response) throws Exception {
		Method doExport = ReportServlet.class.getDeclaredMethod("doExport", HttpServletRequest.class, HttpServletResponse.class);
		doExport.setAccessible(true);
		try {
			doExport.invoke(null, request, response);
		}
		catch (InvocationTargetException e) {
			Throwable cause = e.getCause();
			if (cause instanceof Exception exception) {
				throw exception;
			}
			throw e;
		}
	}

	private static void invokePumpOutReportFormat(byte[] bytes,
													JasperPrint jasperPrint,
													ReportFormat format,
													HttpSession session,
													HttpServletResponse response)
	throws Exception {
		Method pumpOutReportFormat = ReportServlet.class.getDeclaredMethod("pumpOutReportFormat",
				byte[].class,
				JasperPrint.class,
				ReportFormat.class,
				String.class,
				HttpSession.class,
				HttpServletResponse.class);
		pumpOutReportFormat.setAccessible(true);
		try {
			pumpOutReportFormat.invoke(null, bytes, jasperPrint, format, "all-attributes", session, response);
		}
		catch (InvocationTargetException e) {
			Throwable cause = e.getCause();
			if (cause instanceof Exception exception) {
				throw exception;
			}
			throw e;
		}
	}

	private static void invokeRedirectToErrorPage(HttpServletResponse response, String reference) throws Exception {
		Method redirectToErrorPage = ReportServlet.class.getDeclaredMethod("redirectToErrorPage", HttpServletResponse.class, String.class);
		redirectToErrorPage.setAccessible(true);
		try {
			redirectToErrorPage.invoke(null, response, reference);
		}
		catch (InvocationTargetException e) {
			Throwable cause = e.getCause();
			if (cause instanceof Exception exception) {
				throw exception;
			}
			throw e;
		}
	}

	private static void invokeWriteReportError(HttpServletResponse response, java.io.OutputStream out, String reference) throws Exception {
		Method writeReportError = ReportServlet.class.getDeclaredMethod("writeReportError",
				HttpServletResponse.class,
				java.io.OutputStream.class,
				String.class);
		writeReportError.setAccessible(true);
		try {
			writeReportError.invoke(null, response, out, reference);
		}
		catch (InvocationTargetException e) {
			Throwable cause = e.getCause();
			if (cause instanceof Exception exception) {
				throw exception;
			}
			throw e;
		}
	}

	@SuppressWarnings("unchecked")
	private static Map<String, Object> invokeGetParameters(HttpServletRequest request) throws Exception {
		Method getParameters = ReportServlet.class.getDeclaredMethod("getParameters", HttpServletRequest.class);
		getParameters.setAccessible(true);
		try {
			return (Map<String, Object>) getParameters.invoke(null, request);
		}
		catch (InvocationTargetException e) {
			Throwable cause = e.getCause();
			if (cause instanceof Exception exception) {
				throw exception;
			}
			throw e;
		}
	}

	private static final class CapturedOutput {
		private final ByteArrayOutputStream bytes = new ByteArrayOutputStream();

		private ServletOutputStream stream() {
			return new ServletOutputStream() {
				@Override
				public void write(int b) throws IOException {
					bytes.write(b);
				}

				@Override
				public boolean isReady() {
					return true;
				}

				@Override
				public void setWriteListener(WriteListener writeListener) {
					// Not needed for these synchronous tests.
				}
			};
		}

		private byte[] bytes() {
			return bytes.toByteArray();
		}

		private String text() {
			return bytes.toString(java.nio.charset.StandardCharsets.UTF_8);
		}
	}
}
