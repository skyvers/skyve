package org.skyve.impl.web;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.ByteArrayOutputStream;
import java.lang.reflect.Method;
import java.util.Collections;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.repository.ProvidedRepository;

import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;

/**
 * Additional unit tests for {@link ReportServlet} targeting uncovered paths.
 */
@SuppressWarnings("all")
class ReportServletAdditionalTest {

	private String savedServerUrl;
	private String savedSkyveContext;
	private ProvidedRepository savedRepository;
	private String savedCustomer;

	@BeforeEach
	void saveState() {
		savedServerUrl = UtilImpl.SERVER_URL;
		savedSkyveContext = UtilImpl.SKYVE_CONTEXT;
		savedRepository = ProvidedRepositoryFactory.get();
		savedCustomer = UtilImpl.CUSTOMER;
	}

	@AfterEach
	void restoreState() {
		UtilImpl.SERVER_URL = savedServerUrl;
		UtilImpl.SKYVE_CONTEXT = savedSkyveContext;
		ProvidedRepositoryFactory.set(savedRepository);
		UtilImpl.CUSTOMER = savedCustomer;
	}

	// ===== doPost delegates to doGet =====

	@Test
	void doPostDelegatesToDoGet() throws Exception {
		ReportServlet servlet = spy(new ReportServlet());
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpServletResponse response = mock(HttpServletResponse.class);
		doNothing().when(servlet).doGet(request, response);

		servlet.doPost(request, response);

		verify(servlet).doGet(request, response);
	}

	// ===== getParameters =====

	@Test
	void getParametersExcludesContextName() throws Exception {
		Method getParamsMethod = ReportServlet.class.getDeclaredMethod("getParameters", HttpServletRequest.class);
		getParamsMethod.setAccessible(true);

		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getParameterMap()).thenReturn(Map.of(
			AbstractWebContext.CONTEXT_NAME, new String[]{"ctx1"},
			"customParam", new String[]{"customValue"}
		));
		when(request.getParameter(AbstractWebContext.CONTEXT_NAME)).thenReturn("ctx1");
		when(request.getParameter("customParam")).thenReturn("customValue");

		@SuppressWarnings("unchecked")
		Map<String, Object> params = (Map<String, Object>) getParamsMethod.invoke(null, request);

		assertEquals(1, params.size());
		assertTrue(params.containsKey("customParam"));
	}

	@Test
	void getParametersExcludesAllControlParams() throws Exception {
		Method getParamsMethod = ReportServlet.class.getDeclaredMethod("getParameters", HttpServletRequest.class);
		getParamsMethod.setAccessible(true);

		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getParameterMap()).thenReturn(Map.of(
			AbstractWebContext.CONTEXT_NAME, new String[]{"c"},
			AbstractWebContext.ID_NAME, new String[]{"id1"},
			AbstractWebContext.REPORT_FORMAT, new String[]{"pdf"},
			AbstractWebContext.MODULE_NAME, new String[]{"admin"},
			AbstractWebContext.DOCUMENT_NAME, new String[]{"Contact"},
			AbstractWebContext.REPORT_NAME, new String[]{"rpt1"},
			"userParam", new String[]{"userValue"}
		));
		when(request.getParameter("userParam")).thenReturn("userValue");

		@SuppressWarnings("unchecked")
		Map<String, Object> params = (Map<String, Object>) getParamsMethod.invoke(null, request);

		assertEquals(1, params.size());
		assertTrue(params.containsKey("userParam"));
	}

	// ===== writeReportError paths =====

	@Test
	void writeReportErrorResponseWhenCommitted() throws Exception {
		Method writeError = ReportServlet.class.getDeclaredMethod("writeReportError",
			HttpServletResponse.class, java.io.OutputStream.class, String.class);
		writeError.setAccessible(true);

		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.isCommitted()).thenReturn(true);

		// Should log and return without writing
		writeError.invoke(null, response, mock(java.io.OutputStream.class), "REF-001");

		verify(response, never()).reset();
	}

	@Test
	void writeReportErrorResponseWhenNotCommitted() throws Exception {
		Method writeError = ReportServlet.class.getDeclaredMethod("writeReportError",
			HttpServletResponse.class, java.io.OutputStream.class, String.class);
		writeError.setAccessible(true);

		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.isCommitted()).thenReturn(false);

		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		writeError.invoke(null, response, baos, "REF-002");

		verify(response).reset();
		verify(response).setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);

		String output = baos.toString("UTF-8");
		assertTrue(output.contains("html"));
		assertTrue(output.contains("REF-002"));
	}

	// ===== redirectToErrorPage =====

	@Test
	void redirectToErrorPageSendsRedirect() throws Exception {
		Method redirect = ReportServlet.class.getDeclaredMethod("redirectToErrorPage",
			HttpServletResponse.class, String.class);
		redirect.setAccessible(true);

		UtilImpl.SERVER_URL = "http://localhost:8080";
		UtilImpl.SKYVE_CONTEXT = "/app";

		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.encodeRedirectURL(anyString())).thenAnswer(inv -> inv.getArgument(0));

		redirect.invoke(null, response, "REF-ERR");

		verify(response).sendRedirect(anyString());
	}

	// ===== pumpOutReportFormat =====

	@SuppressWarnings("deprecation")
	@Test
	void pumpOutReportFormatForHtmlSetsSessionAttributeAndContentType() throws Exception {
		Method pumpOut = ReportServlet.class.getDeclaredMethod("pumpOutReportFormat",
			byte[].class, net.sf.jasperreports.engine.JasperPrint.class, org.skyve.report.ReportFormat.class,
			String.class, HttpSession.class, HttpServletResponse.class);
		pumpOut.setAccessible(true);

		HttpServletResponse response = mock(HttpServletResponse.class);
		ServletOutputStream sos = mock(ServletOutputStream.class);
		when(response.getOutputStream()).thenReturn(sos);

		HttpSession session = mock(HttpSession.class);
		net.sf.jasperreports.engine.JasperPrint jasperPrint = mock(net.sf.jasperreports.engine.JasperPrint.class);

		pumpOut.invoke(null, new byte[]{1, 2, 3}, jasperPrint,
			org.skyve.report.ReportFormat.html, "TestReport", session, response);

		verify(response).setContentType("text/html");
		// HTML format stores jasperPrint in session
		verify(session).setAttribute(anyString(), any());
	}

	@Test
	void pumpOutReportFormatForPdfSetsAttachmentDisposition() throws Exception {
		Method pumpOut = ReportServlet.class.getDeclaredMethod("pumpOutReportFormat",
			byte[].class, net.sf.jasperreports.engine.JasperPrint.class, org.skyve.report.ReportFormat.class,
			String.class, HttpSession.class, HttpServletResponse.class);
		pumpOut.setAccessible(true);

		HttpServletResponse response = mock(HttpServletResponse.class);
		ServletOutputStream sos = mock(ServletOutputStream.class);
		when(response.getOutputStream()).thenReturn(sos);

		HttpSession session = mock(HttpSession.class);

		pumpOut.invoke(null, new byte[]{1, 2, 3}, null,
			org.skyve.report.ReportFormat.pdf, "TestReport", session, response);

		verify(response).setContentType("application/pdf");
		verify(response).setHeader("Content-Disposition", "attachment; filename=\"TestReport.pdf\"");
	}

	@Test
	void pumpOutReportFormatForCsvSetsContentType() throws Exception {
		Method pumpOut = ReportServlet.class.getDeclaredMethod("pumpOutReportFormat",
			byte[].class, net.sf.jasperreports.engine.JasperPrint.class, org.skyve.report.ReportFormat.class,
			String.class, HttpSession.class, HttpServletResponse.class);
		pumpOut.setAccessible(true);

		HttpServletResponse response = mock(HttpServletResponse.class);
		ServletOutputStream sos = mock(ServletOutputStream.class);
		when(response.getOutputStream()).thenReturn(sos);

		HttpSession session = mock(HttpSession.class);

		pumpOut.invoke(null, new byte[0], null,
			org.skyve.report.ReportFormat.csv, "data", session, response);

		verify(response).setContentType("text/csv");
	}

	@Test
	void pumpOutReportFormatForTxtSetsPlainText() throws Exception {
		Method pumpOut = ReportServlet.class.getDeclaredMethod("pumpOutReportFormat",
			byte[].class, net.sf.jasperreports.engine.JasperPrint.class, org.skyve.report.ReportFormat.class,
			String.class, HttpSession.class, HttpServletResponse.class);
		pumpOut.setAccessible(true);

		HttpServletResponse response = mock(HttpServletResponse.class);
		ServletOutputStream sos = mock(ServletOutputStream.class);
		when(response.getOutputStream()).thenReturn(sos);

		HttpSession session = mock(HttpSession.class);

		pumpOut.invoke(null, new byte[0], null,
			org.skyve.report.ReportFormat.txt, "data", session, response);

		verify(response).setContentType("text/plain");
	}

	@Test
	void pumpOutReportFormatForXlsSetsExcelType() throws Exception {
		Method pumpOut = ReportServlet.class.getDeclaredMethod("pumpOutReportFormat",
			byte[].class, net.sf.jasperreports.engine.JasperPrint.class, org.skyve.report.ReportFormat.class,
			String.class, HttpSession.class, HttpServletResponse.class);
		pumpOut.setAccessible(true);

		HttpServletResponse response = mock(HttpServletResponse.class);
		ServletOutputStream sos = mock(ServletOutputStream.class);
		when(response.getOutputStream()).thenReturn(sos);

		HttpSession session = mock(HttpSession.class);

		pumpOut.invoke(null, new byte[0], null,
			org.skyve.report.ReportFormat.xls, "data", session, response);

		verify(response).setContentType("application/vnd.ms-excel");
	}

	@Test
	void pumpOutReportFormatForXlsxSetsContentType() throws Exception {
		Method pumpOut = ReportServlet.class.getDeclaredMethod("pumpOutReportFormat",
			byte[].class, net.sf.jasperreports.engine.JasperPrint.class, org.skyve.report.ReportFormat.class,
			String.class, HttpSession.class, HttpServletResponse.class);
		pumpOut.setAccessible(true);

		HttpServletResponse response = mock(HttpServletResponse.class);
		ServletOutputStream sos = mock(ServletOutputStream.class);
		when(response.getOutputStream()).thenReturn(sos);

		HttpSession session = mock(HttpSession.class);

		pumpOut.invoke(null, new byte[0], null,
			org.skyve.report.ReportFormat.xlsx, "data", session, response);

		verify(response).setContentType("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet");
	}

	@Test
	void pumpOutReportFormatSetsCacheHeaders() throws Exception {
		Method pumpOut = ReportServlet.class.getDeclaredMethod("pumpOutReportFormat",
			byte[].class, net.sf.jasperreports.engine.JasperPrint.class, org.skyve.report.ReportFormat.class,
			String.class, HttpSession.class, HttpServletResponse.class);
		pumpOut.setAccessible(true);

		HttpServletResponse response = mock(HttpServletResponse.class);
		ServletOutputStream sos = mock(ServletOutputStream.class);
		when(response.getOutputStream()).thenReturn(sos);

		HttpSession session = mock(HttpSession.class);

		pumpOut.invoke(null, new byte[]{1, 2, 3}, null,
			org.skyve.report.ReportFormat.pdf, "TestReport", session, response);

		verify(response).setHeader("Cache-Control", "cache");
		verify(response).setHeader("Pragma", "cache");
		verify(response).setHeader("Accept-Ranges", "bytes");
		verify(response).setContentLength(3);
	}
}
