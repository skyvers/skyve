package org.skyve.impl.web;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.util.LinkedHashMap;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.report.jasperreports.ReportDesignParameters;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.user.User;
import org.skyve.metadata.user.UserAccess;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.report.ReportFormat;
import org.skyve.web.UserAgentType;

import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.WriteListener;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import net.sf.jasperreports.engine.JasperPrint;
import net.sf.jasperreports.jakarta.servlets.BaseHttpServlet;

@SuppressWarnings("static-method")
class ReportServletTest {

	@Test
	void getParametersExcludesReservedAndIncludesCustom() throws Exception {
		HttpServletRequest request = mock(HttpServletRequest.class);
		Map<String, String[]> parameterMap = new LinkedHashMap<>();
		parameterMap.put(AbstractWebContext.CONTEXT_NAME, new String[] {"ctx"});
		parameterMap.put(AbstractWebContext.ID_NAME, new String[] {"id-1"});
		parameterMap.put(AbstractWebContext.MODULE_NAME, new String[] {"admin"});
		parameterMap.put(AbstractWebContext.DOCUMENT_NAME, new String[] {"User"});
		parameterMap.put(AbstractWebContext.REPORT_FORMAT, new String[] {"pdf"});
		parameterMap.put(AbstractWebContext.REPORT_NAME, new String[] {"Report"});
		parameterMap.put("customFilter", new String[] {"alpha"});
		parameterMap.put("customEmpty", new String[] {"   "});

		when(request.getParameterMap()).thenReturn(parameterMap);
		when(request.getParameter("customFilter")).thenReturn("alpha");
		when(request.getParameter("customEmpty")).thenReturn("   ");

		Map<String, Object> params = invokeGetParameters(request);

		assertEquals(2, params.size());
		assertEquals("alpha", params.get("customFilter"));
		assertEquals(null, params.get("customEmpty"));
	}

	@Test
	@SuppressWarnings("resource")
	void pumpOutReportFormatSetsCsvHeadersAndWritesBytes() throws Exception {
		byte[] bytes = "a,b,c".getBytes(StandardCharsets.UTF_8);
		HttpServletResponse response = mock(HttpServletResponse.class);
		try (CapturingServletOutputStream out = new CapturingServletOutputStream()) {
			when(response.getOutputStream()).thenReturn(out);

			invokePumpOutReportFormat(bytes,
					mock(JasperPrint.class),
					ReportFormat.csv,
					"export-file",
					mock(HttpSession.class),
					response);

			verify(response).setContentType("text/csv");
			verify(response).setHeader("Content-Disposition", "attachment; filename=\"export-file.csv\"");
			verify(response).setContentLength(bytes.length);
			verify(response, never()).setHeader("Accept-Ranges", "bytes");
			verify(response).addDateHeader(eq("Expires"), anyLong());
			assertEquals("a,b,c", out.asString());
		}
	}

	@Test
	@SuppressWarnings("resource")
	void pumpOutReportFormatStoresJasperPrintForHtml() throws Exception {
		byte[] bytes = "<html/>".getBytes(StandardCharsets.UTF_8);
		HttpServletResponse response = mock(HttpServletResponse.class);
		HttpSession session = mock(HttpSession.class);
		JasperPrint jasperPrint = mock(JasperPrint.class);
		try (CapturingServletOutputStream out = new CapturingServletOutputStream()) {
			when(response.getOutputStream()).thenReturn(out);

			invokePumpOutReportFormat(bytes, jasperPrint, ReportFormat.html, "report", session, response);

			verify(response).setContentType("text/html");
			verify(response).setHeader("Content-Disposition", "inline; filename=\"report.html\"");
			verify(session).setAttribute(BaseHttpServlet.DEFAULT_JASPER_PRINT_SESSION_ATTRIBUTE, jasperPrint);
			assertFalse(out.asString().isEmpty());
		}
	}

	@Test
	@SuppressWarnings("resource")
	void pumpOutReportFormatCoversAllAttachmentFormats() throws Exception {
		Map<ReportFormat, String> contentTypes = new LinkedHashMap<>();
		contentTypes.put(ReportFormat.pdf, "application/pdf");
		contentTypes.put(ReportFormat.xls, "application/vnd.ms-excel");
		contentTypes.put(ReportFormat.rtf, "application/rtf");
		contentTypes.put(ReportFormat.odt, "application/vnd.oasis.opendocument.text");
		contentTypes.put(ReportFormat.ods, "application/vnd.oasis.opendocument.spreadsheet");
		contentTypes.put(ReportFormat.docx, "application/vnd.openxmlformats-officedocument.wordprocessingml.document");
		contentTypes.put(ReportFormat.xlsx, "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet");
		contentTypes.put(ReportFormat.pptx, "application/vnd.openxmlformats-officedocument.presentationml.presentation");
		contentTypes.put(ReportFormat.xml, "application/xml");
		contentTypes.put(ReportFormat.csv, "text/csv");

		for (Map.Entry<ReportFormat, String> entry : contentTypes.entrySet()) {
			byte[] bytes = "row".getBytes(StandardCharsets.UTF_8);
			HttpServletResponse response = mock(HttpServletResponse.class);
			HttpSession session = mock(HttpSession.class);
			try (CapturingServletOutputStream out = new CapturingServletOutputStream()) {
				when(response.getOutputStream()).thenReturn(out);
				invokePumpOutReportFormat(bytes,
						mock(JasperPrint.class),
						entry.getKey(),
						"sample",
						session,
						response);

				verify(response).setContentType(entry.getValue());
				verify(response).setHeader("Content-Disposition", "attachment; filename=\"sample." + entry.getKey().name() + "\"");
				verify(response).setHeader("Cache-Control", "cache");
				verify(response).setHeader("Pragma", "cache");
				verify(response, never()).setHeader("Accept-Ranges", "bytes");
				verify(response).addDateHeader(eq("Expires"), anyLong());
			}
		}
	}

	@Test
	@SuppressWarnings("resource")
	void pumpOutReportFormatForTxtHasNoContentDisposition() throws Exception {
		byte[] bytes = "plain".getBytes(StandardCharsets.UTF_8);
		HttpServletResponse response = mock(HttpServletResponse.class);
		try (CapturingServletOutputStream out = new CapturingServletOutputStream()) {
			when(response.getOutputStream()).thenReturn(out);
			invokePumpOutReportFormat(bytes,
					mock(JasperPrint.class),
					ReportFormat.txt,
					"ignored",
					mock(HttpSession.class),
					response);

			verify(response).setContentType("text/plain");
			verify(response, never()).setHeader(eq("Content-Disposition"), org.mockito.ArgumentMatchers.anyString());
		}
	}

	@Test
	void writeReportErrorWritesHtmlWhenResponseNotCommitted() throws Exception {
		HttpServletResponse response = mock(HttpServletResponse.class);
		doReturn(Boolean.FALSE).when(response).isCommitted();
		ByteArrayOutputStream out = new ByteArrayOutputStream();

		invokeWriteReportError(response, out, "ERR-123");

		verify(response).reset();
		verify(response).setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
		verify(response).setContentType("text/html");
		String html = out.toString(StandardCharsets.UTF_8);
		assertTrue(html.contains("<html>"));
		assertTrue(html.contains("ERR-123"));
	}

	@Test
	void writeReportErrorDoesNothingWhenCommitted() throws Exception {
		HttpServletResponse response = mock(HttpServletResponse.class);
		doReturn(Boolean.TRUE).when(response).isCommitted();
		ByteArrayOutputStream out = new ByteArrayOutputStream();

		invokeWriteReportError(response, out, "ERR-456");

		verify(response, never()).reset();
		verify(response, never()).setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
		assertEquals("", out.toString(StandardCharsets.UTF_8));
	}

	@Test
	@SuppressWarnings("resource")
	void writeReportErrorSwallowsIoExceptionFromOutputStreamWrite() throws Exception {
		HttpServletResponse response = mock(HttpServletResponse.class);
		doReturn(Boolean.FALSE).when(response).isCommitted();

		invokeWriteReportError(response, new ThrowingOutputStream(), "ERR-789");

		verify(response).reset();
		verify(response).setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
		verify(response).setContentType("text/html");
	}

	@Test
	@SuppressWarnings("resource")
	void writeReportErrorWrapperSwallowsIoExceptionFromGetOutputStream() throws Exception {
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.getOutputStream()).thenThrow(new IOException("boom"));

		invokeWriteReportError(response, "ERR-999");

		verify(response).getOutputStream();
	}

	private static Map<String, Object> invokeGetParameters(HttpServletRequest request) throws Exception {
		Method method = ReportServlet.class.getDeclaredMethod("getParameters", HttpServletRequest.class);
		method.setAccessible(true);
		@SuppressWarnings("unchecked")
		Map<String, Object> result = (Map<String, Object>) method.invoke(null, request);
		return result;
	}

	@Test
	@SuppressWarnings("resource")
	void doExportReportsMissingParametersWhenValuesAreJsonNull() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class, CALLS_REAL_METHODS);
		persistence.setUser(mock(User.class));
		persistence.setForThread();
		try (CapturingServletOutputStream out = new CapturingServletOutputStream()) {
			HttpServletRequest request = mock(HttpServletRequest.class);
			when(request.getParameter("values")).thenReturn("null");
			HttpServletResponse response = mock(HttpServletResponse.class);
			when(response.getOutputStream()).thenReturn(out);

			Method method = ReportServlet.class.getDeclaredMethod("doExport", HttpServletRequest.class, HttpServletResponse.class);
			method.setAccessible(true);
			invokeVoid(method, request, response);

			verify(response).setContentType(MimeType.html.toString());
			assertTrue(out.asString().contains("There are no report parameters in this request"), out.asString());
		}
		finally {
			clearThreadPersistence();
		}
	}

	@Test
	@SuppressWarnings({"resource", "boxing"})
	void doExportOfAModelWithoutADrivingDocumentWritesTheGenericError() throws Exception {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		ListModel<Bean> model = mock(ListModel.class);
		when(user.getCustomer()).thenReturn(customer);
		when(user.canAccess(any(UserAccess.class), anyString())).thenReturn(true);
		when(customer.getModule("mod")).thenReturn(module);
		when(module.getDocument(customer, "Doc")).thenReturn(document);
		when(document.getListModel(customer, "Model", true)).thenReturn(model); // getDrivingDocument() returns null
		AbstractPersistence persistence = mock(AbstractPersistence.class, CALLS_REAL_METHODS);
		persistence.setUser(user);
		persistence.setForThread();
		try (CapturingServletOutputStream out = new CapturingServletOutputStream()) {
			HttpServletRequest request = mock(HttpServletRequest.class);
			when(request.getParameter("values")).thenReturn("{\"ds\":\"mod_Doc__Model\",\"reportFormat\":\"csv\"}");
			when(request.getAttribute(UserAgent.class.getName() + ".selection")).thenReturn(
					new RequestUxUiSelection(UserAgentType.desktop, false, UxUi.newSmartClient(UxUi.DESKTOP_NAME, "Cerulean", "omega")));
			HttpServletResponse response = mock(HttpServletResponse.class);
			when(response.getOutputStream()).thenReturn(out);

			Method method = ReportServlet.class.getDeclaredMethod("doExport", HttpServletRequest.class, HttpServletResponse.class);
			method.setAccessible(true);
			invokeVoid(method, request, response);

			verify(model).getDrivingDocument();
			verify(user, never()).canReadDocument(any());
			verify(response).setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
			assertTrue(out.asString().startsWith("<html><head/><body><h3>"), out.asString());
		}
		finally {
			clearThreadPersistence();
		}
	}

	@SuppressWarnings("unchecked")
	private static void clearThreadPersistence() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		((ThreadLocal<AbstractPersistence>) field.get(null)).remove();
	}

	private static void invokePumpOutReportFormat(byte[] bytes,
												JasperPrint jasperPrint,
												ReportFormat format,
												String fileNameNoSuffix,
												HttpSession session,
												HttpServletResponse response) throws Exception {
		Method method = ReportServlet.class.getDeclaredMethod("pumpOutReportFormat",
				byte[].class,
				JasperPrint.class,
				ReportFormat.class,
				String.class,
				HttpSession.class,
				HttpServletResponse.class);
		method.setAccessible(true);
		invokeVoid(method, bytes, jasperPrint, format, fileNameNoSuffix, session, response);
	}

	private static void invokeWriteReportError(HttpServletResponse response, OutputStream out, String reference) throws Exception {
		Method method = ReportServlet.class.getDeclaredMethod("writeReportError", HttpServletResponse.class, OutputStream.class, String.class);
		method.setAccessible(true);
		invokeVoid(method, response, out, reference);
	}

	private static void invokeWriteReportError(HttpServletResponse response, String reference) throws Exception {
		Method method = ReportServlet.class.getDeclaredMethod("writeReportError", HttpServletResponse.class, String.class);
		method.setAccessible(true);
		invokeVoid(method, response, reference);
	}

	private static void invokeVoid(Method method, Object... args) throws Exception {
		try {
			method.invoke(null, args);
		}
		catch (InvocationTargetException e) {
			Throwable cause = e.getCause();
			if (cause instanceof Exception exception) {
				throw exception;
			}
			throw e;
		}
	}

	private static final class CapturingServletOutputStream extends ServletOutputStream {
		private final ByteArrayOutputStream delegate = new ByteArrayOutputStream();

		@Override
		public boolean isReady() {
			return true;
		}

		@Override
		public void setWriteListener(WriteListener writeListener) {
			// no-op for tests
		}

		@Override
		public void write(int b) throws IOException {
			delegate.write(b);
		}

		private String asString() {
			return delegate.toString(StandardCharsets.UTF_8);
		}
	}

	private static final class ThrowingOutputStream extends OutputStream {
		@Override
		public void write(int b) throws IOException {
			throw new IOException("forced failure");
		}
	}

	@Test
	@SuppressWarnings("boxing")
	void applyMarginsSetsEachMarginFromItsOwnValue() {
		Map<String, Object> values = new LinkedHashMap<>();
		values.put("top", 11);
		values.put("bottom", 12);
		values.put("left", 13);
		values.put("right", 14);
		ReportDesignParameters designParams = new ReportDesignParameters();

		ReportServlet.applyMargins(designParams, values);

		assertEquals(11, designParams.getTopMargin());
		assertEquals(12, designParams.getBottomMargin());
		assertEquals(13, designParams.getLeftMargin());
		assertEquals(14, designParams.getRightMargin());
	}
}
