package org.skyve.impl.web;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.controller.Download;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.metadata.controller.WebFileInputStream;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.web.BackgroundTask;
import org.skyve.web.WebContext;

import jakarta.servlet.ServletException;
import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.WriteListener;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;

@SuppressWarnings({"static-method", "resource"})
class DownloadServletTest {
	@Test
	void doGetRunsSecurityInterceptorsWriterAndCachesConversation() throws Exception {
		Download download = new Download("result.txt", "download body", MimeType.plain);
		DownloadAction<Bean> action = actionReturning(download);
		Lifecycle lifecycle = lifecycle(action, false, true);
		TestableDownloadServlet servlet = new TestableDownloadServlet(lifecycle.webContext);
		HttpServletRequest request = request(lifecycle.user, "mod.Doc", "DownloadResult", "bytes=2-4", "if-range-value");
		HttpServletResponse response = mock(HttpServletResponse.class);

		servlet.doGet(request, response);

		assertSame(download, servlet.writtenDownload);
		assertEquals("bytes=2-4", servlet.writtenRangeHeader);
		assertEquals("if-range-value", servlet.writtenIfRangeHeader);
		assertFalse(servlet.writtenHeadOnly);
		assertSame(lifecycle.webContext, servlet.cachedWebContext);
		verify(lifecycle.persistence).begin();
		verify(lifecycle.persistence).setUser(lifecycle.user);
		verify(lifecycle.persistence).commit(true);
		verify(lifecycle.persistence, never()).rollback();
		verify(lifecycle.customer).interceptBeforeDownloadAction(lifecycle.document, "DownloadResult", lifecycle.bean, lifecycle.webContext);
		verify(lifecycle.customer).interceptAfterDownloadAction(lifecycle.document, "DownloadResult", lifecycle.bean, download, lifecycle.webContext);
	}

	@Test
	void deniedRangeDoesNotInvokeWriterOrAction() throws Exception {
		DownloadAction<Bean> action = actionReturning(new Download("result.txt", "download body", MimeType.plain));
		Lifecycle lifecycle = lifecycle(action, false, false);
		TestableDownloadServlet servlet = new TestableDownloadServlet(lifecycle.webContext);
		CapturingServletOutputStream output = new CapturingServletOutputStream();
		HttpServletRequest request = request(lifecycle.user, "mod.Doc", "DownloadResult", "bytes=0-3");
		HttpServletResponse response = response(output);

		servlet.doGet(request, response);

		assertFalse(servlet.writeInvoked);
		assertTrue(output.asString().contains("An error occured whilst processing your report."));
		verify(action, never()).download(any(), any());
		verify(response, never()).setHeader("Accept-Ranges", "bytes");
	}

	@Test
	void streamBackedDownloadIsProcessedAndClosedWhenWriterFails() throws Exception {
		TrackingInputStream raw = new TrackingInputStream("streamed".getBytes(StandardCharsets.UTF_8));
		WebFileInputStream stream = new WebFileInputStream(raw);
		Download download = new Download("stream.txt", stream, MimeType.plain);
		Lifecycle lifecycle = lifecycle(actionReturning(download), false, true);
		TestableDownloadServlet servlet = new TestableDownloadServlet(lifecycle.webContext);
		servlet.writerFailure = new IOException("writer failed");
		CapturingServletOutputStream output = new CapturingServletOutputStream();

		servlet.doGet(request(lifecycle.user, "mod.Doc", "DownloadResult", null), response(output));

		assertTrue(raw.closed);
		assertTrue(output.asString().contains("An error occured whilst processing your report."));
		assertFalse(servlet.cacheInvoked);
		verify(lifecycle.persistence).rollback();
		verify(lifecycle.persistence).commit(true);
	}

	@Test
	void doHeadUsesSameLifecycleWithoutBodyWriting() throws Exception {
		Download download = new Download("result.txt", "download body", MimeType.plain);
		Lifecycle lifecycle = lifecycle(actionReturning(download), false, true);
		TestableDownloadServlet servlet = new TestableDownloadServlet(lifecycle.webContext);

		servlet.doHead(request(lifecycle.user, "mod.Doc", "DownloadResult", null), mock(HttpServletResponse.class));

		assertSame(download, servlet.writtenDownload);
		assertTrue(servlet.writtenHeadOnly);
		assertSame(lifecycle.webContext, servlet.cachedWebContext);
	}

	@Test
	void vetoedDownloadCachesConversationWithZeroLength() throws Exception {
		Lifecycle lifecycle = lifecycle(actionReturning(new Download("result.txt", "download body", MimeType.plain)), true, true);
		TestableDownloadServlet servlet = new TestableDownloadServlet(lifecycle.webContext);
		HttpServletResponse response = mock(HttpServletResponse.class);

		servlet.doGet(request(lifecycle.user, "mod.Doc", "DownloadResult", null), response);

		assertFalse(servlet.writeInvoked);
		assertSame(lifecycle.webContext, servlet.cachedWebContext);
		verify(response).setContentLength(0);
	}

	@Test
	void missingSessionRollsBackAndWritesGenericHtmlFallback() throws Exception {
		Lifecycle lifecycle = lifecycle(actionReturning(new Download("result.txt", "download body", MimeType.plain)), false, true);
		TestableDownloadServlet servlet = new TestableDownloadServlet(lifecycle.webContext);
		CapturingServletOutputStream output = new CapturingServletOutputStream();
		HttpServletRequest request = requestWithoutSession("mod.Doc", "DownloadResult");

		servlet.doGet(request, response(output));

		assertTrue(output.asString().contains("An error occured whilst processing your report."));
		assertFalse(servlet.writeInvoked);
		assertFalse(servlet.cacheInvoked);
		verify(lifecycle.persistence).rollback();
		verify(lifecycle.persistence).commit(true);
	}

	@Test
	void invocationTargetExceptionIsUnwrappedBeforeRollbackAndFallback() throws Exception {
		DownloadAction<Bean> action = actionThrowing(new InvocationTargetException(new IllegalStateException("target failure")));
		Lifecycle lifecycle = lifecycle(action, false, true);
		TestableDownloadServlet servlet = new TestableDownloadServlet(lifecycle.webContext);
		CapturingServletOutputStream output = new CapturingServletOutputStream();

		servlet.doGet(request(lifecycle.user, "mod.Doc", "DownloadResult", null), response(output));

		assertTrue(output.asString().contains("An error occured whilst processing your report."));
		assertFalse(servlet.cacheInvoked);
		verify(lifecycle.persistence).rollback();
		verify(lifecycle.persistence).commit(true);
	}

	@Test
	@SuppressWarnings("boxing")
	void writerFailureAfterResponseCommitDoesNotWriteGenericFallback() throws Exception {
		Lifecycle lifecycle = lifecycle(actionReturning(new Download("result.txt", "download body", MimeType.plain)), false, true);
		TestableDownloadServlet servlet = new TestableDownloadServlet(lifecycle.webContext);
		servlet.writerFailure = new IOException("committed failure");
		HttpServletResponse response = mock(HttpServletResponse.class);
		when(response.isCommitted()).thenReturn(Boolean.TRUE);

		servlet.doGet(request(lifecycle.user, "mod.Doc", "DownloadResult", null), response);

		assertFalse(servlet.cacheInvoked);
		verify(response, never()).getOutputStream();
		verify(response, never()).setContentType(MimeType.html.toString());
		verify(lifecycle.persistence).rollback();
		verify(lifecycle.persistence).commit(true);
	}

	@Test
	void productionWriterWrapperDelegatesToDownloadResponseWriter() throws Exception {
		CapturingServletOutputStream output = new CapturingServletOutputStream();
		HttpServletResponse response = response(output);

		new org.skyve.impl.web.DownloadServlet().writeDownloadResponse(new Download("wrapper.txt", "wrapped", MimeType.plain), null, null, response, false);

		assertEquals("wrapped", output.asString());
		verify(response).setContentLength("wrapped".getBytes(StandardCharsets.UTF_8).length);
	}

	private static DownloadAction<Bean> actionReturning(Download download) throws Exception {
		DownloadAction<Bean> action = mock(DownloadAction.class);
		when(action.download(any(), any())).thenReturn(download);
		return action;
	}

	private static DownloadAction<Bean> actionThrowing(Exception exception) throws Exception {
		DownloadAction<Bean> action = mock(DownloadAction.class);
		when(action.download(any(), any())).thenThrow(exception);
		return action;
	}

	@SuppressWarnings("boxing") // Mockito thenReturn boxes primitive boolean return values.
	private static Lifecycle lifecycle(DownloadAction<Bean> action, boolean vetoed, boolean canExecute) throws Exception {
		Lifecycle result = new Lifecycle();
		when(result.bean.getBizId()).thenReturn("bean-id");
		result.webContext.setConversation(result.persistence);
		result.webContext.setCurrentBean(result.bean);
		when(result.user.getCustomer()).thenReturn(result.customer);
		when(result.user.getName()).thenReturn("tester");
		when(result.user.canExecuteAction(result.document, "DownloadResult")).thenReturn(Boolean.valueOf(canExecute));
		when(result.customer.getModule("mod")).thenReturn(result.module);
		when(result.module.getDocument(result.customer, "Doc")).thenReturn(result.document);
		when(result.document.getDownloadAction(result.customer, "DownloadResult", true)).thenReturn(action);
		when(result.customer.interceptBeforeDownloadAction(result.document, "DownloadResult", result.bean, result.webContext))
				.thenReturn(Boolean.valueOf(vetoed));
		return result;
	}

	private static HttpServletRequest request(User user, String documentName, String resourceName, String rangeHeader) {
		return request(user, documentName, resourceName, rangeHeader, null);
	}

	private static HttpServletRequest request(User user,
												String documentName,
												String resourceName,
												String rangeHeader,
												String ifRangeHeader) {
		HttpServletRequest request = mock(HttpServletRequest.class);
		HttpSession session = mock(HttpSession.class);
		when(session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME)).thenReturn(user);
		when(request.getSession(false)).thenReturn(session);
		when(request.getLocale()).thenReturn(Locale.ENGLISH);
		when(request.getParameter(AbstractWebContext.CONTEXT_NAME)).thenReturn("context-key");
		when(request.getParameter(AbstractWebContext.DOCUMENT_NAME)).thenReturn(documentName);
		when(request.getParameter(AbstractWebContext.RESOURCE_FILE_NAME)).thenReturn(resourceName);
		when(request.getHeader("Range")).thenReturn(rangeHeader);
		when(request.getHeader("If-Range")).thenReturn(ifRangeHeader);
		return request;
	}

	private static HttpServletRequest requestWithoutSession(String documentName, String resourceName) {
		HttpServletRequest request = mock(HttpServletRequest.class);
		when(request.getSession(false)).thenReturn(null);
		when(request.getLocale()).thenReturn(Locale.ENGLISH);
		when(request.getParameter(AbstractWebContext.CONTEXT_NAME)).thenReturn("context-key");
		when(request.getParameter(AbstractWebContext.DOCUMENT_NAME)).thenReturn(documentName);
		when(request.getParameter(AbstractWebContext.RESOURCE_FILE_NAME)).thenReturn(resourceName);
		return request;
	}

	private static HttpServletResponse response(CapturingServletOutputStream output) throws IOException {
		HttpServletResponse response = mock(HttpServletResponse.class);
		doReturn(output).when(response).getOutputStream();
		return response;
	}

	private static final class Lifecycle {
		private final TestWebContext webContext = new TestWebContext();
		private final AbstractPersistence persistence = mock(AbstractPersistence.class);
		private final CustomerImpl customer = mock(CustomerImpl.class);
		private final Module module = mock(Module.class);
		private final Document document = mock(Document.class);
		private final User user = mock(User.class);
		private final Bean bean = mock(Bean.class);
	}

	private static final class TestWebContext extends AbstractWebContext {
		private static final long serialVersionUID = 1L;

		private TestWebContext() {
			super("test-web-context");
		}

		@Override
		public List<Map<String, String>> getGrowls() {
			return null;
		}

		@Override
		public List<Map<String, String>> getMessages() {
			return null;
		}

		@Override
		public void message(MessageSeverity severity, String message) {
			// Not needed for these lifecycle tests.
		}

		@Override
		public void growl(MessageSeverity severity, String message) {
			// Not needed for these lifecycle tests.
		}

		@Override
		public void cacheConversation() {
			// Not needed for these lifecycle tests.
		}

		@Override
		public <T extends Bean> void background(Class<? extends BackgroundTask<T>> taskClass) {
			// Not needed for these lifecycle tests.
		}

		@Override
		public <T extends Bean> void backgroundWithoutCachingConversation(Class<? extends BackgroundTask<T>> taskClass) {
			// Not needed for these lifecycle tests.
		}
	}

	private static final class TestableDownloadServlet extends org.skyve.impl.web.DownloadServlet {
		private static final long serialVersionUID = 1L;

		private final AbstractWebContext webContext;
		private boolean writeInvoked;
		private boolean cacheInvoked;
		private boolean writtenHeadOnly;
		private String writtenRangeHeader;
		private String writtenIfRangeHeader;
		private Download writtenDownload;
		private AbstractWebContext cachedWebContext;
		private IOException writerFailure;

		private TestableDownloadServlet(AbstractWebContext webContext) {
			this.webContext = webContext;
		}

		@Override
		AbstractWebContext getCachedConversation(String contextKey, HttpServletRequest request) {
			return webContext;
		}

		@Override
		void writeDownloadResponse(Download result, String rangeHeader, String ifRangeHeader, HttpServletResponse response, boolean headOnly)
		throws IOException {
			writeInvoked = true;
			writtenDownload = result;
			writtenRangeHeader = rangeHeader;
			writtenIfRangeHeader = ifRangeHeader;
			writtenHeadOnly = headOnly;
			if (writerFailure != null) {
				throw writerFailure;
			}
		}

		@Override
		void cacheConversation(AbstractWebContext contextToCache) {
			cacheInvoked = true;
			cachedWebContext = contextToCache;
		}

		@Override
		public void doHead(HttpServletRequest request, HttpServletResponse response)
		throws ServletException, IOException {
			super.doHead(request, response);
		}
	}

	private static final class TrackingInputStream extends ByteArrayInputStream {
		private boolean closed;

		private TrackingInputStream(byte[] bytes) {
			super(bytes);
		}

		@Override
		public void close() throws IOException {
			closed = true;
			super.close();
		}
	}

	private static final class CapturingServletOutputStream extends ServletOutputStream {
		private final ByteArrayOutputStream bytes = new ByteArrayOutputStream();

		@Override
		public void write(int b) {
			bytes.write(b);
		}

		@Override
		public boolean isReady() {
			return true;
		}

		@Override
		public void setWriteListener(WriteListener writeListener) {
			// Synchronous test stream.
		}

		private String asString() {
			return bytes.toString(StandardCharsets.UTF_8);
		}
	}
}
