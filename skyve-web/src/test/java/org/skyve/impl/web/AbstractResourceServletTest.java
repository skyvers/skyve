package org.skyve.impl.web;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.Field;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.content.MimeType;
import org.skyve.domain.messages.SecurityException;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.router.UxUi;
import org.skyve.util.Thumbnail;

import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.WriteListener;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

@SuppressWarnings("static-method")
class AbstractResourceServletTest {

	@BeforeEach
	void beforeEach() throws Exception {
		clearThreadLocalResource();
	}

	@AfterEach
	void afterEach() throws Exception {
		clearThreadLocalResource();
	}

	@Test
	void doGetWritesTextResourceAndSetsUtf8() {
		TestServlet servlet = new TestServlet();
		TestResource resource = TestResource.with("hello".getBytes(StandardCharsets.UTF_8), "text/plain", "greeting.txt");
		servlet.resource = resource;

		HttpServletRequest request = request(Map.of(
				AbstractWebContext.DOCUMENT_NAME, "admin.User",
				AbstractWebContext.BINDING_NAME, "contact",
				AbstractWebContext.RESOURCE_FILE_NAME, "avatar"),
				null,
				null,
				false);
		HttpServletResponse response = responseWithOutput();

		servlet.doGet(request, response);

		verify(response).setContentType("text/plain");
		verify(response).setCharacterEncoding(StandardCharsets.UTF_8.name());
		verify(response).setContentLength(5);
		assertNotNull(servlet.lastParams);
		assertEquals("admin", servlet.lastParams.moduleName());
		assertEquals("User", servlet.lastParams.documentName());
		assertEquals("contact", servlet.lastParams.binding());
		assertEquals("avatar", servlet.lastParams.resourceFileName());
		assertEquals("desktop", servlet.lastUxUiName);
	}

	@Test
	void doGetSetsUtf8ForUnknownContentType() {
		TestServlet servlet = new TestServlet();
		servlet.resource = TestResource.with("abc".getBytes(StandardCharsets.UTF_8), null, "unknown.bin");

		HttpServletRequest request = request(Map.of(), null, null, false);
		HttpServletResponse response = responseWithOutput();

		servlet.doGet(request, response);

		verify(response, never()).setContentType(anyString());
		verify(response).setCharacterEncoding(StandardCharsets.UTF_8.name());
	}

	@Test
	void doGetReturns404WhenResourceBytesAreNull() throws Exception {
		TestServlet servlet = new TestServlet();
		servlet.resource = TestResource.with(null, "application/octet-stream", "missing.dat");

		HttpServletRequest request = request(Map.of(), null, null, false);
		HttpServletResponse response = responseWithOutput();

		servlet.doGet(request, response);

		verify(response).sendError(HttpServletResponse.SC_NOT_FOUND);
	}

	@Test
	void doGetReturns403WhenSecureResourceThrowsSecurityException() {
		TestServlet servlet = new TestServlet();
		servlet.resource = TestResource.with("x".getBytes(StandardCharsets.UTF_8), "text/plain", "x.txt");
		servlet.securityFailure = mock(SecurityException.class);

		HttpServletRequest request = request(Map.of(), null, null, false);
		HttpServletResponse response = responseWithOutput();

		servlet.doGet(request, response);

		verify(response).setStatus(HttpServletResponse.SC_FORBIDDEN);
	}

	@Test
	void doGetReturns500WhenUnexpectedExceptionOccurs() {
		TestServlet servlet = new TestServlet();
		servlet.resource = TestResource.throwing(new IOException("boom"));

		HttpServletRequest request = request(Map.of(), null, null, false);
		HttpServletResponse response = responseWithOutput();

		servlet.doGet(request, response);

		verify(response).setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
	}

	@Test
	void serviceDisposesThreadLocalResourceAfterGet() throws Exception {
		TestServlet servlet = new TestServlet();
		TestResource resource = TestResource.with("dispose".getBytes(StandardCharsets.UTF_8), "text/plain", "dispose.txt");
		servlet.resource = resource;

		HttpServletRequest request = request(Map.of(), null, null, true);
		HttpServletResponse response = responseWithOutput();

		servlet.service(request, response);

		assertTrue(resource.disposed);
	}

	@Test
	void getLastModifiedUsesCreatedResourceAndCachesIt() {
		TestServlet servlet = new TestServlet();
		TestResource resource = TestResource.with("data".getBytes(StandardCharsets.UTF_8), "text/plain", "f.txt");
		resource.lastModified = 1234L;
		servlet.resource = resource;

		HttpServletRequest request = request(Map.of(), null, null, false);

		long modified = servlet.getLastModified(request);

		assertEquals(1234L, modified);
		assertEquals(1, servlet.createCalls);
	}

	@Test
	void getLastModifiedReturnsMinusOneWhenCreationFails() {
		TestServlet servlet = new TestServlet();
		servlet.createFailure = new IOException("fail");

		HttpServletRequest request = request(Map.of(), null, null, false);

		assertEquals(-1L, servlet.getLastModified(request));
	}

	@Test
	void resourceDefaultLastModifiedIsUnknown() {
		AbstractResourceServlet.Resource resource = new AbstractResourceServlet.Resource() {
			@Override
			public void dispose() {
				// no-op
			}

			@Override
			public byte[] getBytes() {
				return null;
			}

			@Override
			public String getContentType() {
				return null;
			}

			@Override
			public String getFileName() {
				return null;
			}
		};

		assertEquals(-1L, resource.getLastModified());
	}

	@Test
	void parseRequestParamsUsesCookieCustomerAndResetsMalformedImageSize() {
		String originalCustomer = UtilImpl.CUSTOMER;
		try {
			UtilImpl.CUSTOMER = null;
			TestServlet servlet = new TestServlet();
			servlet.resource = TestResource.with("x".getBytes(StandardCharsets.UTF_8), "text/plain", "x.txt");

			Cookie customerCookie = new Cookie(AbstractWebContext.CUSTOMER_COOKIE_NAME, "demo");
			HttpServletRequest request = request(Map.of(
					AbstractWebContext.DOCUMENT_NAME, "malformedDocName",
					DynamicImageServlet.IMAGE_WIDTH_NAME, "NaN",
					DynamicImageServlet.IMAGE_HEIGHT_NAME, "99"),
					null,
					new Cookie[] {customerCookie},
					false);
			HttpServletResponse response = responseWithOutput();

			servlet.doGet(request, response);

			assertNotNull(servlet.lastParams);
			assertNull(servlet.lastParams.moduleName());
			assertEquals("malformedDocName", servlet.lastParams.documentName());
			assertEquals(0, servlet.lastParams.imageWidth());
			assertEquals(0, servlet.lastParams.imageHeight());
			assertEquals("demo", servlet.lastParams.customerName());
		}
		finally {
			UtilImpl.CUSTOMER = originalCustomer;
		}
	}

	@Test
	void abstractResourceUsesResolveValuesWhenNoThumbnailRequested() throws Exception {
		SimpleAbstractResource resource = new SimpleAbstractResource();
		resource.bytes = "abc".getBytes(StandardCharsets.UTF_8);
		resource.contentType = "text/plain";
		resource.fileName = "a.txt";

		assertEquals("text/plain", resource.getContentType());
		assertEquals("a.txt", resource.getFileName());
		assertEquals("abc", new String(resource.getBytes(), StandardCharsets.UTF_8));
		assertEquals(1, resource.loadCalls);
		assertFalse(resource.disposed);
		resource.dispose();
		assertTrue(resource.disposed);
	}

	@Test
	void abstractResourceUsesThumbnailValuesWhenDimensionsSet() throws Exception {
		SimpleAbstractResource resource = new SimpleAbstractResource();
		resource.thumbnail = thumbnailForFile("thumbnail-test", ".png");
		resource.imageWidth = 100;
		resource.imageHeight = 50;

		assertEquals(MimeType.png.toString(), resource.getContentType());
		assertEquals("thumbnail.png", resource.getFileName());
	}

	@Test
	void abstractResourceReturnsNullThumbnailDetailsWhenLoadReturnsNull() throws Exception {
		SimpleAbstractResource resource = new SimpleAbstractResource();
		resource.imageWidth = 10;
		resource.imageHeight = 10;
		resource.thumbnail = null;

		assertNull(resource.getContentType());
		assertNull(resource.getFileName());
	}

	private static Thumbnail thumbnailForFile(String prefix, String suffix) throws Exception {
		File file = File.createTempFile(prefix, suffix);
		Files.writeString(file.toPath(), "image-data", StandardCharsets.UTF_8);
		file.deleteOnExit();
		return new Thumbnail(file);
	}

	private static HttpServletRequest request(Map<String, String> params,
														Object session,
														Cookie[] cookies,
														boolean forService) {
		HttpServletRequest request = mock(HttpServletRequest.class);
		Map<String, String> mutable = new HashMap<>(params);
		when(request.getParameter(anyString())).thenAnswer(i -> mutable.get(i.getArgument(0, String.class)));
		when(request.getSession(false)).thenReturn((jakarta.servlet.http.HttpSession) session);
		when(request.getCookies()).thenReturn(cookies);

		UxUi uxui = UxUi.newSmartClient(UxUi.DESKTOP_NAME, "Cerulean", "omega");
		when(request.getAttribute(AbstractWebContext.UXUI)).thenReturn(uxui);

		if (forService) {
			when(request.getMethod()).thenReturn("GET");
		}

		return request;
	}

	@SuppressWarnings("resource")
	private static HttpServletResponse responseWithOutput() {
		HttpServletResponse response = mock(HttpServletResponse.class);
		try {
			when(response.getOutputStream()).thenReturn(new CapturingServletOutputStream());
		}
		catch (IOException e) {
			throw new IllegalStateException(e);
		}
		return response;
	}

	@SuppressWarnings("unchecked")
	private static void clearThreadLocalResource() throws Exception {
		Field field = AbstractResourceServlet.class.getDeclaredField("RESOURCES");
		field.setAccessible(true);
		ThreadLocal<AbstractResourceServlet.Resource> resources = (ThreadLocal<AbstractResourceServlet.Resource>) field.get(null);
		resources.remove();
	}

	private static final class TestServlet extends AbstractResourceServlet {
		private static final long serialVersionUID = 1L;

		private TestResource resource;
		private Exception createFailure;
		private SecurityException securityFailure;
		private int createCalls;
		private RequestParams lastParams;
		private String lastUxUiName;

		@Override
		protected Resource createResource(HttpServletRequest request, RequestParams params) throws Exception {
			createCalls++;
			lastParams = params;
			if (createFailure != null) {
				throw createFailure;
			}
			return resource;
		}

		@Override
		protected void secureResource(Resource resolvedResource,
									String moduleName,
									String documentName,
									String binding,
									String resourceFileName,
									org.skyve.metadata.user.User user,
									String uxui) {
			lastUxUiName = uxui;
			if (securityFailure != null) {
				throw securityFailure;
			}
		}
	}

	private static final class TestResource implements AbstractResourceServlet.Resource {
		private byte[] bytes;
		private String contentType;
		private String fileName;
		private IOException bytesException;
		private long lastModified = -1L;
		private boolean disposed;

		static TestResource with(byte[] bytes, String contentType, String fileName) {
			TestResource result = new TestResource();
			result.bytes = bytes;
			result.contentType = contentType;
			result.fileName = fileName;
			return result;
		}

		static TestResource throwing(IOException e) {
			TestResource result = with(null, "text/plain", "boom.txt");
			result.bytesException = e;
			return result;
		}

		@Override
		public void dispose() {
			disposed = true;
		}

		@Override
		public long getLastModified() {
			return lastModified;
		}

		@Override
		public byte[] getBytes() throws IOException {
			if (bytesException != null) {
				throw bytesException;
			}
			return bytes;
		}

		@Override
		public String getContentType() {
			return contentType;
		}

		@Override
		public String getFileName() {
			return fileName;
		}
	}

	private static final class SimpleAbstractResource extends AbstractResourceServlet.AbstractResource {
		private byte[] bytes;
		private String contentType;
		private String fileName;
		private Thumbnail thumbnail;
		private int loadCalls;
		private boolean disposed;

		@Override
		protected Thumbnail load() {
			loadCalls++;
			if (thumbnail != null) {
				return thumbnail;
			}
			if (bytes == null) {
				return null;
			}
			try {
				File file = File.createTempFile("resource", ".txt");
				Files.write(file.toPath(), bytes);
				file.deleteOnExit();
				return new Thumbnail(file);
			}
			catch (IOException e) {
				throw new IllegalStateException(e);
			}
		}

		@Override
		protected String resolveContentType() {
			return contentType;
		}

		@Override
		protected String resolveFileName() {
			return fileName;
		}

		@Override
		public void dispose() throws Exception {
			super.dispose();
			disposed = true;
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
			// no-op
		}

		@Override
		public void write(int b) throws IOException {
			delegate.write(b);
		}
	}
}
