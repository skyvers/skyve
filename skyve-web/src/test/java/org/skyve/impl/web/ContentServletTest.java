package org.skyve.impl.web;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.content.AttachmentContent;
import org.skyve.content.ContentManager;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;

import javax.imageio.ImageIO;

import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.WriteListener;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

@SuppressWarnings({"static-method", "resource"})
class ContentServletTest {
	@AfterEach
	void tearDown() throws Exception {
		clearThreadPersistence();
		clearThreadLocalResource();
	}

	@BeforeEach
	void setUp() throws Exception {
		clearThreadLocalResource();
	}

	@Test
	void addResponseHeadersSanitisesFileNameAndSetsCacheHeaders() throws Exception {
		ContentServlet servlet = new ContentServlet();
		AbstractResourceServlet.Resource resource = newContentResource(newAttachmentContent("report\u0001.svg"));
		HttpServletResponse response = mock(HttpServletResponse.class);

		servlet.addResponseHeaders(resource, response);

		verify(response).setHeader("Content-Disposition", "inline; filename=\"report.svg\"");
		verify(response).setHeader("Cache-Control", "cache");
		verify(response).setHeader("Pragma", "cache");
		verify(response, never()).setHeader("Accept-Ranges", "bytes");
		verify(response).addDateHeader(org.mockito.ArgumentMatchers.eq("Expires"), org.mockito.ArgumentMatchers.anyLong());
	}

	@Test
	void secureResourceReturnsWhenContentIsMissing() throws Exception {
		ContentServlet servlet = new ContentServlet();
		AbstractResourceServlet.Resource resource = newContentResource(null);

		User user = mock(User.class);
		assertDoesNotThrow(() -> servlet.secureResource(resource, "admin", "Contact", "name", "contentId", user, "desktop"));
	}

	@Test
	void secureResourceRejectsAnonymousUserWhenContentPresent() throws Exception {
		ContentServlet servlet = new ContentServlet();
		AbstractResourceServlet.Resource resource = newContentResource(newAttachmentContent("report.txt"));
		bindPersistenceToThread();

		assertThrows(IllegalArgumentException.class, () -> servlet.secureResource(resource, "admin", "Contact", "name", "contentId", null, "desktop"));
	}

	@Test
	void secureResourceRejectsMissingContentCoordinates() throws Exception {
		ContentServlet servlet = new ContentServlet();
		AbstractResourceServlet.Resource resource = newContentResource(newAttachmentContent("report.txt"));
		User user = mock(User.class);
		bindPersistenceToThread();

		assertThrows(DomainException.class, () -> servlet.secureResource(resource, null, "Contact", "name", "contentId", user, "desktop"));
		assertThrows(DomainException.class, () -> servlet.secureResource(resource, "admin", null, "name", "contentId", user, "desktop"));
		assertThrows(DomainException.class, () -> servlet.secureResource(resource, "admin", "Contact", null, "contentId", user, "desktop"));
	}

	@Test
	void secureResourceRejectsImplicitAttributeBindings() throws Exception {
		ContentServlet servlet = new ContentServlet();
		AbstractResourceServlet.Resource resource = newContentResource(newAttachmentContent("report.txt"));
		bindPersistenceToThread();

		DocumentImpl document = new DocumentImpl();
		document.setName("Contact");
		document.setOwningModuleName("admin");
		Module module = mock(Module.class);
		when(module.getDocument(org.mockito.ArgumentMatchers.any(), org.mockito.ArgumentMatchers.eq("Contact"))).thenReturn(document);
		Customer customer = mock(Customer.class);
		when(customer.getModule("admin")).thenReturn(module);
		User user = newContentUser(customer);

		assertThrows(IllegalArgumentException.class, () -> servlet.secureResource(resource, "admin", "Contact", "bizId", "contentId", user, "desktop"));
	}

	@Test
	void secureResourceAllowsReadableExplicitAttribute() throws Exception {
		ContentServlet servlet = new ContentServlet();
		AbstractResourceServlet.Resource resource = newContentResource(newAttachmentContent("report.txt"));

		DocumentImpl document = new DocumentImpl();
		document.setName("Contact");
		document.setOwningModuleName("admin");
		Text attachment = new Text();
		attachment.setName("name");
		document.putAttribute(attachment);

		Module module = mock(Module.class);
		when(module.getDocument(org.mockito.ArgumentMatchers.any(), org.mockito.ArgumentMatchers.eq("Contact"))).thenReturn(document);

		Customer customer = mock(Customer.class);
		when(customer.getModule("admin")).thenReturn(module);

		User user = newContentUser(customer);

		assertDoesNotThrow(() -> servlet.secureResource(resource, "admin", "Contact", "name", "contentId", user, "desktop"));
	}

	@Test
	void contentResourceExposesAttachmentMetadataAndBytes() throws Exception {
		AttachmentContent content = newAttachmentContent("report.txt");
		content.setLastModified(new Date(12345L));
		AbstractResourceServlet.Resource resource = newContentResource(content);

		assertEquals(12345L, resource.getLastModified());
		assertEquals("text/plain", resource.getContentType());
		assertEquals("report.txt", resource.getFileName());
		assertArrayEquals(new byte[] {1}, resource.getBytes());
	}

	@Test
	void contentResourceStreamsAttachmentMetadataWithoutMaterialisingFileBytes() throws Exception {
		File file = File.createTempFile("content-stream", ".txt");
		Files.writeString(file.toPath(), "streamed content", StandardCharsets.UTF_8);
		file.deleteOnExit();
		AttachmentContent content = newAttachmentContent("streamed.txt", file);
		AbstractResourceServlet.Resource resource = newContentResource(content);

		assertEquals(file.length(), ((AbstractResourceServlet.StreamableResource) resource).getContentLength());
		try (java.io.InputStream in = ((AbstractResourceServlet.StreamableResource) resource).openStream()) {
			assertArrayEquals("streamed content".getBytes(StandardCharsets.UTF_8), in.readAllBytes());
		}
		assertNull(getCachedBytes(content));
	}

	@Test
	void shouldStreamOriginalRequiresUnmarkedContentAndNoThumbnailDimensions() throws Exception {
		ContentServlet servlet = new ContentServlet();
		AttachmentContent plain = newAttachmentContent("plain.txt");
		AbstractResourceServlet.Resource plainResource = newContentResource(plain);
		AbstractResourceServlet.RequestParams normalParams = requestParams(0, 0);

		assertTrue(servlet.shouldStreamOriginal(normalParams, plainResource));

		AttachmentContent marked = newAttachmentContent("marked.txt");
		marked.setMarkup("<svg/>");
		assertFalse(servlet.shouldStreamOriginal(normalParams, newContentResource(marked)));
		assertFalse(servlet.shouldStreamOriginal(requestParams(16, 16), plainResource));
		assertFalse(servlet.shouldStreamOriginal(normalParams, newContentResource(null)));
	}

	@Test
	void doGetStreamsFullUntransformedContentWithoutCachingFileBytes() throws Exception {
		File file = File.createTempFile("content-full", ".mp4");
		byte[] bytes = "0123456789".getBytes(StandardCharsets.UTF_8);
		Files.write(file.toPath(), bytes);
		file.deleteOnExit();
		AttachmentContent content = newAttachmentContent("clip.mp4", "video/mp4", file);
		TestContentServlet servlet = new TestContentServlet(newContentResource(content));
		CapturingServletOutputStream out = new CapturingServletOutputStream();

		servlet.doGet(request(Map.of(), null), responseWithOutput(out));

		assertArrayEquals(bytes, out.toByteArray());
		assertNull(getCachedBytes(content));
		assertEquals(1, servlet.secureCalls);
	}

	@Test
	void doGetStreamsContentRange() throws Exception {
		File file = File.createTempFile("content-range", ".bin");
		Files.writeString(file.toPath(), "0123456789", StandardCharsets.UTF_8);
		file.deleteOnExit();
		AttachmentContent content = newAttachmentContent("movie.bin", "application/octet-stream", file);
		TestContentServlet servlet = new TestContentServlet(newContentResource(content));
		CapturingServletOutputStream out = new CapturingServletOutputStream();
		HttpServletResponse response = responseWithOutput(out);

		servlet.doGet(request(Map.of(), "bytes=4-7"), response);

		verify(response).setStatus(HttpServletResponse.SC_PARTIAL_CONTENT);
		verify(response).setHeader("Accept-Ranges", "bytes");
		verify(response).setHeader("Content-Range", "bytes 4-7/10");
		verify(response).setContentLengthLong(4L);
		assertArrayEquals("4567".getBytes(StandardCharsets.UTF_8), out.toByteArray());
		assertNull(getCachedBytes(content));
	}

	@Test
	void doGetFallsBackToFullStreamForMalformedRange() throws Exception {
		File file = File.createTempFile("content-malformed-range", ".bin");
		byte[] bytes = "0123456789".getBytes(StandardCharsets.UTF_8);
		Files.write(file.toPath(), bytes);
		file.deleteOnExit();
		AttachmentContent content = newAttachmentContent("movie.bin", "application/octet-stream", file);
		TestContentServlet servlet = new TestContentServlet(newContentResource(content));
		CapturingServletOutputStream out = new CapturingServletOutputStream();
		HttpServletResponse response = responseWithOutput(out);

		servlet.doGet(request(Map.of(), "bytes=not-a-range"), response);

		verify(response, never()).setStatus(HttpServletResponse.SC_PARTIAL_CONTENT);
		verify(response, never()).setHeader(org.mockito.ArgumentMatchers.eq("Content-Range"), org.mockito.ArgumentMatchers.anyString());
		verify(response).setHeader("Accept-Ranges", "bytes");
		verify(response).setContentLengthLong(bytes.length);
		assertArrayEquals(bytes, out.toByteArray());
		assertNull(getCachedBytes(content));
	}

	@Test
	void doHeadSetsStreamHeadersWithoutWritingBody() throws Exception {
		File file = File.createTempFile("content-head", ".mp3");
		Files.writeString(file.toPath(), "0123456789", StandardCharsets.UTF_8);
		file.deleteOnExit();
		AttachmentContent content = newAttachmentContent("clip.mp3", "audio/mpeg", file);
		TestContentServlet servlet = new TestContentServlet(newContentResource(content));
		CapturingServletOutputStream out = new CapturingServletOutputStream();
		HttpServletResponse response = responseWithOutput(out);

		servlet.doHead(request(Map.of(), "bytes=2-5"), response);

		verify(response).setContentType("audio/mpeg");
		verify(response).setHeader("Accept-Ranges", "bytes");
		verify(response).setStatus(HttpServletResponse.SC_PARTIAL_CONTENT);
		verify(response).setHeader("Content-Range", "bytes 2-5/10");
		verify(response).setContentLengthLong(4L);
		verify(response, never()).getOutputStream();
		assertEquals(0, out.size());
		assertNull(getCachedBytes(content));
	}

	@Test
	void doGetReturns416ForUnsatisfiableContentRangeWithoutOpeningBodyStream() throws Exception {
		File file = File.createTempFile("content-unsatisfiable", ".bin");
		Files.writeString(file.toPath(), "0123456789", StandardCharsets.UTF_8);
		file.deleteOnExit();
		AttachmentContent content = newAttachmentContent("movie.bin", "application/octet-stream", file);
		TestContentServlet servlet = new TestContentServlet(newContentResource(content));
		CapturingServletOutputStream out = new CapturingServletOutputStream();
		HttpServletResponse response = responseWithOutput(out);

		servlet.doGet(request(Map.of(), "bytes=99-"), response);

		verify(response).setStatus(HttpServletResponse.SC_REQUESTED_RANGE_NOT_SATISFIABLE);
		verify(response).setHeader("Content-Range", "bytes */10");
		verify(response, never()).getOutputStream();
		assertEquals(0, out.size());
		assertNull(getCachedBytes(content));
	}

	@Test
	void doGetUsesBufferedPathWhenThumbnailDimensionsAreRequested() throws Exception {
		byte[] bytes = "thumbnail-source".getBytes(StandardCharsets.UTF_8);
		AttachmentContent content = newAttachmentContent("thumb.txt", "text/plain", bytes);
		TestContentServlet servlet = new TestContentServlet(newContentResource(content));
		CapturingServletOutputStream out = new CapturingServletOutputStream();
		HttpServletResponse response = responseWithOutput(out);

		servlet.doGet(request(Map.of("_w", "32", "_h", "32"), "bytes=1-4"), response);

		verify(response, never()).setHeader("Accept-Ranges", "bytes");
		verify(response, never()).setStatus(HttpServletResponse.SC_PARTIAL_CONTENT);
		verify(response).setContentLength(bytes.length);
		assertArrayEquals(bytes, out.toByteArray());
	}

	@Test
	void doGetUsesBufferedPathForMarkedContent() throws Exception {
		File file = File.createTempFile("content-marked", ".png");
		ImageIO.write(new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB), "png", file);
		file.deleteOnExit();
		AttachmentContent content = newAttachmentContent("marked.png", "image/png", file);
		content.setContentId("01234567-89ab-cdef-0123-456789abcdef");
		content.setMarkup("<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"1\" height=\"1\"></svg>");
		TestContentServlet servlet = new TestContentServlet(newContentResource(content));
		CapturingServletOutputStream out = new CapturingServletOutputStream();
		HttpServletResponse response = responseWithOutput(out);

		servlet.doGet(request(Map.of(), "bytes=1-3"), response);

		verify(response, never()).setHeader("Accept-Ranges", "bytes");
		verify(response, never()).setStatus(HttpServletResponse.SC_PARTIAL_CONTENT);
		verify(response, never()).setContentLengthLong(org.mockito.ArgumentMatchers.anyLong());
		verify(response, never()).getOutputStream();
		assertEquals(0, out.size());
	}

	@Test
	void doGetReturns404ForMissingContent() throws Exception {
		TestContentServlet servlet = new TestContentServlet(newContentResource(null));
		HttpServletResponse response = responseWithOutput(new CapturingServletOutputStream());

		servlet.doGet(request(Map.of(), null), response);

		verify(response).sendError(HttpServletResponse.SC_NOT_FOUND);
	}

	@Test
	void deniedGetAndHeadDoNotAdvertiseRangesOrWriteBody() throws Exception {
		AttachmentContent content = newAttachmentContent("clip.mp4", "video/mp4", "012345".getBytes(StandardCharsets.UTF_8));
		TestContentServlet servlet = new TestContentServlet(newContentResource(content));
		servlet.deny = true;
		CapturingServletOutputStream out = new CapturingServletOutputStream();
		HttpServletResponse response = responseWithOutput(out);

		servlet.doGet(request(Map.of(), "bytes=1-3"), response);

		verify(response).setStatus(HttpServletResponse.SC_FORBIDDEN);
		verify(response, never()).setHeader("Accept-Ranges", "bytes");
		assertEquals(0, out.size());

		CapturingServletOutputStream headOut = new CapturingServletOutputStream();
		HttpServletResponse headResponse = responseWithOutput(headOut);
		servlet.doHead(request(Map.of(), "bytes=1-3"), headResponse);

		verify(headResponse).setStatus(HttpServletResponse.SC_FORBIDDEN);
		verify(headResponse, never()).setHeader("Accept-Ranges", "bytes");
		assertEquals(0, headOut.size());
	}

	@Test
	void contentResourceReturnsDefaultsWhenContentMissing() throws Exception {
		AbstractResourceServlet.Resource resource = newContentResource(null);

		assertEquals(-1L, resource.getLastModified());
		assertNull(resource.getContentType());
		assertNull(resource.getFileName());
		assertNull(resource.getBytes());
	}

	@Test
	void contentResourceDisposeClosesContentManagerAndClearsContent() throws Exception {
		AbstractResourceServlet.Resource resource = newContentResource(newAttachmentContent("report.txt"));
		try (ContentManager contentManager = mock(ContentManager.class)) {
			setContentManager(resource, contentManager);

			resource.dispose();

			verify(contentManager).close();
			assertNull(resource.getContentType());
			assertNull(resource.getFileName());
		}
	}

	private static AttachmentContent newAttachmentContent(String fileName) {
		return new AttachmentContent("testCustomer", "admin", "Contact", "group", "user", "bizId", "name")
				.attachment(fileName, "text/plain", new byte[] {1});
	}

	private static AttachmentContent newAttachmentContent(String fileName, File file) {
		return newAttachmentContent(fileName, "text/plain", file);
	}

	private static AttachmentContent newAttachmentContent(String fileName, String contentType, File file) {
		return new AttachmentContent("testCustomer", "admin", "Contact", "group", "user", "bizId", "name")
				.attachment(fileName, contentType, file);
	}

	private static AttachmentContent newAttachmentContent(String fileName, String contentType, byte[] bytes) {
		return new AttachmentContent("testCustomer", "admin", "Contact", "group", "user", "bizId", "name")
				.attachment(fileName, contentType, bytes);
	}

	private static AbstractResourceServlet.Resource newContentResource(AttachmentContent content) throws Exception {
		Class<?> clazz = Class.forName("org.skyve.impl.web.ContentServlet$ContentResource");
		Constructor<?> constructor = clazz.getDeclaredConstructor();
		constructor.setAccessible(true);
		Object resource = constructor.newInstance();

		Field contentField = clazz.getDeclaredField("content");
		contentField.setAccessible(true);
		contentField.set(resource, content);

		return (AbstractResourceServlet.Resource) resource;
	}

	private static void setContentManager(AbstractResourceServlet.Resource resource, ContentManager contentManager) throws Exception {
		Field contentManagerField = resource.getClass().getDeclaredField("cm");
		contentManagerField.setAccessible(true);
		contentManagerField.set(resource, contentManager);
	}

	private static byte[] getCachedBytes(AttachmentContent content) throws Exception {
		Field bytesField = AttachmentContent.class.getDeclaredField("bytes");
		bytesField.setAccessible(true);
		return (byte[]) bytesField.get(content);
	}

	private static AbstractResourceServlet.RequestParams requestParams(int imageWidth, int imageHeight) {
		return new AbstractResourceServlet.RequestParams(
				"admin",
				"Contact",
				"name",
				"01234567-89ab-cdef-0123-456789abcdef",
				imageWidth,
				imageHeight,
				null,
				null,
				"testCustomer");
	}

	private static HttpServletRequest request(Map<String, String> params, String rangeHeader) {
		HttpServletRequest request = mock(HttpServletRequest.class);
		Map<String, String> mutable = new HashMap<>(params);
		when(request.getParameter(anyString())).thenAnswer(i -> mutable.get(i.getArgument(0, String.class)));
		when(request.getHeader("Range")).thenReturn(rangeHeader);
		org.skyve.metadata.router.UxUi uxui = org.skyve.metadata.router.UxUi.newSmartClient(org.skyve.metadata.router.UxUi.DESKTOP_NAME, "Cerulean", "omega");
		when(request.getAttribute(AbstractWebContext.UXUI)).thenReturn(uxui);
		return request;
	}

	private static HttpServletResponse responseWithOutput(CapturingServletOutputStream out) {
		HttpServletResponse response = mock(HttpServletResponse.class);
		try {
			when(response.getOutputStream()).thenReturn(out);
		}
		catch (java.io.IOException e) {
			throw new IllegalStateException(e);
		}
		return response;
	}

	private static User newContentUser(Customer customer) {
		return mock(User.class, invocation -> {
			String method = invocation.getMethod().getName();
			if ("getCustomer".equals(method)) {
				return customer;
			}
			if ("getName".equals(method)) {
				return "tester";
			}
			if ("canTextSearch".equals(method)) {
				return Boolean.TRUE;
			}
			if ("canAccessContent".equals(method)) {
				return Boolean.TRUE;
			}
			return org.mockito.Answers.RETURNS_DEFAULTS.answer(invocation);
		});
	}

	private static void bindPersistenceToThread() {
		AbstractPersistence persistence = mock(AbstractPersistence.class, CALLS_REAL_METHODS);
		persistence.setForThread();
	}

	@SuppressWarnings("unchecked")
	private static void clearThreadPersistence() {
		try {
			Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
			field.setAccessible(true);
			((ThreadLocal<AbstractPersistence>) field.get(null)).remove();
		}
		catch (Exception e) {
			throw new IllegalStateException("Unable to clear thread local persistence", e);
		}
	}

	@SuppressWarnings("unchecked")
	private static void clearThreadLocalResource() throws Exception {
		Field field = AbstractResourceServlet.class.getDeclaredField("RESOURCES");
		field.setAccessible(true);
		ThreadLocal<AbstractResourceServlet.Resource> resources = (ThreadLocal<AbstractResourceServlet.Resource>) field.get(null);
		resources.remove();
	}

	private static final class TestContentServlet extends ContentServlet {
		private static final long serialVersionUID = 1L;

		private final AbstractResourceServlet.Resource resource;
		private boolean deny;
		private int secureCalls;

		private TestContentServlet(AbstractResourceServlet.Resource resource) {
			this.resource = resource;
		}

		@Override
		protected Resource createResource(HttpServletRequest request, RequestParams params) {
			return resource;
		}

		@Override
		protected void secureResource(Resource resolvedResource,
										String moduleName,
										String documentName,
										String binding,
										String resourceFileName,
										User user,
										String uxui) {
			secureCalls++;
			if (deny) {
				throw mock(org.skyve.domain.messages.SecurityException.class);
			}
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
		public void write(int b) {
			delegate.write(b);
		}

		byte[] toByteArray() {
			return delegate.toByteArray();
		}

		int size() {
			return delegate.size();
		}
	}
}
