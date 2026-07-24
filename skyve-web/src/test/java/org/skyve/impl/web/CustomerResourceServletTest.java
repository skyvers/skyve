package org.skyve.impl.web;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
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
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.repository.ProvidedRepository;

import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.WriteListener;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

// Reflection, temporary files, cookies, repeated values and assertions are isolated test fixtures.
@SuppressWarnings({ "static-method", "resource", "java:S112", "java:S1192", "java:S2092", "java:S3011",
		"java:S5443", "java:S5960" })
class CustomerResourceServletTest {

	private ProvidedRepository originalRepository;

	@BeforeEach
	void setUp() {
		originalRepository = ProvidedRepositoryFactory.get();
	}

	@AfterEach
	void tearDown() throws Exception {
		ProvidedRepositoryFactory.set(originalRepository);
		clearThreadLocalResource();
	}

	@Test
	void createResourceReturnsDirectRepositoryFileWhenItExists() throws Exception {
		ProvidedRepository repository = mock(ProvidedRepository.class);
		File resourceFile = File.createTempFile("customer-resource", ".txt");
		Files.writeString(resourceFile.toPath(), "hello", StandardCharsets.UTF_8);
		resourceFile.deleteOnExit();

		when(repository.findResourceFile("logo.txt", "demo", "admin")).thenReturn(resourceFile);
		ProvidedRepositoryFactory.set(repository);

		CustomerResourceServlet servlet = new CustomerResourceServlet();
		AbstractResourceServlet.RequestParams params = new AbstractResourceServlet.RequestParams(
				"admin",
				"User",
				null,
				"logo.txt",
				0,
				0,
				null,
				null,
				"demo");

		AbstractResourceServlet.Resource resource = servlet.createResource(mock(jakarta.servlet.http.HttpServletRequest.class), params);

		assertNotNull(resource);
		assertEquals("text/plain", resource.getContentType());
		assertEquals(resourceFile.getName(), resource.getFileName());
		assertArrayEquals("hello".getBytes(StandardCharsets.UTF_8), resource.getBytes());
		assertEquals(resourceFile.lastModified(), resource.getLastModified());
		assertTrue(resource instanceof AbstractResourceServlet.StreamableResource);
		AbstractResourceServlet.StreamableResource streamable = (AbstractResourceServlet.StreamableResource) resource;
		assertEquals(resourceFile.length(), streamable.getContentLength());
		assertTrue(streamable.isRangeSupported());
		verify(repository).findResourceFile("logo.txt", "demo", "admin");
	}

	@Test
	void doGetStreamsFullRepositoryFile() throws Exception {
		ProvidedRepository repository = mock(ProvidedRepository.class);
		File resourceFile = File.createTempFile("customer-resource-full", ".mpg");
		byte[] bytes = "0123456789".getBytes(StandardCharsets.UTF_8);
		Files.write(resourceFile.toPath(), bytes);
		resourceFile.deleteOnExit();

		when(repository.findResourceFile("clip.mpg", "demo", "admin")).thenReturn(resourceFile);
		ProvidedRepositoryFactory.set(repository);

		CustomerResourceServlet servlet = new CustomerResourceServlet();
		CapturingServletOutputStream out = new CapturingServletOutputStream();
		HttpServletRequest request = request(Map.of(
				AbstractWebContext.DOCUMENT_NAME, "admin.User",
				AbstractWebContext.RESOURCE_FILE_NAME, "clip.mpg"),
				null);
		HttpServletResponse response = responseWithOutput(out);

		servlet.doGet(request, response);

		verify(response).setContentType("video/mpeg");
		verify(response).setHeader("Accept-Ranges", "bytes");
		verify(response).setContentLengthLong(bytes.length);
		verify(response, never()).setContentLength(bytes.length);
		assertArrayEquals(bytes, out.toByteArray());
	}

	@Test
	void doGetStreamsRepositoryFileRange() throws Exception {
		ProvidedRepository repository = mock(ProvidedRepository.class);
		File resourceFile = File.createTempFile("customer-resource-range", ".bin");
		byte[] bytes = "0123456789".getBytes(StandardCharsets.UTF_8);
		Files.write(resourceFile.toPath(), bytes);
		resourceFile.deleteOnExit();

		when(repository.findResourceFile("movie.bin", "demo", "admin")).thenReturn(resourceFile);
		ProvidedRepositoryFactory.set(repository);

		CustomerResourceServlet servlet = new CustomerResourceServlet();
		CapturingServletOutputStream out = new CapturingServletOutputStream();
		HttpServletRequest request = request(Map.of(
				AbstractWebContext.DOCUMENT_NAME, "admin.User",
				AbstractWebContext.RESOURCE_FILE_NAME, "movie.bin"),
				"bytes=3-6");
		HttpServletResponse response = responseWithOutput(out);

		servlet.doGet(request, response);

		verify(response).setStatus(HttpServletResponse.SC_PARTIAL_CONTENT);
		verify(response).setHeader("Accept-Ranges", "bytes");
		verify(response).setHeader("Content-Range", "bytes 3-6/10");
		verify(response).setContentLengthLong(4L);
		assertArrayEquals("3456".getBytes(StandardCharsets.UTF_8), out.toByteArray());
	}

	@Test
	void doGetReturns416ForUnsatisfiableRepositoryFileRange() throws Exception {
		ProvidedRepository repository = mock(ProvidedRepository.class);
		File resourceFile = File.createTempFile("customer-resource-unsat", ".bin");
		Files.writeString(resourceFile.toPath(), "0123456789", StandardCharsets.UTF_8);
		resourceFile.deleteOnExit();

		when(repository.findResourceFile("movie.bin", "demo", "admin")).thenReturn(resourceFile);
		ProvidedRepositoryFactory.set(repository);

		CustomerResourceServlet servlet = new CustomerResourceServlet();
		CapturingServletOutputStream out = new CapturingServletOutputStream();
		HttpServletRequest request = request(Map.of(
				AbstractWebContext.DOCUMENT_NAME, "admin.User",
				AbstractWebContext.RESOURCE_FILE_NAME, "movie.bin"),
				"bytes=20-");
		HttpServletResponse response = responseWithOutput(out);

		servlet.doGet(request, response);

		verify(response).setStatus(HttpServletResponse.SC_REQUESTED_RANGE_NOT_SATISFIABLE);
		verify(response).setHeader("Content-Range", "bytes */10");
		verify(response, never()).getOutputStream();
		assertEquals(0, out.size());
	}

	@Test
	void shouldStreamOriginalReturnsFalseWhenThumbnailDimensionsAreRequested() throws Exception {
		ProvidedRepository repository = mock(ProvidedRepository.class);
		File resourceFile = File.createTempFile("customer-resource-thumb", ".txt");
		Files.writeString(resourceFile.toPath(), "thumbnail-source", StandardCharsets.UTF_8);
		resourceFile.deleteOnExit();

		when(repository.findResourceFile("logo.txt", "demo", "admin")).thenReturn(resourceFile);
		ProvidedRepositoryFactory.set(repository);

		CustomerResourceServlet servlet = new CustomerResourceServlet();
		AbstractResourceServlet.RequestParams params = new AbstractResourceServlet.RequestParams(
				"admin",
				"User",
				null,
				"logo.txt",
				16,
				16,
				null,
				null,
				"demo");

		AbstractResourceServlet.Resource resource = servlet.createResource(mock(HttpServletRequest.class), params);

		assertTrue(resource instanceof AbstractResourceServlet.StreamableResource);
		assertFalse(servlet.shouldStreamOriginal(params, resource));
	}

	@Test
	void streamableResourceReportsUnknownLengthWhenRepositoryFileIsMissing() throws Exception {
		ProvidedRepository repository = mock(ProvidedRepository.class);
		File missingFile = new File(System.getProperty("java.io.tmpdir"), "missing-customer-resource-contract-" + System.nanoTime() + ".txt");
		when(repository.findResourceFile("missing.txt", "demo", "admin")).thenReturn(missingFile);
		ProvidedRepositoryFactory.set(repository);

		CustomerResourceServlet servlet = new CustomerResourceServlet();
		AbstractResourceServlet.RequestParams params = new AbstractResourceServlet.RequestParams(
				"admin",
				"User",
				null,
				"missing.txt",
				0,
				0,
				null,
				null,
				"demo");

		AbstractResourceServlet.Resource resource = servlet.createResource(mock(HttpServletRequest.class), params);

		assertEquals(-1L, resource.getLastModified());
		assertEquals(null, resource.getContentType());
		assertEquals(null, resource.getFileName());
		assertTrue(resource instanceof AbstractResourceServlet.StreamableResource);
		AbstractResourceServlet.StreamableResource streamable = (AbstractResourceServlet.StreamableResource) resource;
		assertEquals(-1L, streamable.getContentLength());
		assertFalse(streamable.isRangeSupported());
	}

	@Test
	void streamableResourceRejectsDirectoryBackedRepositoryResult() throws Exception {
		ProvidedRepository repository = mock(ProvidedRepository.class);
		File directory = Files.createTempDirectory("customer-resource-directory").toFile();
		directory.deleteOnExit();
		when(repository.findResourceFile("folder", "demo", "admin")).thenReturn(directory);
		ProvidedRepositoryFactory.set(repository);

		CustomerResourceServlet servlet = new CustomerResourceServlet();
		AbstractResourceServlet.RequestParams params = new AbstractResourceServlet.RequestParams(
				"admin",
				"User",
				null,
				"folder",
				0,
				0,
				null,
				null,
				"demo");

		AbstractResourceServlet.Resource resource = servlet.createResource(mock(HttpServletRequest.class), params);

		assertTrue(resource instanceof AbstractResourceServlet.StreamableResource);
		AbstractResourceServlet.StreamableResource streamable = (AbstractResourceServlet.StreamableResource) resource;
		assertEquals(-1L, streamable.getContentLength());
		assertFalse(streamable.isRangeSupported());
	}

	@Test
	void resourceWithUnknownExtensionHasNoContentTypeButCanStillStream() throws Exception {
		ProvidedRepository repository = mock(ProvidedRepository.class);
		File resourceFile = File.createTempFile("customer-resource-unknown", ".xyzzy");
		Files.writeString(resourceFile.toPath(), "opaque", StandardCharsets.UTF_8);
		resourceFile.deleteOnExit();
		when(repository.findResourceFile("opaque.xyzzy", "demo", "admin")).thenReturn(resourceFile);
		ProvidedRepositoryFactory.set(repository);

		CustomerResourceServlet servlet = new CustomerResourceServlet();
		AbstractResourceServlet.RequestParams params = new AbstractResourceServlet.RequestParams(
				"admin",
				"User",
				null,
				"opaque.xyzzy",
				0,
				0,
				null,
				null,
				"demo");

		AbstractResourceServlet.Resource resource = servlet.createResource(mock(HttpServletRequest.class), params);

		assertEquals(null, resource.getContentType());
		assertEquals(resourceFile.getName(), resource.getFileName());
		assertTrue(resource instanceof AbstractResourceServlet.StreamableResource);
		assertTrue(((AbstractResourceServlet.StreamableResource) resource).isRangeSupported());
	}

	@Test
	void disposeClearsResolvedFileState() throws Exception {
		ProvidedRepository repository = mock(ProvidedRepository.class);
		File resourceFile = File.createTempFile("customer-resource-dispose", ".txt");
		Files.writeString(resourceFile.toPath(), "dispose", StandardCharsets.UTF_8);
		resourceFile.deleteOnExit();
		when(repository.findResourceFile("dispose.txt", "demo", "admin")).thenReturn(resourceFile);
		ProvidedRepositoryFactory.set(repository);

		CustomerResourceServlet servlet = new CustomerResourceServlet();
		AbstractResourceServlet.RequestParams params = new AbstractResourceServlet.RequestParams(
				"admin",
				"User",
				null,
				"dispose.txt",
				0,
				0,
				null,
				null,
				"demo");
		AbstractResourceServlet.Resource resource = servlet.createResource(mock(HttpServletRequest.class), params);

		resource.dispose();

		assertEquals(null, resource.getContentType());
		assertEquals(null, resource.getFileName());
		assertEquals(-1L, resource.getLastModified());
		assertTrue(resource instanceof AbstractResourceServlet.StreamableResource);
		AbstractResourceServlet.StreamableResource streamable = (AbstractResourceServlet.StreamableResource) resource;
		assertEquals(-1L, streamable.getContentLength());
		assertFalse(streamable.isRangeSupported());
	}

	@Test
	void doGetReturns404WhenRepositoryFileIsMissing() throws Exception {
		ProvidedRepository repository = mock(ProvidedRepository.class);
		File missingFile = new File(System.getProperty("java.io.tmpdir"), "missing-customer-resource-" + System.nanoTime() + ".txt");
		when(repository.findResourceFile("missing.txt", "demo", "admin")).thenReturn(missingFile);
		ProvidedRepositoryFactory.set(repository);

		CustomerResourceServlet servlet = new CustomerResourceServlet();
		HttpServletRequest request = request(Map.of(
				AbstractWebContext.DOCUMENT_NAME, "admin.User",
				AbstractWebContext.RESOURCE_FILE_NAME, "missing.txt"),
				null);
		HttpServletResponse response = responseWithOutput(new CapturingServletOutputStream());

		servlet.doGet(request, response);

		verify(response).sendError(HttpServletResponse.SC_NOT_FOUND);
	}

	@Test
	void createResourceFallsBackToBaseFileNameWhenVariantLookupMisses() throws Exception {
		ProvidedRepository repository = mock(ProvidedRepository.class);
		File missingVariant = new File(System.getProperty("java.io.tmpdir"), "icon_24.png");
		File fallbackFile = File.createTempFile("icon", ".txt");
		Files.writeString(fallbackFile.toPath(), "png-data", StandardCharsets.UTF_8);
		fallbackFile.deleteOnExit();

		when(repository.findResourceFile("icon_24.png", "demo", "admin")).thenReturn(missingVariant);
		when(repository.findResourceFile("icon.png", "demo", "admin")).thenReturn(fallbackFile);
		ProvidedRepositoryFactory.set(repository);

		CustomerResourceServlet servlet = new CustomerResourceServlet();
		AbstractResourceServlet.RequestParams params = new AbstractResourceServlet.RequestParams(
				"admin",
				"User",
				null,
				"icon_24.png",
				0,
				0,
				null,
				null,
				"demo");

		AbstractResourceServlet.Resource resource = servlet.createResource(mock(jakarta.servlet.http.HttpServletRequest.class), params);

		assertNotNull(resource);
		assertEquals("text/plain", resource.getContentType());
		assertEquals(fallbackFile.getName(), resource.getFileName());
		assertNotNull(resource.getBytes());
		verify(repository).findResourceFile("icon_24.png", "demo", "admin");
		verify(repository).findResourceFile("icon.png", "demo", "admin");
	}

	@Test
	void createResourceReturnsEmptyResourceWhenFileNameMissing() throws Exception {
		ProvidedRepository repository = mock(ProvidedRepository.class);
		ProvidedRepositoryFactory.set(repository);

		CustomerResourceServlet servlet = new CustomerResourceServlet();
		AbstractResourceServlet.RequestParams params = new AbstractResourceServlet.RequestParams(
				"admin",
				"User",
				null,
				null,
				0,
				0,
				null,
				null,
				"demo");

		AbstractResourceServlet.Resource resource = servlet.createResource(mock(jakarta.servlet.http.HttpServletRequest.class), params);

		assertNotNull(resource);
		assertEquals(null, resource.getContentType());
		assertEquals(null, resource.getFileName());
	}

	@Test
	void createResourceDoesNotFallbackWhenVariantHasNoExtension() throws Exception {
		ProvidedRepository repository = mock(ProvidedRepository.class);
		File missingVariant = new File(System.getProperty("java.io.tmpdir"), "icon_24");
		when(repository.findResourceFile("icon_24", "demo", "admin")).thenReturn(missingVariant);
		ProvidedRepositoryFactory.set(repository);

		CustomerResourceServlet servlet = new CustomerResourceServlet();
		AbstractResourceServlet.RequestParams params = new AbstractResourceServlet.RequestParams(
				"admin",
				"User",
				null,
				"icon_24",
				0,
				0,
				null,
				null,
				"demo");

		AbstractResourceServlet.Resource resource = servlet.createResource(mock(jakarta.servlet.http.HttpServletRequest.class), params);

		assertNotNull(resource);
		assertEquals(null, resource.getContentType());
		assertEquals(null, resource.getFileName());
		verify(repository).findResourceFile("icon_24", "demo", "admin");
	}

	private static HttpServletRequest request(Map<String, String> params, String rangeHeader) {
		HttpServletRequest request = mock(HttpServletRequest.class);
		Map<String, String> mutable = new HashMap<>(params);
		when(request.getParameter(org.mockito.ArgumentMatchers.anyString())).thenAnswer(i -> mutable.get(i.getArgument(0, String.class)));
		when(request.getSession(false)).thenReturn(null);
		when(request.getCookies()).thenReturn(new Cookie[] {new Cookie(AbstractWebContext.CUSTOMER_COOKIE_NAME, "demo")});
		when(request.getHeader("Range")).thenReturn(rangeHeader);

		UxUi uxui = UxUi.newSmartClient(UxUi.DESKTOP_NAME, "Cerulean", "omega");
		RequestUxUiSelectionTestUtil.install(request, org.skyve.web.UserAgentType.desktop, false, uxui);

		return request;
	}

	private static HttpServletResponse responseWithOutput(CapturingServletOutputStream out) {
		HttpServletResponse response = mock(HttpServletResponse.class);
		try {
			when(response.getOutputStream()).thenReturn(out);
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
