package org.skyve.impl.web;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.metadata.repository.ProvidedRepository;


@SuppressWarnings("static-method")
	class CustomerResourceServletTest {

	private ProvidedRepository originalRepository;

	@BeforeEach
	void setUp() {
		originalRepository = ProvidedRepositoryFactory.get();
	}

	@AfterEach
	void tearDown() {
		ProvidedRepositoryFactory.set(originalRepository);
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
		verify(repository).findResourceFile("logo.txt", "demo", "admin");
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
}