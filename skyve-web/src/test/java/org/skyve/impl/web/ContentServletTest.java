package org.skyve.impl.web;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.util.Date;

import org.junit.jupiter.api.AfterEach;
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

import jakarta.servlet.http.HttpServletResponse;

@SuppressWarnings("static-method")
class ContentServletTest {
	@AfterEach
	void tearDown() {
		clearThreadPersistence();
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
		verify(response).setHeader("Accept-Ranges", "bytes");
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
}
