package org.skyve.impl.web.faces.views;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.HashMap;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.primefaces.event.FileUploadEvent;
import org.primefaces.model.file.UploadedFile;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.user.User;

@SuppressWarnings({"static-method", "boxing"})
class FacesContentUtilTest {
	@AfterEach
	void tearDown() {
		clearThreadPersistence();
	}

	@Test
	void handleFileUploadByteArrayThrowsWhenContentAccessDenied() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Bean bean = mock(Bean.class);
		bindPersistenceForUser(user);

		when(user.getCustomer()).thenReturn(customer);
		when(user.getName()).thenReturn("u1");
		when(bean.getBizId()).thenReturn("b1");
		when(bean.getBizModule()).thenReturn("admin");
		when(bean.getBizDocument()).thenReturn("Contact");
		when(bean.getBizCustomer()).thenReturn("bizCustomer");
		when(bean.getBizDataGroupId()).thenReturn("dg1");
		when(bean.getBizUserId()).thenReturn("bu1");
		when(user.canAccessContent("b1", "admin", "Contact", "bizCustomer", "dg1", "bu1", "contentId"))
				.thenReturn(Boolean.FALSE);

		assertThrows(IllegalArgumentException.class,
				() -> FacesContentUtil.handleFileUpload(new byte[] {1, 2}, "text/plain", bean, "contentId"));
	}

	@Test
	void handleFileUploadEventOverloadDelegatesAndChecksAccess() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Bean bean = mock(Bean.class);
		FileUploadEvent event = mock(FileUploadEvent.class);
		UploadedFile file = mock(UploadedFile.class);
		bindPersistenceForUser(user);

		when(event.getFile()).thenReturn(file);
		when(file.getFileName()).thenReturn("/tmp/test.txt");
		when(file.getContent()).thenReturn(new byte[] {3, 4});
		when(file.getContentType()).thenReturn("text/plain");

		when(user.getCustomer()).thenReturn(customer);
		when(user.getName()).thenReturn("u2");
		when(bean.getBizId()).thenReturn("b2");
		when(bean.getBizModule()).thenReturn("admin");
		when(bean.getBizDocument()).thenReturn("Contact");
		when(bean.getBizCustomer()).thenReturn("bizCustomer");
		when(bean.getBizDataGroupId()).thenReturn("dg2");
		when(bean.getBizUserId()).thenReturn("bu2");
		when(user.canAccessContent("b2", "admin", "Contact", "bizCustomer", "dg2", "bu2", "contentId"))
				.thenReturn(Boolean.FALSE);

		assertThrows(IllegalArgumentException.class,
				() -> FacesContentUtil.handleFileUpload(event, bean, "contentId"));

		verify(event).getFile();
	}

	@Test
	void handleFileUploadUsesCompoundBindingOwnerForSecurityCheck() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Bean child = mock(Bean.class);
		HashMap<String, Object> values = new HashMap<>();
		DynamicBean root = new DynamicBean("admin", "Root", values);
		bindPersistenceForUser(user);

		values.put("child", child);
		when(user.getCustomer()).thenReturn(customer);
		when(user.getName()).thenReturn("u3");
		when(child.getBizId()).thenReturn("c1");
		when(child.getBizModule()).thenReturn("admin");
		when(child.getBizDocument()).thenReturn("Child");
		when(child.getBizCustomer()).thenReturn("bizCustomer");
		when(child.getBizDataGroupId()).thenReturn("dg3");
		when(child.getBizUserId()).thenReturn("bu3");
		when(user.canAccessContent("c1", "admin", "Child", "bizCustomer", "dg3", "bu3", "contentId"))
				.thenReturn(Boolean.FALSE);

		assertThrows(IllegalArgumentException.class,
				() -> FacesContentUtil.handleFileUpload("test.txt", new byte[] {9}, "text/plain", root, "child.contentId"));

		verify(user).canAccessContent("c1", "admin", "Child", "bizCustomer", "dg3", "bu3", "contentId");
	}

	private static void bindPersistenceForUser(User user) {
		AbstractPersistence persistence = mock(AbstractPersistence.class, CALLS_REAL_METHODS);
		persistence.setUser(user);
		persistence.setForThread();
	}

	@SuppressWarnings("unchecked")
	private static void clearThreadPersistence() {
		try {
			Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
			field.setAccessible(true);
			ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
			threadLocal.remove();
		}
		catch (Exception e) {
			throw new IllegalStateException("Unable to clear thread local persistence", e);
		}
	}
}
