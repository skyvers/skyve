package org.skyve.impl.generate.client.react;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.concurrent.atomic.AtomicReference;

import org.junit.jupiter.api.Test;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;

class ReactEditViewTest {
	@Test
	@SuppressWarnings("static-method")
	void setViewsReadsEditAndCreateViewsFromDocument() throws Exception {
		Path projectDir = Files.createTempDirectory("react-edit-set-views");
		ReactGenerator generator = new ReactGenerator("desktop", projectDir.toString());
		ReactEditView view = new ReactEditView(generator, "admin", "User");

		Module module = mock(Module.class);
		Document document = mock(Document.class);
		View edit = mock(View.class);
		View create = mock(View.class);
		Customer customer = mock(Customer.class);

		when(document.getView("desktop", customer, "edit")).thenReturn(edit);
		when(document.getView("desktop", customer, "create")).thenReturn(create);

		AtomicReference<Object> previous = setThreadLocalPersistence(mockPersistence(customer));
		try {
			view.setViews(module, document);
		} finally {
			restoreThreadLocalPersistence(previous.get());
		}

		assertEquals(module, getField(view, "module"));
		assertEquals(document, getField(view, "document"));
		assertEquals(edit, getField(view, "editView"));
		assertEquals(create, getField(view, "createView"));
	}

	@Test
	@SuppressWarnings("static-method")
	void createWithNoViewsWritesBaseEditComponent() throws Exception {
		Path projectDir = Files.createTempDirectory("react-edit-create");
		ReactGenerator generator = new ReactGenerator("desktop", projectDir.toString());
		generator.srcSkyveViewsPath.mkdirs();

		ReactEditView view = new ReactEditView(generator, "admin", "User");
		setField(view, "module", mock(Module.class));
		Document document = mock(Document.class);
		when(document.getName()).thenReturn("User");
		setField(view, "document", document);
		setField(view, "editView", null);
		setField(view, "createView", null);

		AtomicReference<Object> previous = setThreadLocalPersistence(mockPersistence(mock(Customer.class)));
		try {
			view.create();
		} finally {
			restoreThreadLocalPersistence(previous.get());
		}

		Path generated = projectDir.resolve("src/skyve/views/admin/User.js");
		assertTrue(Files.exists(generated));
		String content = Files.readString(generated);
		assertTrue(content.contains("export class adminUser extends View"));
		assertTrue(content.contains("this.edit('admin', 'User', this.props.match.params.bizId)"));
		assertTrue(content.contains("render()"));
		assertFalse(content.contains("if (this.state.created)"));
		assertNotNull(content);
	}

	private static AbstractPersistence mockPersistence(Customer customer) {
		User user = mock(User.class);
		when(user.getCustomer()).thenReturn(customer);
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		when(persistence.getUser()).thenReturn(user);
		return persistence;
	}

	@SuppressWarnings("unchecked")
	private static AtomicReference<Object> setThreadLocalPersistence(AbstractPersistence persistence) throws Exception {
		Field threadLocalField = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		threadLocalField.setAccessible(true);
		ThreadLocal<?> threadLocal = (ThreadLocal<?>) threadLocalField.get(null);
		AtomicReference<Object> previous = new AtomicReference<>(threadLocal.get());
		((ThreadLocal<Object>) threadLocal).set(persistence);
		return previous;
	}

	@SuppressWarnings("unchecked")
	private static void restoreThreadLocalPersistence(Object previous) throws Exception {
		Field threadLocalField = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		threadLocalField.setAccessible(true);
		ThreadLocal<?> threadLocal = (ThreadLocal<?>) threadLocalField.get(null);
		if (previous == null) {
			threadLocal.remove();
		} else {
			((ThreadLocal<Object>) threadLocal).set(previous);
		}
	}

	private static Object getField(Object instance, String fieldName) throws Exception {
		Field field = instance.getClass().getDeclaredField(fieldName);
		field.setAccessible(true);
		return field.get(instance);
	}

	private static void setField(Object instance, String fieldName, Object value) throws Exception {
		Field field = instance.getClass().getDeclaredField(fieldName);
		field.setAccessible(true);
		field.set(instance, value);
	}
}