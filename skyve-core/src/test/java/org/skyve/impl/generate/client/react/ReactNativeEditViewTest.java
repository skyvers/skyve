package org.skyve.impl.generate.client.react;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.FileWriter;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.concurrent.atomic.AtomicReference;

import org.junit.jupiter.api.Test;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;

class ReactNativeEditViewTest {
	@Test
	@SuppressWarnings("static-method")
	void createWithoutViewsWritesBaseFunction() throws Exception {
		Path projectDir = Files.createTempDirectory("react-native-edit-empty");
		ReactGenerator generator = new ReactGenerator("desktop", projectDir.toString());
		generator.srcSkyveViewsPath.mkdirs();

		ReactNativeEditView view = new ReactNativeEditView(generator, "admin", "User");

		AtomicReference<Object> previous = setThreadLocalPersistence(mockPersistence());
		try {
			view.create();
		} finally {
			restoreThreadLocalPersistence(previous.get());
		}

		String content = Files.readString(projectDir.resolve("src/skyve/views/admin/User.js"));
		assertTrue(content.contains("export default function adminUser(props)"));
		assertTrue(content.contains("import { useNavigation } from '@react-navigation/native';"));
	}

	@Test
	@SuppressWarnings("static-method")
	void addNavigationOptionsWritesTitleFromView() throws Exception {
		Path outputFile = Files.createTempFile("react-native-nav-options", ".txt");
		View view = mock(View.class);
		when(view.getLocalisedTitle()).thenReturn("Mail Log");

		Method method = ReactNativeEditView.class.getDeclaredMethod("addNavigationOptions", FileWriter.class, View.class,
				String.class);
		method.setAccessible(true);

		try (FileWriter fw = new FileWriter(outputFile.toFile())) {
			method.invoke(null, fw, view, "\t");
		}

		String content = Files.readString(outputFile);
		assertTrue(content.contains("const navigation = useNavigation();"));
		assertTrue(content.contains("title: 'Mail Log'"));
	}

	private static AbstractPersistence mockPersistence() {
		Customer customer = mock(Customer.class);
		when(customer.getModules()).thenReturn(Collections.emptyList());
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
}