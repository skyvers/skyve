package org.skyve.impl.generate.client.react;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.Field;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.concurrent.atomic.AtomicReference;

import org.junit.jupiter.api.Test;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.user.User;

class ReactGeneratorTest {
	@Test
	@SuppressWarnings("static-method")
	void constructorThrowsWhenProjectFolderDoesNotExist() {
		String missingPath = "/tmp/skyve-react-generator-missing-" + System.nanoTime();

		IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
				() -> new ReactGenerator("desktop", missingPath));

		assertTrue(ex.getMessage().contains("does not exist"));
	}

	@Test
	@SuppressWarnings("static-method")
	void constructorInitializesGeneratorPaths() throws Exception {
		Path projectDir = Files.createTempDirectory("react-generator-project");

		ReactGenerator generator = new ReactGenerator("desktop", projectDir.toString());

		assertEquals("desktop", generator.uxui);
		assertEquals(projectDir.toString(), generator.projectFolderPath);
		assertEquals(projectDir.resolve("src/skyve").toFile(), generator.srcSkyvePath);
		assertEquals(projectDir.resolve("src/skyve/views").toFile(), generator.srcSkyveViewsPath);
		assertNotNull(generator.components);
		assertTrue(generator.components.isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	void generateCreatesRouterAndRunsComponentGeneration() throws Exception {
		Path projectDir = Files.createTempDirectory("react-generator-generate");
		ReactGenerator generator = new ReactGenerator("desktop", projectDir.toString());
		generator.components.add(new StubReactComponent(generator, "admin", "UserGenerated"));

		Customer customer = org.mockito.Mockito.mock(Customer.class);
		org.mockito.Mockito.when(customer.getModules()).thenReturn(Collections.emptyList());
		User user = org.mockito.Mockito.mock(User.class);
		org.mockito.Mockito.when(user.getCustomer()).thenReturn(customer);
		AbstractPersistence persistence = org.mockito.Mockito.mock(AbstractPersistence.class);
		org.mockito.Mockito.when(persistence.getUser()).thenReturn(user);

		AtomicReference<Object> previous = setThreadLocalPersistence(persistence);
		try {
			generator.generate();
		} finally {
			restoreThreadLocalPersistence(previous.get());
		}

		assertTrue(projectDir.resolve("src/skyve/Router.js").toFile().exists());
		assertTrue(projectDir.resolve("src/skyve/views/admin/UserGenerated.js").toFile().exists());
	}

	@Test
	@SuppressWarnings("static-method")
	void mainUsesConfiguredPathAndThrowsWhenUnavailable() {
		assertThrows(IllegalArgumentException.class, () -> ReactGenerator.main(new String[0]));
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

	private static final class StubReactComponent extends ReactComponent {
		private StubReactComponent(ReactGenerator generator, String moduleName, String componentName) {
			super(generator, moduleName, componentName);
		}

		@Override
		protected void create(FileWriter fw) throws IOException {
			fw.write("export const generated = true;\n");
		}
	}
}