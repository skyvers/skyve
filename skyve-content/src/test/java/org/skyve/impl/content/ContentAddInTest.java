package org.skyve.impl.content;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.pf4j.DefaultPluginDescriptor;
import org.pf4j.DefaultPluginManager;
import org.pf4j.PluginWrapper;

@SuppressWarnings({"static-method", "deprecation"})
class ContentAddInTest {
	private Path tempPluginsRoot;

	@AfterEach
	void cleanupTempPluginsRoot() throws Exception {
		if (tempPluginsRoot != null) {
			try (var stream = Files.walk(tempPluginsRoot)) {
				stream.sorted(Comparator.reverseOrder()).forEach(path -> {
					try {
						Files.deleteIfExists(path);
					}
					catch (Exception e) {
						throw new IllegalStateException(e);
					}
				});
			}
		}
	}

	@Test
	void testConstructWithNullWrapperThrows() {
		IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> new ContentAddIn(null));
		assertTrue(ex.getMessage().contains("Wrapper cannot be null"));
	}

	@Test
	void testConstructWithWrapper() throws Exception {
		tempPluginsRoot = Files.createTempDirectory("skyve-content-addin-");
		DefaultPluginManager pluginManager = new DefaultPluginManager(tempPluginsRoot);
		DefaultPluginDescriptor descriptor = new DefaultPluginDescriptor("contentAddInTest", "test plugin", null, "1.0.0", null, "test", null);
		PluginWrapper wrapper = new PluginWrapper(pluginManager, descriptor, tempPluginsRoot, getClass().getClassLoader());

		ContentAddIn addIn = new ContentAddIn(wrapper);

		assertSame(wrapper, addIn.getWrapper());
	}
}