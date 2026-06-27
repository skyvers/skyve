package org.skyve.impl.addin;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.List;

import org.junit.Test;
import org.pf4j.PluginManager;
import org.skyve.impl.util.UtilImpl;

@SuppressWarnings("static-method")
public class PF4JAddInManagerTest {
	private static void setPluginManager(PF4JAddInManager manager, PluginManager pluginManager) {
		try {
			Field field = PF4JAddInManager.class.getDeclaredField("plugInManager");
			field.setAccessible(true);
			field.set(manager, pluginManager);
		}
		catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	@Test
	public void getReturnsSingletonInstance() {
		PF4JAddInManager instance = PF4JAddInManager.get();
		assertNotNull(instance);
		assertSame(instance, PF4JAddInManager.get());
	}

	@Test
	public void shutdownWhenPluginManagerIsNullDoesNotThrow() {
		// After construction, plugInManager is null — shutdown must be safe
		PF4JAddInManager instance = PF4JAddInManager.get();
		// This exercises the null-guard in shutdown()
		instance.shutdown();
		assertNotNull(instance);
	}

	@Test
	public void getExtensionReturnsNullWhenNotStarted() {
		// plugInManager is null before startup() is called
		PF4JAddInManager instance = PF4JAddInManager.get();
		setPluginManager(instance, null);
		assertNull(instance.getExtension(Runnable.class));
	}

	@Test
	public void shutdownStopsPluginsWhenPluginManagerPresent() {
		PF4JAddInManager instance = PF4JAddInManager.get();
		PluginManager pluginManager = mock(PluginManager.class);
		setPluginManager(instance, pluginManager);

		instance.shutdown();

		verify(pluginManager).stopPlugins();
		assertNull(instance.getExtension(Runnable.class));
	}

	@Test
	public void getExtensionReturnsFirstRegisteredExtension() {
		PF4JAddInManager instance = PF4JAddInManager.get();
		PluginManager pluginManager = mock(PluginManager.class);
		Runnable first = mock(Runnable.class);
		Runnable second = mock(Runnable.class);
		when(pluginManager.getExtensions(Runnable.class)).thenReturn(List.of(first, second));
		setPluginManager(instance, pluginManager);

		assertSame(first, instance.getExtension(Runnable.class));
	}

	@Test
	public void getExtensionReturnsNullWhenExtensionsEmpty() {
		PF4JAddInManager instance = PF4JAddInManager.get();
		PluginManager pluginManager = mock(PluginManager.class);
		when(pluginManager.getExtensions(Runnable.class)).thenReturn(Collections.emptyList());
		setPluginManager(instance, pluginManager);

		assertNull(instance.getExtension(Runnable.class));
	}

	@Test
	public void getExtensionReturnsNullWhenExtensionsListIsNull() {
		PF4JAddInManager instance = PF4JAddInManager.get();
		PluginManager pluginManager = mock(PluginManager.class);
		when(pluginManager.getExtensions(Runnable.class)).thenReturn(null);
		setPluginManager(instance, pluginManager);

		assertNull(instance.getExtension(Runnable.class));
	}

	@Test
	public void startupAndShutdownWithEmptyAddinsDirectory() throws Exception {
		PF4JAddInManager instance = PF4JAddInManager.get();
		String originalAddinsDirectory = UtilImpl.ADDINS_DIRECTORY;
		Path tempDir = Files.createTempDirectory("pf4j-addins-");
		try {
			UtilImpl.ADDINS_DIRECTORY = tempDir.toString();
			instance.startup();

			assertNotNull(getPluginManager(instance));
			assertNull(instance.getExtension(Runnable.class));
		}
		finally {
			instance.shutdown();
			UtilImpl.ADDINS_DIRECTORY = originalAddinsDirectory;
			setPluginManager(instance, null);
		}
	}

	private static PluginManager getPluginManager(PF4JAddInManager manager) {
		try {
			Field field = PF4JAddInManager.class.getDeclaredField("plugInManager");
			field.setAccessible(true);
			return (PluginManager) field.get(manager);
		}
		catch (Exception e) {
			throw new RuntimeException(e);
		}
	}
}
