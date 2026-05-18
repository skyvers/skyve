package org.skyve.impl.addin;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;

import org.junit.Test;

@SuppressWarnings("static-method")
public class PF4JAddInManagerTest {

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
	}

	@Test
	public void getExtensionReturnsNullWhenNotStarted() {
		// plugInManager is null before startup() is called
		PF4JAddInManager instance = PF4JAddInManager.get();
		assertNull(instance.getExtension(Runnable.class));
	}
}
