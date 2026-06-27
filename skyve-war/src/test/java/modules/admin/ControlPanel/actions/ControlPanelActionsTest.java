package modules.admin.ControlPanel.actions;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.controller.ServerSideActionResult;

import modules.admin.ControlPanel.ControlPanelExtension;

/**
 * Tests for ControlPanel EvictSelectedCache action.
 */
@SuppressWarnings("static-method")
class ControlPanelActionsTest {

	@Test
	void evictSelectedCacheWithNullCacheReturnsBean() throws Exception {
		EvictSelectedCache action = new EvictSelectedCache();
		ControlPanelExtension bean = new ControlPanelExtension();
		// selectedCache is null → skip caching block, return bean
		ServerSideActionResult<ControlPanelExtension> result = action.execute(bean, null);

		assertNotNull(result);
		assertSame(bean, result.getBean());
	}
}
