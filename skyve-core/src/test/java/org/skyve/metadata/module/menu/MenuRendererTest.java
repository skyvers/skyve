package org.skyve.metadata.module.menu;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class MenuRendererTest {

	private static class NoOpMenuRenderer extends MenuRenderer {
		NoOpMenuRenderer() {
			super("desktop", null);
		}
	}

	@Test
	void defaultConstructorCreatesInstance() {
		assertNotNull(new NoOpMenuRenderer());
	}

	@Test
	void renderModuleMenuDoesNotThrow() {
		assertDoesNotThrow(() -> new NoOpMenuRenderer().renderModuleMenu(null, null, false));
	}

	@Test
	void renderMenuRootDoesNotThrow() {
		assertDoesNotThrow(() -> new NoOpMenuRenderer().renderMenuRoot(null, null));
	}

	@Test
	void renderMenuGroupDoesNotThrow() {
		assertDoesNotThrow(() -> new NoOpMenuRenderer().renderMenuGroup(null, null));
	}

	@Test
	void renderTreeItemDoesNotThrow() {
		assertDoesNotThrow(() -> new NoOpMenuRenderer().renderTreeItem(null, null, null, null, null, null, null));
	}

	@Test
	void renderListItemDoesNotThrow() {
		assertDoesNotThrow(() -> new NoOpMenuRenderer().renderListItem(null, null, null, null, null, null, null));
	}

	@Test
	void renderCalendarItemDoesNotThrow() {
		assertDoesNotThrow(() -> new NoOpMenuRenderer().renderCalendarItem(null, null, null, null, null, null, null));
	}

	@Test
	void renderMapItemDoesNotThrow() {
		assertDoesNotThrow(() -> new NoOpMenuRenderer().renderMapItem(null, null, null, null, null, null, null));
	}

	@Test
	void renderEditItemDoesNotThrow() {
		assertDoesNotThrow(() -> new NoOpMenuRenderer().renderEditItem(null, null, null, null, null, null));
	}

	@Test
	void renderLinkItemDoesNotThrow() {
		assertDoesNotThrow(() -> new NoOpMenuRenderer().renderLinkItem(null, null, false, null));
	}

	@Test
	void renderedMenuGroupDoesNotThrow() {
		assertDoesNotThrow(() -> new NoOpMenuRenderer().renderedMenuGroup(null, null));
	}

	@Test
	void renderedMenuRootDoesNotThrow() {
		assertDoesNotThrow(() -> new NoOpMenuRenderer().renderedMenuRoot(null, null));
	}

	@Test
	void renderedModuleMenuDoesNotThrow() {
		assertDoesNotThrow(() -> new NoOpMenuRenderer().renderedModuleMenu(null, null, false));
	}
}
