package org.skyve.impl.metadata.view.widget;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.view.Action.ActionShow;

@SuppressWarnings("static-method")
class ButtonTest {

	@Test
	void showsLabelByDefaultReturnsFalse() {
		assertFalse(new Button().showsLabelByDefault());
	}

	@Test
	void getPropertiesReturnsNonNullMap() {
		assertNotNull(new Button().getProperties());
	}

	@Test
	void settersPopulateButtonConfiguration() {
		Button button = new Button();
		button.setActionName("  save  ");
		button.setPixelWidth(Integer.valueOf(120));
		button.setPixelHeight(Integer.valueOf(35));
		button.setMinPixelHeight(Integer.valueOf(20));
		button.setMaxPixelHeight(Integer.valueOf(50));
		button.setShow(ActionShow.both);

		assertEquals("save", button.getActionName());
		assertEquals(Integer.valueOf(120), button.getPixelWidth());
		assertEquals(Integer.valueOf(35), button.getPixelHeight());
		assertEquals(Integer.valueOf(20), button.getMinPixelHeight());
		assertEquals(Integer.valueOf(50), button.getMaxPixelHeight());
		assertEquals(ActionShow.both, button.getShow());
	}
}
