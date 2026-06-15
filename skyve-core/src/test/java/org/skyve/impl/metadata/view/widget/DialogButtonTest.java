package org.skyve.impl.metadata.view.widget;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class DialogButtonTest {

	@Test
	void showsLabelByDefaultReturnsFalse() {
		assertFalse(new DialogButton().showsLabelByDefault());
	}

	@Test
	void getLocalisedDisplayNameReturnsI18nOfDisplayName() {
		DialogButton button = new DialogButton();
		button.setDisplayName("OK");
		assertEquals("OK", button.getLocalisedDisplayName());
	}

	@Test
	void jaxbHelperGetVisibleConditionNameReturnsNull() {
		assertNull(new DialogButton().getVisibleConditionName());
	}

	@Test
	void jaxbHelperGetEnabledConditionNameReturnsNull() {
		assertNull(new DialogButton().getEnabledConditionName());
	}

	@Test
	void getPropertiesReturnsNonNullMap() {
		assertNotNull(new DialogButton().getProperties());
	}

	@Test
	void setVisibleConditionNameStoresNegatedCondition() {
		DialogButton button = new DialogButton();
		button.setVisibleConditionName("myVisible");
		assertNotNull(button.getInvisibleConditionName());
	}

	@Test
	void setEnabledConditionNameStoresNegatedCondition() {
		DialogButton button = new DialogButton();
		button.setEnabledConditionName("myEnabled");
		assertNotNull(button.getDisabledConditionName());
	}
}
