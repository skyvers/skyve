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
	void xmlVisibleConditionIsNullByDefault() {
		assertNull(new DialogButton().getXmlVisibleCondition());
	}

	@Test
	void xmlEnabledConditionIsNullByDefault() {
		assertNull(new DialogButton().getXmlEnabledCondition());
	}

	@Test
	void getPropertiesReturnsNonNullMap() {
		assertNotNull(new DialogButton().getProperties());
	}

	@Test
	void setVisibleConditionDerivesNegatedCondition() {
		DialogButton button = new DialogButton();
		button.setXmlVisibleCondition("myVisible");
		assertEquals("notMyVisible", button.getInvisibleCondition());
	}

	@Test
	void setEnabledConditionDerivesNegatedCondition() {
		DialogButton button = new DialogButton();
		button.setXmlEnabledCondition("myEnabled");
		assertEquals("notMyEnabled", button.getDisabledCondition());
	}
}
