package org.skyve.impl.metadata.view.container.form;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class FormTest {

	@Test
	void xmlEnabledConditionIsNullByDefault() {
		assertNull(new Form().getXmlEnabledCondition());
	}

	@Test
	void xmlVisibleConditionIsNullByDefault() {
		assertNull(new Form().getXmlVisibleCondition());
	}

	@Test
	void getPropertiesReturnsNonNullMap() {
		assertNotNull(new Form().getProperties());
	}

	@Test
	void setEnabledConditionDerivesNegatedCondition() {
		Form form = new Form();
		form.setXmlEnabledCondition("myCondition");
		assertEquals("notMyCondition", form.getDisabledCondition());
	}

	@Test
	void setVisibleConditionDerivesNegatedCondition() {
		Form form = new Form();
		form.setXmlVisibleCondition("myVisible");
		assertEquals("notMyVisible", form.getInvisibleCondition());
	}
}
