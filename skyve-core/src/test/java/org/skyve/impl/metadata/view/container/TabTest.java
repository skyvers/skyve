package org.skyve.impl.metadata.view.container;

import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class TabTest {

	@Test
	void xmlEnabledConditionIsNullByDefault() {
		assertNull(new Tab().getXmlEnabledCondition());
	}

	@Test
	void xmlVisibleConditionIsNullByDefault() {
		assertNull(new Tab().getXmlVisibleCondition());
	}
}
