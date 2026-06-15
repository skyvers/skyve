package org.skyve.toolchain.config;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

class GenerateEditViewConfigTest {

	@Test
	@SuppressWarnings("static-method")
	void defaultsAreCorrect() {
		GenerateEditViewConfig config = new GenerateEditViewConfig();
		assertNull(config.getCustomer());
		assertNull(config.getModule());
		assertNull(config.getDocument());
		assertFalse(config.isCustomerOverriden());
		assertNull(config.getOverridenViewName());
	}

	@Test
	@SuppressWarnings("static-method")
	void settersAndGettersRoundTrip() {
		GenerateEditViewConfig config = new GenerateEditViewConfig();

		config.setCustomer("testCustomer");
		assertEquals("testCustomer", config.getCustomer());

		config.setModule("admin");
		assertEquals("admin", config.getModule());

		config.setDocument("User");
		assertEquals("User", config.getDocument());

		config.setCustomerOverriden(true);
		assertTrue(config.isCustomerOverriden());

		config.setOverridenViewName("customEdit.xml");
		assertEquals("customEdit.xml", config.getOverridenViewName());
	}
}
