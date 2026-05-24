package org.skyve.toolchain.config;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

class GenerateDefaultQueriesConfigTest {

	@Test
	@SuppressWarnings("static-method")
	void defaultsAreCorrect() {
		GenerateDefaultQueriesConfig config = new GenerateDefaultQueriesConfig();
		assertNull(config.getCustomer());
		assertNull(config.getModule());
		assertFalse(config.isIncludeAssociationBizKeys());
	}

	@Test
	@SuppressWarnings("static-method")
	void settersAndGettersRoundTrip() {
		GenerateDefaultQueriesConfig config = new GenerateDefaultQueriesConfig();

		config.setCustomer("myCustomer");
		assertEquals("myCustomer", config.getCustomer());

		config.setModule("finance");
		assertEquals("finance", config.getModule());

		config.setIncludeAssociationBizKeys(true);
		assertTrue(config.isIncludeAssociationBizKeys());
	}
}
