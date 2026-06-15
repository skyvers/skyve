package org.skyve.toolchain.config;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

class GenerateDomainConfigTest {

	@Test
	@SuppressWarnings("static-method")
	void defaultsAreCorrect() {
		GenerateDomainConfig config = new GenerateDomainConfig();
		assertFalse(config.isDebug());
		assertFalse(config.isMultiTenant());
		assertEquals("H2", config.getDialect());
		assertEquals("", config.getExcludedModules());
		assertNull(config.getCustomisationsClass());
	}

	@Test
	@SuppressWarnings("static-method")
	void settersAndGettersRoundTrip() {
		GenerateDomainConfig config = new GenerateDomainConfig();
		config.setDebug(true);
		assertTrue(config.isDebug());

		config.setMultiTenant(true);
		assertTrue(config.isMultiTenant());

		config.setDialect("H2_NO_SEQUENCE");
		assertEquals("H2_NO_SEQUENCE", config.getDialect());

		config.setExcludedModules("admin,test");
		assertEquals("admin,test", config.getExcludedModules());

		config.setCustomisationsClass("com.example.Customisations");
		assertEquals("com.example.Customisations", config.getCustomisationsClass());
	}

	@Test
	@SuppressWarnings("static-method")
	void setCustomisationsClassTreatsBlankAsNull() {
		GenerateDomainConfig config = new GenerateDomainConfig();
		config.setCustomisationsClass("   ");
		assertNull(config.getCustomisationsClass());
	}
}
