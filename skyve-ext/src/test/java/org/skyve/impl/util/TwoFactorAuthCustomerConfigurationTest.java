package org.skyve.impl.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.impl.util.TwoFactorAuthCustomerConfiguration.TfaType;

@SuppressWarnings("static-method")
class TwoFactorAuthCustomerConfigurationTest {

	// ---- Constructor and getters ----

	@Test
	void constructorStoresTfaType() {
		TwoFactorAuthCustomerConfiguration cfg = new TwoFactorAuthCustomerConfiguration("email", 60, "Subject", "Body");
		assertEquals("email", cfg.getTfaType());
	}

	@Test
	void constructorStoresTimeoutSeconds() {
		TwoFactorAuthCustomerConfiguration cfg = new TwoFactorAuthCustomerConfiguration("email", 120, "Subject", "Body");
		assertEquals(120, cfg.getTfaTimeOutSeconds());
	}

	@Test
	void constructorStoresEmailSubject() {
		TwoFactorAuthCustomerConfiguration cfg = new TwoFactorAuthCustomerConfiguration("email", 60, "MySubject", "Body");
		assertEquals("MySubject", cfg.getTwoFactorEmailSubject());
	}

	@Test
	void constructorStoresEmailBody() {
		TwoFactorAuthCustomerConfiguration cfg = new TwoFactorAuthCustomerConfiguration("email", 60, "Subject", "MyBody");
		assertEquals("MyBody", cfg.getTwoFactorEmailBody());
	}

	// ---- isTfaEmail ----

	@Test
	void isTfaEmailTrueForEmailType() {
		TwoFactorAuthCustomerConfiguration cfg = new TwoFactorAuthCustomerConfiguration("email", 60, "S", "B");
		assertTrue(cfg.isTfaEmail());
	}

	@Test
	void isTfaEmailTrueForEmailUpperCase() {
		TwoFactorAuthCustomerConfiguration cfg = new TwoFactorAuthCustomerConfiguration("EMAIL", 60, "S", "B");
		assertTrue(cfg.isTfaEmail());
	}

	@Test
	void isTfaEmailFalseForOffType() {
		TwoFactorAuthCustomerConfiguration cfg = new TwoFactorAuthCustomerConfiguration("off", 0, "S", "B");
		assertFalse(cfg.isTfaEmail());
	}

	// ---- isTfaOff ----

	@Test
	void isTfaOffTrueForOffType() {
		TwoFactorAuthCustomerConfiguration cfg = new TwoFactorAuthCustomerConfiguration("off", 0, "S", "B");
		assertTrue(cfg.isTfaOff());
	}

	@Test
	void isTfaOffFalseForEmailType() {
		TwoFactorAuthCustomerConfiguration cfg = new TwoFactorAuthCustomerConfiguration("email", 60, "S", "B");
		assertFalse(cfg.isTfaOff());
	}

	// ---- getParsedTfaType ----

	@Test
	void parsedTfaTypeIsEmailForEmailString() {
		TwoFactorAuthCustomerConfiguration cfg = new TwoFactorAuthCustomerConfiguration("email", 60, "S", "B");
		assertEquals(TfaType.EMAIL, cfg.getParsedTfaType());
	}

	@Test
	void parsedTfaTypeIsOffForOffString() {
		TwoFactorAuthCustomerConfiguration cfg = new TwoFactorAuthCustomerConfiguration("off", 0, "S", "B");
		assertEquals(TfaType.OFF, cfg.getParsedTfaType());
	}

	@Test
	void parsedTfaTypeIsUnsupportedForUnknownString() {
		TwoFactorAuthCustomerConfiguration cfg = new TwoFactorAuthCustomerConfiguration("sms", 60, "S", "B");
		assertEquals(TfaType.UNSUPPORTED, cfg.getParsedTfaType());
	}

	@Test
	void parsedTfaTypeIsUnsupportedForNullString() {
		TwoFactorAuthCustomerConfiguration cfg = new TwoFactorAuthCustomerConfiguration(null, 0, "S", "B");
		assertEquals(TfaType.UNSUPPORTED, cfg.getParsedTfaType());
	}
}
