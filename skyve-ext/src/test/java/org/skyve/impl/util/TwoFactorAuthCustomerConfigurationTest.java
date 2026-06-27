package org.skyve.impl.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.skyve.impl.util.TwoFactorAuthCustomerConfiguration.TfaType;

@SuppressWarnings("static-method")
public class TwoFactorAuthCustomerConfigurationTest {

	// ---- Constructor and getters ----

	@Test
	public void constructorStoresTfaType() {
		TwoFactorAuthCustomerConfiguration cfg = new TwoFactorAuthCustomerConfiguration("email", 60, "Subject", "Body");
		assertEquals("email", cfg.getTfaType());
	}

	@Test
	public void constructorStoresTimeoutSeconds() {
		TwoFactorAuthCustomerConfiguration cfg = new TwoFactorAuthCustomerConfiguration("email", 120, "Subject", "Body");
		assertEquals(120, cfg.getTfaTimeOutSeconds());
	}

	@Test
	public void constructorStoresEmailSubject() {
		TwoFactorAuthCustomerConfiguration cfg = new TwoFactorAuthCustomerConfiguration("email", 60, "MySubject", "Body");
		assertEquals("MySubject", cfg.getTwoFactorEmailSubject());
	}

	@Test
	public void constructorStoresEmailBody() {
		TwoFactorAuthCustomerConfiguration cfg = new TwoFactorAuthCustomerConfiguration("email", 60, "Subject", "MyBody");
		assertEquals("MyBody", cfg.getTwoFactorEmailBody());
	}

	// ---- isTfaEmail ----

	@Test
	public void isTfaEmailTrueForEmailType() {
		TwoFactorAuthCustomerConfiguration cfg = new TwoFactorAuthCustomerConfiguration("email", 60, "S", "B");
		assertTrue(cfg.isTfaEmail());
	}

	@Test
	public void isTfaEmailTrueForEmailUpperCase() {
		TwoFactorAuthCustomerConfiguration cfg = new TwoFactorAuthCustomerConfiguration("EMAIL", 60, "S", "B");
		assertTrue(cfg.isTfaEmail());
	}

	@Test
	public void isTfaEmailFalseForOffType() {
		TwoFactorAuthCustomerConfiguration cfg = new TwoFactorAuthCustomerConfiguration("off", 0, "S", "B");
		assertFalse(cfg.isTfaEmail());
	}

	// ---- isTfaOff ----

	@Test
	public void isTfaOffTrueForOffType() {
		TwoFactorAuthCustomerConfiguration cfg = new TwoFactorAuthCustomerConfiguration("off", 0, "S", "B");
		assertTrue(cfg.isTfaOff());
	}

	@Test
	public void isTfaOffFalseForEmailType() {
		TwoFactorAuthCustomerConfiguration cfg = new TwoFactorAuthCustomerConfiguration("email", 60, "S", "B");
		assertFalse(cfg.isTfaOff());
	}

	// ---- getParsedTfaType ----

	@Test
	public void parsedTfaTypeIsEmailForEmailString() {
		TwoFactorAuthCustomerConfiguration cfg = new TwoFactorAuthCustomerConfiguration("email", 60, "S", "B");
		assertEquals(TfaType.EMAIL, cfg.getParsedTfaType());
	}

	@Test
	public void parsedTfaTypeIsOffForOffString() {
		TwoFactorAuthCustomerConfiguration cfg = new TwoFactorAuthCustomerConfiguration("off", 0, "S", "B");
		assertEquals(TfaType.OFF, cfg.getParsedTfaType());
	}

	@Test
	public void parsedTfaTypeIsUnsupportedForUnknownString() {
		TwoFactorAuthCustomerConfiguration cfg = new TwoFactorAuthCustomerConfiguration("sms", 60, "S", "B");
		assertEquals(TfaType.UNSUPPORTED, cfg.getParsedTfaType());
	}

	@Test
	public void parsedTfaTypeIsUnsupportedForNullString() {
		TwoFactorAuthCustomerConfiguration cfg = new TwoFactorAuthCustomerConfiguration(null, 0, "S", "B");
		assertEquals(TfaType.UNSUPPORTED, cfg.getParsedTfaType());
	}
}
