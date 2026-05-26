package modules.admin.SelfRegistration;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
public class SelfRegistrationExtensionTest {

	@Test
	void confirmEmailInvalidWithNullConfirmEmailReturnsFalse() {
		SelfRegistrationExtension ext = new SelfRegistrationExtension();
		assertFalse(ext.confirmEmailInvalid());
	}

	@Test
	void confirmEmailInvalidWithValidEmailReturnsFalse() {
		SelfRegistrationExtension ext = new SelfRegistrationExtension();
		ext.setConfirmEmail("user@example.com");
		assertFalse(ext.confirmEmailInvalid());
	}

	@Test
	void confirmEmailInvalidWithInvalidEmailReturnsTrue() {
		SelfRegistrationExtension ext = new SelfRegistrationExtension();
		ext.setConfirmEmail("not-an-email");
		assertTrue(ext.confirmEmailInvalid());
	}

	@Test
	void emailInvalidWithNullUserReturnsFalse() {
		SelfRegistrationExtension ext = new SelfRegistrationExtension();
		assertFalse(ext.emailInvalid());
	}

	@Test
	void getSiteKeyWithNoRecaptchaConfiguredReturnsNull() {
		SelfRegistrationExtension ext = new SelfRegistrationExtension();
		// UtilImpl.GOOGLE_RECAPTCHA_SITE_KEY and CLOUDFLARE_TURNSTILE_SITE_KEY are null in test env
		assertNull(ext.getSiteKey());
	}

	@Test
	void validateConfirmPasswordWithNullUserDoesNotThrow() {
		SelfRegistrationExtension ext = new SelfRegistrationExtension();
		assertDoesNotThrow(() -> ext.validateConfirmPassword());
	}

	@Test
	void validateConfirmEmailWithNullUserDoesNotThrow() {
		SelfRegistrationExtension ext = new SelfRegistrationExtension();
		assertDoesNotThrow(() -> ext.validateConfirmEmail());
	}
}
