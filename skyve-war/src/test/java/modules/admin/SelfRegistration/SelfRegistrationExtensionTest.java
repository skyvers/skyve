package modules.admin.SelfRegistration;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.util.UtilImpl;

import modules.admin.Contact.ContactExtension;
import modules.admin.User.UserExtension;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
class SelfRegistrationExtensionTest extends AbstractH2Test {
	@AfterEach
	void clearCaptchaKeys() {
		UtilImpl.GOOGLE_RECAPTCHA_SITE_KEY = null;
		UtilImpl.CLOUDFLARE_TURNSTILE_SITE_KEY = null;
	}

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
	void emailInvalidWithInvalidContactEmailReturnsTrue() {
		SelfRegistrationExtension ext = registrationWithEmail("not-an-email");
		assertTrue(ext.emailInvalid());
	}

	@Test
	void emailInvalidWithValidContactEmailReturnsFalse() {
		SelfRegistrationExtension ext = registrationWithEmail("user@example.com");
		assertFalse(ext.emailInvalid());
	}

	@Test
	void getSiteKeyWithNoRecaptchaConfiguredReturnsNull() {
		SelfRegistrationExtension ext = new SelfRegistrationExtension();
		// UtilImpl.GOOGLE_RECAPTCHA_SITE_KEY and CLOUDFLARE_TURNSTILE_SITE_KEY are null in test env
		assertNull(ext.getSiteKey());
	}

	@Test
	void getSiteKeyReturnsGoogleRecaptchaKeyWhenConfigured() {
		UtilImpl.GOOGLE_RECAPTCHA_SITE_KEY = "google-key";
		SelfRegistrationExtension ext = new SelfRegistrationExtension();

		assertEquals("google-key", ext.getSiteKey());
	}

	@Test
	void getSiteKeyReturnsCloudflareTurnstileKeyWhenConfigured() {
		UtilImpl.CLOUDFLARE_TURNSTILE_SITE_KEY = "cloudflare-key";
		SelfRegistrationExtension ext = new SelfRegistrationExtension();

		assertEquals("cloudflare-key", ext.getSiteKey());
	}

	@Test
	void validateConfirmPasswordWithNullUserDoesNotThrow() {
		SelfRegistrationExtension ext = new SelfRegistrationExtension();
		assertDoesNotThrow(ext::validateConfirmPassword);
	}

	@Test
	void validateConfirmPasswordThrowsWhenPasswordAndConfirmPasswordMissing() {
		SelfRegistrationExtension ext = new SelfRegistrationExtension();
		ext.setUser(new UserExtension());

		ValidationException exception = assertThrows(ValidationException.class, ext::validateConfirmPassword);

		assertEquals(2, exception.getMessages().size());
		assertEquals(SelfRegistrationExtension.PASSWORD_REQUIRED, exception.getMessages().get(0).getText());
		assertEquals(SelfRegistrationExtension.CONFIRM_PASSWORD_REQUIRED, exception.getMessages().get(1).getText());
	}

	@Test
	void validateConfirmPasswordThrowsWhenPasswordsMismatch() {
		SelfRegistrationExtension ext = new SelfRegistrationExtension();
		UserExtension user = new UserExtension();
		user.setPassword("Password1!");
		ext.setUser(user);
		ext.setConfirmPassword("OtherPassword1!");

		ValidationException exception = assertThrows(ValidationException.class, ext::validateConfirmPassword);

		assertEquals(SelfRegistrationExtension.PASSWORD_MISMATCH, exception.getMessages().get(0).getText());
	}

	@Test
	void validateConfirmEmailWithNullUserDoesNotThrow() {
		SelfRegistrationExtension ext = new SelfRegistrationExtension();
		assertDoesNotThrow(ext::validateConfirmEmail);
	}

	@Test
	void validateConfirmEmailThrowsWhenEmailAndConfirmEmailMissing() {
		SelfRegistrationExtension ext = registrationWithEmail(null);

		ValidationException exception = assertThrows(ValidationException.class, ext::validateConfirmEmail);

		assertEquals(2, exception.getMessages().size());
	}

	@Test
	void validateConfirmEmailThrowsWhenEmailsMismatch() {
		SelfRegistrationExtension ext = registrationWithEmail("user@example.com");
		ext.setConfirmEmail("other@example.com");

		ValidationException exception = assertThrows(ValidationException.class, ext::validateConfirmEmail);

		assertEquals("You did not type the same email. Please re-enter your email again.",
				exception.getMessages().get(0).getText());
	}

	@Test
	void validateConfirmEmailWithMatchingEmailsDoesNotThrow() {
		SelfRegistrationExtension ext = registrationWithEmail("user@example.com");
		ext.setConfirmEmail("user@example.com");

		assertDoesNotThrow(ext::validateConfirmEmail);
	}

	private static SelfRegistrationExtension registrationWithEmail(String email) {
		SelfRegistrationExtension ext = new SelfRegistrationExtension();
		UserExtension user = new UserExtension();
		ContactExtension contact = new ContactExtension();
		contact.setEmail1(email);
		user.setContact(contact);
		ext.setUser(user);
		return ext;
	}
}
