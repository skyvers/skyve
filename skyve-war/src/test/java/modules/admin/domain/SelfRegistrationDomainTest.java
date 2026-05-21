package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.DateTime;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.AbstractH2Test;

public class SelfRegistrationDomainTest extends AbstractH2Test {

	@Test
	@SuppressWarnings("static-method")
	void dataBuilderCreatesSelfRegistration() throws Exception {
		SelfRegistration bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(SelfRegistration.MODULE_NAME, SelfRegistration.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	@SuppressWarnings("static-method")
	void moduleAndDocumentNames() throws Exception {
		SelfRegistration bean = SelfRegistration.newInstance();
		assertEquals("admin", bean.getBizModule());
		assertEquals("SelfRegistration", bean.getBizDocument());
	}

	@Test
	@SuppressWarnings("static-method")
	void activateUrlSetAndGet() throws Exception {
		SelfRegistration bean = SelfRegistration.newInstance();
		bean.setActivateUrl("https://example.com/activate?token=abc");
		assertEquals("https://example.com/activate?token=abc", bean.getActivateUrl());
	}

	@Test
	@SuppressWarnings("static-method")
	void confirmEmailSetAndGet() throws Exception {
		SelfRegistration bean = SelfRegistration.newInstance();
		bean.setConfirmEmail("user@example.com");
		assertEquals("user@example.com", bean.getConfirmEmail());
	}

	@Test
	@SuppressWarnings("static-method")
	void confirmPasswordSetAndGet() throws Exception {
		SelfRegistration bean = SelfRegistration.newInstance();
		bean.setConfirmPassword("Passw0rd!");
		assertEquals("Passw0rd!", bean.getConfirmPassword());
	}

	@Test
	@SuppressWarnings("static-method")
	void loginUrlSetAndGet() throws Exception {
		SelfRegistration bean = SelfRegistration.newInstance();
		bean.setLoginUrl("https://example.com/login");
		assertEquals("https://example.com/login", bean.getLoginUrl());
	}

	@Test
	@SuppressWarnings("static-method")
	void loginMessageSetAndGet() throws Exception {
		SelfRegistration bean = SelfRegistration.newInstance();
		bean.setLoginMessage("Registration successful. Please log in.");
		assertEquals("Registration successful. Please log in.", bean.getLoginMessage());
	}

	@Test
	@SuppressWarnings("static-method")
	void registrationDateSetAndGet() throws Exception {
		SelfRegistration bean = SelfRegistration.newInstance();
		DateTime now = new DateTime();
		bean.setRegistrationDate(now);
		assertNotNull(bean.getRegistrationDate());
	}

	@Test
	@SuppressWarnings("static-method")
	void passSilentlySetAndGet() throws Exception {
		SelfRegistration bean = SelfRegistration.newInstance();
		bean.setPassSilently(Boolean.TRUE);
		assertEquals(Boolean.TRUE, bean.getPassSilently());
	}

	@Test
	@SuppressWarnings("static-method")
	void previouslyAttemptedPasswordSetAndGet() throws Exception {
		SelfRegistration bean = SelfRegistration.newInstance();
		bean.setPreviouslyAttemptedPassword("OldPassword1");
		assertEquals("OldPassword1", bean.getPreviouslyAttemptedPassword());
	}
}
