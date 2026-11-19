package org.skyve.impl.web.spring;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.Collections;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.EXT;
import org.skyve.impl.mail.CaptureMailService;
import org.skyve.impl.mail.MailServiceStaticSingleton;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.MailService;
import org.springframework.security.provisioning.UserDetailsManager;

import util.AbstractH2Test;

class TwoFactorAuthPushEmailFilterH2Test extends AbstractH2Test {
	private final CaptureMailService capture = new CaptureMailService();

	private MailService originalMailService;
	private String originalSmtpTestRecipient;
	private boolean originalSmtpTestBogusSend;

	@BeforeEach
	void beforeEach() throws SQLException {
		MailServiceStaticSingleton.setDefault();
		originalMailService = MailServiceStaticSingleton.get();
		originalSmtpTestRecipient = UtilImpl.SMTP_TEST_RECIPIENT;
		originalSmtpTestBogusSend = UtilImpl.SMTP_TEST_BOGUS_SEND;

		MailServiceStaticSingleton.set(capture);
		UtilImpl.SMTP_TEST_RECIPIENT = null;
		UtilImpl.SMTP_TEST_BOGUS_SEND = false;
		clearConfiguredEmailTemplate();
	}

	@AfterEach
	void afterEach() {
		MailServiceStaticSingleton.set(originalMailService);
		UtilImpl.SMTP_TEST_RECIPIENT = originalSmtpTestRecipient;
		UtilImpl.SMTP_TEST_BOGUS_SEND = originalSmtpTestBogusSend;
	}

	@Test
	void testPushNotificationUsesExtMailService() {
		ExposedTwoFactorAuthPushEmailFilter filter = new ExposedTwoFactorAuthPushEmailFilter();
		TwoFactorAuthUser user = new TwoFactorAuthUser("bizhub/test.user",
														"ignored",
														true,
														true,
														true,
														true,
														Collections.emptyList(),
														"bizhub",
														"test.user",
														null,
														null,
														null,
														"to@skyve.org",
														"hashed");

		filter.send(user, "654321");

		assertEquals(1, capture.getSent().size());
		assertTrue(capture.getLastSent().getRecipientEmailAddresses().contains("to@skyve.org"));
		assertThat(capture.getLastSent().getSenderEmailAddress(), is(UtilImpl.SMTP_SENDER));
		assertThat(capture.getLastSent().getSubject(), is(TwoFactorAuthPushEmailFilter.SYSTEM_TWO_FACTOR_CODE_SUBJECT));
		assertThat(capture.getLastSent().getBody(), containsString("654321"));
	}

	@Test
	void testResendGeneratesNewCodeAndSendsEmail() {
		ExposedTwoFactorAuthPushEmailFilter filter = new ExposedTwoFactorAuthPushEmailFilter();
		TwoFactorAuthUser user = new TwoFactorAuthUser("bizhub/test.user",
														"ignored",
														true,
														true,
														true,
														true,
														Collections.emptyList(),
														"bizhub",
														"test.user",
														null,
														null,
														null,
														"to@skyve.org",
														"hashed");

		filter.send(user, "111111");
		filter.send(user, "222222");

		assertEquals(2, capture.getSent().size());
		assertTrue(capture.getLastSent().getRecipientEmailAddresses().contains("to@skyve.org"));
		assertThat(capture.getLastSent().getSenderEmailAddress(), is(UtilImpl.SMTP_SENDER));
		assertThat(capture.getLastSent().getSubject(), is(TwoFactorAuthPushEmailFilter.SYSTEM_TWO_FACTOR_CODE_SUBJECT));
		assertThat(capture.getLastSent().getBody(), containsString("222222"));
		assertThat(capture.getLastSent().getBody(), not(containsString("111111")));
	}

	private static final class ExposedTwoFactorAuthPushEmailFilter extends TwoFactorAuthPushEmailFilter {
		private ExposedTwoFactorAuthPushEmailFilter() {
			super(mock(UserDetailsManager.class));
		}

		private void send(TwoFactorAuthUser user, String code) {
			pushNotification(user, code);
		}
	}

	private static void clearConfiguredEmailTemplate() throws SQLException {
		try (Connection c = EXT.getDataStoreConnection();
				PreparedStatement s = c.prepareStatement(
						"update ADM_Configuration set twoFactorEmailSubject = null, twoFactorEmailBody = null where bizCustomer = ?")) {
			s.setString(1, CUSTOMER);
			s.executeUpdate();
			c.commit();
		}
	}
}
