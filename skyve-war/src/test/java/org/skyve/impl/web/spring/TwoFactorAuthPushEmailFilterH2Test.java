package org.skyve.impl.web.spring;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;

import java.io.OutputStream;
import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.mail.MailServiceStaticSingleton;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.Mail;
import org.skyve.util.MailService;
import org.springframework.security.provisioning.UserDetailsManager;

import util.AbstractH2Test;

class TwoFactorAuthPushEmailFilterH2Test extends AbstractH2Test {
	private final CaptureMailService capture = new CaptureMailService();

	private MailService originalMailService;
	private String originalSmtpTestRecipient;
	private boolean originalSmtpTestBogusSend;

	@BeforeEach
	void beforeEach() {
		originalMailService = MailServiceStaticSingleton.get();
		originalSmtpTestRecipient = UtilImpl.SMTP_TEST_RECIPIENT;
		originalSmtpTestBogusSend = UtilImpl.SMTP_TEST_BOGUS_SEND;

		MailServiceStaticSingleton.set(capture);
		UtilImpl.SMTP_TEST_RECIPIENT = null;
		UtilImpl.SMTP_TEST_BOGUS_SEND = false;
	}

	@AfterEach
	void afterEach() {
		MailServiceStaticSingleton.set(originalMailService);
		UtilImpl.SMTP_TEST_RECIPIENT = originalSmtpTestRecipient;
		UtilImpl.SMTP_TEST_BOGUS_SEND = originalSmtpTestBogusSend;
	}

	@SuppressWarnings({ "boxing", "static-method" })
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

		assertThat(capture.sendCount, is(1));
		assertThat(capture.lastSend.getRecipientEmailAddresses().contains("to@skyve.org"), is(true));
		assertThat(capture.lastSend.getSenderEmailAddress(), is(UtilImpl.SMTP_SENDER));
		assertThat(capture.lastSend.getSubject(), is(TwoFactorAuthPushEmailFilter.SYSTEM_TWO_FACTOR_CODE_SUBJECT));
		assertThat(capture.lastSend.getBody(), containsString("654321"));
	}

	private static final class ExposedTwoFactorAuthPushEmailFilter extends TwoFactorAuthPushEmailFilter {
		private ExposedTwoFactorAuthPushEmailFilter() {
			super(mock(UserDetailsManager.class));
		}

		private void send(TwoFactorAuthUser user, String code) {
			pushNotification(user, code);
		}
	}

	private static class CaptureMailService implements MailService {
		private Mail lastSend;
		private int sendCount;

		@Override
		public void writeMail(Mail mail, OutputStream out) {
			// no-op
		}

		@Override
		public void sendMail(Mail mail) {
			lastSend = mail;
			sendCount++;
		}

		@Override
		public void sendBulkMail(List<Mail> mails) {
			// no-op
		}
	}
}
