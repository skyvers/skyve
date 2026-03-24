package org.skyve.impl.web;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.io.OutputStream;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.impl.mail.MailServiceStaticSingleton;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.DataBuilder;
import org.skyve.util.Mail;
import org.skyve.util.MailService;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.User.UserExtension;
import modules.admin.domain.User;
import util.AbstractH2Test;

class WebUtilH2Test extends AbstractH2Test {
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
	void testRequestPasswordResetUsesExtMailService() throws Exception {
		UserExtension user = new DataBuilder().fixture(FixtureType.crud).build(User.MODULE_NAME, User.DOCUMENT_NAME);
		String email = "reset-" + System.nanoTime() + "@skyve.org";
		user.setUserName("reset." + System.nanoTime());
		user.getContact().setEmail1(email);
		CORE.getPersistence().save(user);

		WebUtil.requestPasswordReset(CUSTOMER, email);

		assertThat(capture.sendCount, is(1));
		assertThat(capture.lastSend.getRecipientEmailAddresses().contains(email), is(true));
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
