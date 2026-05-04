package org.skyve.impl.backup;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.mail.MailServiceStaticSingleton;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.Mail;
import org.skyve.util.MailService;

class BackupJobTest {
	private final CaptureMailService capture = new CaptureMailService();

	private MailService originalMailService;
	private String originalArchiveName;
	private String originalEnvironmentIdentifier;
	private String originalSupportEmailAddress;
	private String originalSmtpSender;
	private String originalSmtpTestRecipient;
	private boolean originalSmtpTestBogusSend;

	@BeforeEach
	void beforeEach() {
		MailServiceStaticSingleton.setDefault();
		originalMailService = MailServiceStaticSingleton.get();
		originalArchiveName = UtilImpl.ARCHIVE_NAME;
		originalEnvironmentIdentifier = UtilImpl.ENVIRONMENT_IDENTIFIER;
		originalSupportEmailAddress = UtilImpl.SUPPORT_EMAIL_ADDRESS;
		originalSmtpSender = UtilImpl.SMTP_SENDER;
		originalSmtpTestRecipient = UtilImpl.SMTP_TEST_RECIPIENT;
		originalSmtpTestBogusSend = UtilImpl.SMTP_TEST_BOGUS_SEND;

		MailServiceStaticSingleton.set(capture);
		UtilImpl.ARCHIVE_NAME = "SkyveTest";
		UtilImpl.ENVIRONMENT_IDENTIFIER = "TEST";
		UtilImpl.SUPPORT_EMAIL_ADDRESS = "support@skyve.org";
		UtilImpl.SMTP_SENDER = "noreply@skyve.org";
		UtilImpl.SMTP_TEST_RECIPIENT = null;
		UtilImpl.SMTP_TEST_BOGUS_SEND = false;
	}

	@AfterEach
	void afterEach() {
		MailServiceStaticSingleton.set(originalMailService);
		UtilImpl.ARCHIVE_NAME = originalArchiveName;
		UtilImpl.ENVIRONMENT_IDENTIFIER = originalEnvironmentIdentifier;
		UtilImpl.SUPPORT_EMAIL_ADDRESS = originalSupportEmailAddress;
		UtilImpl.SMTP_SENDER = originalSmtpSender;
		UtilImpl.SMTP_TEST_RECIPIENT = originalSmtpTestRecipient;
		UtilImpl.SMTP_TEST_BOGUS_SEND = originalSmtpTestBogusSend;
	}

	@SuppressWarnings({ "boxing", "static-method" })
	@Test
	void testEmailProblemUsesExtMailService() throws Exception {
		List<String> jobLog = new ArrayList<>();

		BackupJob.emailProblem(jobLog, "simulated failure");

		assertThat(capture.sendCount, is(1));
		assertThat(capture.lastSend.getRecipientEmailAddresses().contains("support@skyve.org"), is(true));
		assertThat(capture.lastSend.getSenderEmailAddress(), is("noreply@skyve.org"));
		assertThat(capture.lastSend.getSubject(), is("[SkyveTest - TEST] Backup Problem"));
		assertThat(capture.lastSend.getBody(), containsString("a problem:- simulated failure"));
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
