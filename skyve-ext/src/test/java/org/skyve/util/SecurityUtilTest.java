package org.skyve.util;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.OutputStream;
import java.lang.reflect.Method;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.app.admin.SecurityLog;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.mail.MailServiceStaticSingleton;
import org.skyve.impl.util.UtilImpl;

class SecurityUtilTest {
	private final CaptureMailService capture = new CaptureMailService();

	private MailService originalMailService;
	private String originalArchiveName;
	private String originalEnvironmentIdentifier;
	private String originalSecurityNotificationsEmailAddress;
	private String originalSupportEmailAddress;
	private String originalSmtp;
	private String originalSmtpSender;
	private String originalSmtpTestRecipient;
	private boolean originalSmtpTestBogusSend;

	@BeforeEach
	void beforeEach() {
		MailServiceStaticSingleton.setDefault();
		originalMailService = MailServiceStaticSingleton.get();
		originalArchiveName = UtilImpl.ARCHIVE_NAME;
		originalEnvironmentIdentifier = UtilImpl.ENVIRONMENT_IDENTIFIER;
		originalSecurityNotificationsEmailAddress = UtilImpl.SECURITY_NOTIFICATIONS_EMAIL_ADDRESS;
		originalSupportEmailAddress = UtilImpl.SUPPORT_EMAIL_ADDRESS;
		originalSmtp = UtilImpl.SMTP;
		originalSmtpSender = UtilImpl.SMTP_SENDER;
		originalSmtpTestRecipient = UtilImpl.SMTP_TEST_RECIPIENT;
		originalSmtpTestBogusSend = UtilImpl.SMTP_TEST_BOGUS_SEND;

		MailServiceStaticSingleton.set(capture);
		UtilImpl.ARCHIVE_NAME = "SkyveTest";
		UtilImpl.ENVIRONMENT_IDENTIFIER = "TEST";
		UtilImpl.SECURITY_NOTIFICATIONS_EMAIL_ADDRESS = "security@skyve.org";
		UtilImpl.SUPPORT_EMAIL_ADDRESS = "support@skyve.org";
		UtilImpl.SMTP = "mail.skyve.org";
		UtilImpl.SMTP_SENDER = "noreply@skyve.org";
		UtilImpl.SMTP_TEST_RECIPIENT = null;
		UtilImpl.SMTP_TEST_BOGUS_SEND = false;
	}

	@AfterEach
	void afterEach() {
		MailServiceStaticSingleton.set(originalMailService);
		UtilImpl.ARCHIVE_NAME = originalArchiveName;
		UtilImpl.ENVIRONMENT_IDENTIFIER = originalEnvironmentIdentifier;
		UtilImpl.SECURITY_NOTIFICATIONS_EMAIL_ADDRESS = originalSecurityNotificationsEmailAddress;
		UtilImpl.SUPPORT_EMAIL_ADDRESS = originalSupportEmailAddress;
		UtilImpl.SMTP = originalSmtp;
		UtilImpl.SMTP_SENDER = originalSmtpSender;
		UtilImpl.SMTP_TEST_RECIPIENT = originalSmtpTestRecipient;
		UtilImpl.SMTP_TEST_BOGUS_SEND = originalSmtpTestBogusSend;
	}

	@SuppressWarnings({ "boxing", "static-method" })
	@Test
	void testEmailUsesExtMailService() throws Exception {
		SecurityLog securityLog = mock(SecurityLog.class);
		when(securityLog.getTimestamp()).thenReturn(new Timestamp());
		when(securityLog.getEventType()).thenReturn("Login Failure");
		when(securityLog.getEventMessage()).thenReturn("Bad password");
		when(securityLog.getSourceIP()).thenReturn("127.0.0.1");

		invokeEmail(securityLog);

		assertThat(capture.sendCount, is(1));
		assertThat(capture.lastSend.getRecipientEmailAddresses().contains("security@skyve.org"), is(true));
		assertThat(capture.lastSend.getSenderEmailAddress(), is("noreply@skyve.org"));
		assertThat(capture.lastSend.getSubject(), is("[SkyveTest - TEST] Security Log Entry - Login Failure"));
		assertThat(capture.lastSend.getBody(), containsString("Event Message: Bad password"));
	}

	private static void invokeEmail(SecurityLog securityLog) throws Exception {
		Method method = SecurityUtil.class.getDeclaredMethod("email", SecurityLog.class);
		method.setAccessible(true);
		method.invoke(null, securityLog);
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
