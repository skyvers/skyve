package org.skyve.impl.mail;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.content.MimeType;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.Mail;
import org.skyve.util.MailAttachment;
import org.skyve.util.MailDispatchOutcome;

import jakarta.mail.BodyPart;
import jakarta.mail.Multipart;
import jakarta.mail.internet.InternetAddress;
import jakarta.mail.internet.MimeMessage;

class SMTPMailServiceTest {
	private String originalSmtp;
	private int originalSmtpPort;
	private String originalSmtpUid;
	private String originalSmtpPwd;
	private Map<String, String> originalSmtpProperties;
	private Map<String, String> originalSmtpHeaders;
	private String originalSmtpSender;

	@BeforeEach
	void beforeEach() {
		originalSmtp = UtilImpl.SMTP;
		originalSmtpPort = UtilImpl.SMTP_PORT;
		originalSmtpUid = UtilImpl.SMTP_UID;
		originalSmtpPwd = UtilImpl.SMTP_PWD;
		originalSmtpProperties = UtilImpl.SMTP_PROPERTIES;
		originalSmtpHeaders = UtilImpl.SMTP_HEADERS;
		originalSmtpSender = UtilImpl.SMTP_SENDER;

		UtilImpl.SMTP = "127.0.0.1";
		UtilImpl.SMTP_PORT = 1;
		UtilImpl.SMTP_UID = null;
		UtilImpl.SMTP_PWD = null;
		UtilImpl.SMTP_PROPERTIES = null;
		UtilImpl.SMTP_HEADERS = null;
		UtilImpl.SMTP_SENDER = "configured-sender@skyve.org";
	}

	@AfterEach
	void afterEach() {
		UtilImpl.SMTP = originalSmtp;
		UtilImpl.SMTP_PORT = originalSmtpPort;
		UtilImpl.SMTP_UID = originalSmtpUid;
		UtilImpl.SMTP_PWD = originalSmtpPwd;
		UtilImpl.SMTP_PROPERTIES = originalSmtpProperties;
		UtilImpl.SMTP_HEADERS = originalSmtpHeaders;
		UtilImpl.SMTP_SENDER = originalSmtpSender;
	}

	@SuppressWarnings("boxing")
	@Test
	void testCreateMailUsesPlainSessionHeadersAndAttachments() throws Exception {
		UtilImpl.SMTP_PROPERTIES = Map.of("mail.smtp.connectiontimeout", "1000");
		UtilImpl.SMTP_HEADERS = Map.of("X-Global-Header", "global");

		Mail mail = new Mail().from("sender@skyve.org")
								.addTo("to@skyve.org")
								.addCC("cc@skyve.org")
								.subject("Subject")
								.body("Body")
								.textPlain();
		mail.getHeaders().put("X-Mail-Header", "value");
		mail.attach(new MailAttachment("report.txt", new byte[] { 1, 2, 3 }, MimeType.plain));
		mail.attach(new MailAttachment("ignored.txt", null, MimeType.plain));
		mail.getAttachments().add(null);

		MimeMessage message = invokeCreateMail(mail, true);
		Multipart multipart = (Multipart) message.getContent();

		assertThat(message.getSession().getProperty("mail.smtp.auth"), is("false"));
		assertThat(((InternetAddress) message.getSender()).getAddress(), is("sender@skyve.org"));
		assertThat(message.getHeader("X-Global-Header", null), is("global"));
		assertThat(message.getHeader("X-Mail-Header", null), is("value"));
		assertThat(multipart.getCount(), is(2));
		BodyPart attachment = multipart.getBodyPart(1);
		assertThat(attachment.getFileName(), is("report.txt"));
		assertThat(attachment.getContentType(), containsString("text/plain"));
	}

	@SuppressWarnings("boxing")
	@Test
	void testCreateMailUsesAuthenticatedSessionWhenUidConfigured() throws Exception {
		UtilImpl.SMTP_UID = "smtp-user";
		UtilImpl.SMTP_PWD = "smtp-pass";

		Mail mail = new Mail().from("sender@skyve.org")
								.addTo("to@skyve.org")
								.subject("Subject")
								.body("Body");

		MimeMessage message = invokeCreateMail(mail, false);

		assertThat(message.getSession().getProperty("mail.smtp.auth"), is("true"));
		assertThat(((InternetAddress) message.getSender()).getAddress(), is("configured-sender@skyve.org"));
	}

	@SuppressWarnings("boxing")
	@Test
	void testSendMailWrapsTransportFailures() {
		Mail mail = new Mail().from("sender@skyve.org")
								.addTo("to@skyve.org")
								.subject("Subject")
								.body("Body");

		ValidationException e = assertThrows(ValidationException.class, () -> new SMTPMailService().sendMail(mail));

		assertThat(e.getMessage(), containsString("Email was not sent"));
	}

	@SuppressWarnings("boxing")
	@Test
	void testDispatchMailAndBulkMailReturnSmtpOutcomeWithoutSending() {
		StubSMTPMailService service = new StubSMTPMailService();
		Mail mail = new Mail().from("sender@skyve.org")
								.addTo("to@skyve.org")
								.subject("Subject")
								.body("Body");

		MailDispatchOutcome singleOutcome = service.dispatchMail(mail);
		MailDispatchOutcome bulkOutcome = service.dispatchBulkMail(Arrays.asList(mail, mail));

		assertThat(singleOutcome.getStatus(), is(MailDispatchOutcome.DispatchStatus.SENT));
		assertThat(singleOutcome.getProvider(), is("smtp"));
		assertThat(bulkOutcome.getStatus(), is(MailDispatchOutcome.DispatchStatus.SENT));
		assertThat(bulkOutcome.getProvider(), is("smtp"));
		assertThat(service.sendCount, is(3));
	}

	private static class StubSMTPMailService extends SMTPMailService {
		private int sendCount;

		@Override
		public void sendMail(Mail mail) {
			sendCount++;
		}
	}

	private static MimeMessage invokeCreateMail(Mail mail, boolean forWriting) throws Exception {
		Method method = SMTPMailService.class.getDeclaredMethod("createMail", Mail.class, boolean.class);
		method.setAccessible(true);
		return (MimeMessage) method.invoke(null, mail, Boolean.valueOf(forWriting));
	}
}
