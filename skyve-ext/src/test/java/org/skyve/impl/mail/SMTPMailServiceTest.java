package org.skyve.impl.mail;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.ByteArrayOutputStream;
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
import jakarta.mail.PasswordAuthentication;
import jakarta.mail.internet.InternetAddress;
import jakarta.mail.internet.MimeMessage;

@SuppressWarnings("static-method")
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
		assertEquals(2, multipart.getCount());
		BodyPart attachment = multipart.getBodyPart(1);
		assertThat(attachment.getFileName(), is("report.txt"));
		assertThat(attachment.getContentType(), containsString("text/plain"));
	}

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

	@Test
	void testSendMailWrapsTransportFailures() {
		Mail mail = new Mail().from("sender@skyve.org")
								.addTo("to@skyve.org")
								.subject("Subject")
								.body("Body");

		SMTPMailService smtpService = new SMTPMailService();
		ValidationException e = assertThrows(ValidationException.class, () -> smtpService.sendMail(mail));

		assertThat(e.getMessage(), containsString("Email was not sent"));
	}

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
		assertEquals(3, service.sendCount);
	}

	@Test
	void testWriteMailWritesRFC822OutputToStream() throws Exception {
		Mail mail = new Mail().from("sender@skyve.org")
				.addTo("to@skyve.org")
				.subject("Test Subject")
				.body("<b>Hello</b>")
				.html();

		ByteArrayOutputStream out = new ByteArrayOutputStream();
		new SMTPMailService().writeMail(mail, out);

		String output = out.toString("UTF-8");
		assertThat(output, containsString("Subject: Test Subject"));
	}

	@Test
	void testWriteMailWrapsExceptionWhenStreamFails() throws Exception {
		Mail mail = new Mail().from("sender@skyve.org")
				.addTo("to@skyve.org")
				.subject("Subject")
				.body("Body")
				.html();

		try (java.io.OutputStream failingStream = new java.io.OutputStream() {
			@Override
			public void write(int b) throws java.io.IOException {
				throw new java.io.IOException("Stream closed");
			}
		}) {
			SMTPMailService smtpService = new SMTPMailService();
			ValidationException e = assertThrows(ValidationException.class, () -> smtpService.writeMail(mail, failingStream));
			assertThat(e.getMessage(), containsString("Email was not written"));
		}
	}

	@Test
	void testCreateMailWithBccLogsBccAddresses() throws Exception {
		// Sends a mail with a BCC address to cover the BCC logging loop.
		Mail mail = new Mail().from("sender@skyve.org")
				.addTo("to@skyve.org")
				.addBCC("bcc@skyve.org")
				.subject("BCC Subject")
				.body("BCC Body")
				.textPlain();

		MimeMessage message = invokeCreateMail(mail, true);
		assertThat(message.getAllRecipients(), is(org.hamcrest.CoreMatchers.notNullValue()));
	}

	@Test
	void testCreateMailAuthenticatedWithSmtpProperties() throws Exception {
		// Authenticated session + SMTP_PROPERTIES covers the SMTP_PROPERTIES
		// iteration inside the authenticated (else) branch.
		UtilImpl.SMTP_UID = "smtp-user";
		UtilImpl.SMTP_PWD = "smtp-pass";
		UtilImpl.SMTP_PROPERTIES = Map.of("mail.smtp.connectiontimeout", "500");

		Mail mail = new Mail().from("sender@skyve.org")
				.addTo("to@skyve.org")
				.subject("Auth Subject")
				.body("Auth Body")
				.textPlain();

		MimeMessage message = invokeCreateMail(mail, false);
		assertThat(message.getSession().getProperty("mail.smtp.auth"), is("true"));
		assertThat(message.getSession().getProperty("mail.smtp.connectiontimeout"), is("500"));
	}

	@Test
	void testSendMailCoversTransportSendLine() {
		// SMTP_SENDER must be non-null so createMail() succeeds and Transport.send() is called.
		UtilImpl.SMTP_SENDER = "relay@skyve.org";
		UtilImpl.SMTP_PROPERTIES = Map.of("mail.smtp.connectiontimeout", "100",
				"mail.smtp.timeout", "100");

		Mail mail = new Mail().from("sender@skyve.org")
				.addTo("to@skyve.org")
				.subject("Subject")
				.body("Body")
				.html();

		SMTPMailService smtpService = new SMTPMailService();
		ValidationException e = assertThrows(ValidationException.class, () -> smtpService.sendMail(mail));
		assertThat(e.getMessage(), containsString("Email was not sent"));
	}

	@Test
	void testAuthenticatorReturnsConfiguredCredentials() throws Exception {
		UtilImpl.SMTP_UID = "smtp-user";
		UtilImpl.SMTP_PWD = "smtp-pass";

		Class<?> authenticatorClass = Class.forName("org.skyve.impl.mail.SMTPMailService$Authenticator");
		java.lang.reflect.Constructor<?> constructor = authenticatorClass.getDeclaredConstructor();
		constructor.setAccessible(true);
		Object authenticator = constructor.newInstance();

		Method getter = authenticatorClass.getDeclaredMethod("getPasswordAuthentication");
		getter.setAccessible(true);
		PasswordAuthentication authentication = (PasswordAuthentication) getter.invoke(authenticator);

		assertThat(authentication.getUserName(), is("smtp-user"));
		assertThat(authentication.getPassword(), is("smtp-pass"));
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
