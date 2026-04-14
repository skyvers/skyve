package org.skyve;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.cdi.MailServiceInjectable;
import org.skyve.impl.mail.AzureCommunication;
import org.skyve.impl.mail.MailServiceStaticSingleton;
import org.skyve.impl.mail.Postmark;
import org.skyve.impl.mail.SMTPMailService;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.Mail;
import org.skyve.util.MailService;

class MailServiceTest {
	private MailService originalMailService;
	private String originalSmtpSender;
	private String originalSmtpTestRecipient;
	private boolean originalSmtpTestBogusSend;

	@BeforeEach
	void beforeEach() {
		MailServiceStaticSingleton.setDefault();
		originalMailService = MailServiceStaticSingleton.get();
		originalSmtpSender = UtilImpl.SMTP_SENDER;
		originalSmtpTestRecipient = UtilImpl.SMTP_TEST_RECIPIENT;
		originalSmtpTestBogusSend = UtilImpl.SMTP_TEST_BOGUS_SEND;

		UtilImpl.SMTP_SENDER = "default-sender@skyve.org";
		UtilImpl.SMTP_TEST_RECIPIENT = null;
		UtilImpl.SMTP_TEST_BOGUS_SEND = false;
	}

	@AfterEach
	void afterEach() {
		MailServiceStaticSingleton.set(originalMailService);
		UtilImpl.SMTP_SENDER = originalSmtpSender;
		UtilImpl.SMTP_TEST_RECIPIENT = originalSmtpTestRecipient;
		UtilImpl.SMTP_TEST_BOGUS_SEND = originalSmtpTestBogusSend;
	}

	@SuppressWarnings("static-method")
	@Test
	void testSetDefaultUsesSMTPImplementation() {
		MailServiceStaticSingleton.setDefault();
		assertThat(MailServiceStaticSingleton.get(), instanceOf(SMTPMailService.class));
	}

	@SuppressWarnings({ "static-method", "boxing" })
	@Test
	void testSendMailAppliesSenderFallbackAndTestRecipientOverride() {
		CaptureMailService capture = new CaptureMailService();
		MailServiceStaticSingleton.set(capture);
		UtilImpl.SMTP_TEST_RECIPIENT = "redirect@skyve.org";
		MailService mailService = new MailServiceInjectable();

		mailService.sendMail(new Mail().addTo("to@skyve.org")
										.addCC("cc@skyve.org")
										.addBCC("bcc@skyve.org")
										.subject("subject")
										.body("body"));

		assertThat(capture.sendCount, is(1));
		assertThat(capture.lastSend.getSenderEmailAddress(), is("default-sender@skyve.org"));
		assertThat(capture.lastSend.getRecipientEmailAddresses(), is(setOf("redirect@skyve.org")));
		assertThat(capture.lastSend.getCcEmailAddresses().isEmpty(), is(true));
		assertThat(capture.lastSend.getBccEmailAddresses().isEmpty(), is(true));
	}

	@SuppressWarnings({ "static-method", "boxing" })
	@Test
	void testSendMailSenderFallbackReusesMailInstanceWithoutTestRecipientOverride() {
		CaptureMailService capture = new CaptureMailService();
		MailServiceStaticSingleton.set(capture);
		MailService mailService = new MailServiceInjectable();
		Mail mail = new Mail().addTo("to@skyve.org")
								.subject("subject")
								.body("body");

		mailService.sendMail(mail);

		assertThat(capture.sendCount, is(1));
		assertSame(mail, capture.lastSend);
		assertThat(capture.lastSend.getSenderEmailAddress(), is("default-sender@skyve.org"));
		assertThat(capture.lastSend.getRecipientEmailAddresses(), is(setOf("to@skyve.org")));
	}

	@SuppressWarnings({ "static-method", "boxing" })
	@Test
	void testBogusSendSkipsDispatchForAllServices() {
		CaptureMailService capture = new CaptureMailService();
		MailServiceStaticSingleton.set(capture);
		UtilImpl.SMTP_TEST_BOGUS_SEND = true;
		MailService mailService = new MailServiceInjectable();

		mailService.sendMail(new Mail().from("sender@skyve.org").addTo("to@skyve.org").subject("subject").body("body"));
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		mailService.writeMail(new Mail().from("sender@skyve.org").addTo("to@skyve.org").subject("subject").body("body"), out);
		mailService.sendBulkMail(Arrays.asList(new Mail().from("sender@skyve.org").addTo("to@skyve.org").subject("subject").body("body")));

		assertThat(capture.sendCount, is(0));
		assertThat(capture.writeCount, is(0));
		assertThat(capture.bulkCount, is(0));
		assertThat(out.size(), is(0));
	}

	@SuppressWarnings({ "static-method", "boxing" })
	@Test
	void testSendBulkMailAppliesNormalisation() {
		CaptureMailService capture = new CaptureMailService();
		MailServiceStaticSingleton.set(capture);
		UtilImpl.SMTP_TEST_RECIPIENT = "redirect@skyve.org";
		MailService mailService = new MailServiceInjectable();

		Mail first = new Mail().addTo("to1@skyve.org")
								.addCC("cc1@skyve.org")
								.subject("subject1")
								.body("body1");
		Mail second = new Mail().from("sender2@skyve.org")
								.addTo("to2@skyve.org")
								.addBCC("bcc2@skyve.org")
								.subject("subject2")
								.body("body2");

		mailService.sendBulkMail(Arrays.asList(first, second));

		assertThat(capture.bulkCount, is(1));
		assertThat(capture.lastBulk.size(), is(2));
		assertThat(capture.lastBulk.get(0).getSenderEmailAddress(), is("default-sender@skyve.org"));
		assertThat(capture.lastBulk.get(0).getRecipientEmailAddresses(), is(setOf("redirect@skyve.org")));
		assertThat(capture.lastBulk.get(0).getCcEmailAddresses().isEmpty(), is(true));
		assertThat(capture.lastBulk.get(1).getSenderEmailAddress(), is("sender2@skyve.org"));
		assertThat(capture.lastBulk.get(1).getRecipientEmailAddresses(), is(setOf("redirect@skyve.org")));
		assertThat(capture.lastBulk.get(1).getBccEmailAddresses().isEmpty(), is(true));
	}

	@SuppressWarnings({ "static-method", "boxing" })
	@Test
	void testInjectableDelegatesViaExtMailService() {
		CaptureMailService capture = new CaptureMailService();
		MailServiceStaticSingleton.set(capture);
		UtilImpl.SMTP_TEST_RECIPIENT = "redirect@skyve.org";

		MailService injectable = new MailServiceInjectable();
		injectable.sendMail(new Mail().addTo("to@skyve.org")
									.subject("subject")
									.body("body"));

		assertThat(capture.sendCount, is(1));
		assertThat(capture.lastSend.getSenderEmailAddress(), is("default-sender@skyve.org"));
		assertThat(capture.lastSend.getRecipientEmailAddresses(), is(setOf("redirect@skyve.org")));
	}

	@SuppressWarnings("static-method")
	@Test
	void testAzureCommunicationIsScaffoldOnly() {
		IllegalStateException e = assertThrows(IllegalStateException.class,
				() -> new AzureCommunication().sendMail(new Mail()));
		assertThat(e.getMessage(), containsString("scaffold-only"));
	}

	@SuppressWarnings("static-method")
	@Test
	void testPostmarkIsScaffoldOnly() {
		IllegalStateException e = assertThrows(IllegalStateException.class,
				() -> new Postmark().sendMail(new Mail()));
		assertThat(e.getMessage(), containsString("scaffold-only"));
	}

	private static Set<String> setOf(String... values) {
		Set<String> result = new TreeSet<>();
		for (String value : values) {
			result.add(value);
		}
		return result;
	}

	private static class CaptureMailService implements MailService {
		private Mail lastSend;
		private Mail lastWrite;
		private List<Mail> lastBulk;
		private int sendCount;
		private int writeCount;
		private int bulkCount;

		@Override
		public void writeMail(Mail mail, OutputStream out) {
			lastWrite = mail;
			writeCount++;
		}

		@Override
		public void sendMail(Mail mail) {
			lastSend = mail;
			sendCount++;
		}

		@Override
		public void sendBulkMail(List<Mail> mails) {
			lastBulk = mails;
			bulkCount++;
		}
	}
}
