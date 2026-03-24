package org.skyve.impl.mail;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.EXT;
import org.skyve.content.MimeType;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.Mail;
import org.skyve.util.MailAttachment;
import org.skyve.util.MailDispatchOutcome;
import org.skyve.util.MailService;

class LoggingMailServiceTest {
	private final List<MailLogUtil.MailLogEntry> entries = new ArrayList<>();

	private MailService originalMailService;
	private String originalSmtpSender;
	private String originalSmtpTestRecipient;
	private boolean originalSmtpTestBogusSend;

	@BeforeEach
	void beforeEach() {
		MailLogUtil.setRecorderForTesting(entries::add);

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
		MailLogUtil.clearRecorderForTesting();
		MailServiceStaticSingleton.set(originalMailService);
		UtilImpl.SMTP_SENDER = originalSmtpSender;
		UtilImpl.SMTP_TEST_RECIPIENT = originalSmtpTestRecipient;
		UtilImpl.SMTP_TEST_BOGUS_SEND = originalSmtpTestBogusSend;
	}

	@Test
	void testSendMailLogsNormalisedRecipientsAndProviderMetadata() {
		RecordingDelegate delegate = new RecordingDelegate();
		delegate.singleOutcome = MailDispatchOutcome.sent("smtp", "message-123", "accepted", "queued");
		MailService service = new PreProcessingMailService(new LoggingMailService(delegate));
		UtilImpl.SMTP_TEST_RECIPIENT = "redirect@skyve.org";

		service.sendMail(new Mail().addTo("to@skyve.org")
									.addCC("cc@skyve.org")
									.subject("Subject")
									.body("<p>Hello <b>World</b></p>")
									.attach(new MailAttachment("a.txt", new byte[] { 1, 2 }, MimeType.plain)));

		assertThat(delegate.singleDispatchCount, is(1));
		assertThat(entries.size(), is(1));

		MailLogUtil.MailLogEntry entry = entries.get(0);
		assertThat(entry.getDispatchStatus(), is("SENT"));
		assertThat(entry.getProvider(), is("smtp"));
		assertThat(entry.getProviderMessageId(), is("message-123"));
		assertThat(entry.getRelayStatus(), is("accepted"));
		assertThat(entry.getToRecipients(), is("redirect@skyve.org"));
		assertThat(entry.getCcRecipients(), is((String) null));
		assertThat(entry.getBccRecipients(), is((String) null));
		assertThat(entry.getBodyExcerpt(), is("Hello World"));
		assertThat(entry.getAttachmentFileNames(), is("a.txt"));
		assertThat(entry.getIsBulk(), is(Boolean.FALSE));
	}

	@Test
	void testSendMailFailureIsLoggedAndRethrown() {
		RecordingDelegate delegate = new RecordingDelegate();
		delegate.singleException = new IllegalStateException("relay down");
		MailService service = new PreProcessingMailService(new LoggingMailService(delegate));

		IllegalStateException e = assertThrows(IllegalStateException.class,
				() -> service.sendMail(new Mail().from("sender@skyve.org")
											.addTo("to@skyve.org")
											.subject("Subject")
											.body("Body")));
		assertThat(e.getMessage(), is("relay down"));
		assertThat(entries.size(), is(1));
		assertThat(entries.get(0).getDispatchStatus(), is("FAILED"));
		assertThat(entries.get(0).getErrorDetail(), containsString("relay down"));
	}

	@Test
	void testBogusSendSkipsDispatchAndLogsSkipped() {
		RecordingDelegate delegate = new RecordingDelegate();
		MailService service = new PreProcessingMailService(new LoggingMailService(delegate));
		UtilImpl.SMTP_TEST_BOGUS_SEND = true;

		service.sendMail(new Mail().from("sender@skyve.org")
									.addTo("to@skyve.org")
									.subject("Subject")
									.body("Body"));

		assertThat(delegate.singleDispatchCount, is(0));
		assertThat(entries.size(), is(1));
		assertThat(entries.get(0).getDispatchStatus(), is("SKIPPED"));
		assertThat(entries.get(0).getRelayDetail(), is("testBogusSend"));
	}

	@Test
	void testExtSendMailBogusSendLogsSkipped() {
		RecordingDelegate delegate = new RecordingDelegate();
		MailServiceStaticSingleton.set(delegate);
		UtilImpl.SMTP_TEST_BOGUS_SEND = true;

		EXT.sendMail(new Mail().from("sender@skyve.org")
								.addTo("to@skyve.org")
								.subject("Subject")
								.body("Body"));

		assertThat(delegate.singleDispatchCount, is(0));
		assertThat(entries.size(), is(1));
		assertThat(entries.get(0).getDispatchStatus(), is("SKIPPED"));
		assertThat(entries.get(0).getRelayDetail(), is("testBogusSend"));
	}

	@Test
	void testBulkSendLogsSingleEntryWithFirstBodyAndVariantCounts() {
		RecordingDelegate delegate = new RecordingDelegate();
		delegate.bulkOutcome = MailDispatchOutcome.sent("smtp");
		MailService service = new PreProcessingMailService(new LoggingMailService(delegate));

		Mail first = new Mail().from("sender@skyve.org")
								.addTo("a@skyve.org")
								.subject("Subject 1")
								.body("<div>Alpha</div>");
		Mail second = new Mail().from("sender@skyve.org")
								.addTo("b@skyve.org")
								.addCC("c@skyve.org")
								.subject("Subject 2")
								.body("<div>Beta</div>");

		service.sendBulkMail(Arrays.asList(first, second));

		assertThat(delegate.bulkDispatchCount, is(1));
		assertThat(entries.size(), is(1));

		MailLogUtil.MailLogEntry entry = entries.get(0);
		assertThat(entry.getIsBulk(), is(Boolean.TRUE));
		assertThat(entry.getMailCount(), is(Long.valueOf(2)));
		assertThat(entry.getRecipientCount(), is(Long.valueOf(3)));
		assertThat(entry.getSubject(), is("Subject 1"));
		assertThat(entry.getBodyExcerpt(), is("Alpha"));
		assertThat(entry.getHasMultipleSubjects(), is(Boolean.TRUE));
		assertThat(entry.getSubjectVariantCount(), is(Long.valueOf(2)));
		assertThat(entry.getHasMultipleBodies(), is(Boolean.TRUE));
		assertThat(entry.getBodyVariantCount(), is(Long.valueOf(2)));
	}

	@Test
	void testBodyExcerptStripsHtmlAndTruncatesTo255() {
		assertThat(MailLogUtil.bodyExcerpt("<p>Hello <b>World</b></p>"), is("Hello World"));
		assertThat(MailLogUtil.bodyExcerpt("Line one<br />Line two"), is("Line one\nLine two"));
		assertThat(MailLogUtil.bodyExcerpt("Your verification code is: 123456"), is("Your verification code is: ******"));

		String longHtml = "<p>" + "x".repeat(300) + "</p>";
		String excerpt = MailLogUtil.bodyExcerpt(longHtml);
		assertThat(excerpt.length(), is(255));
	}

	@Test
	void testWriteMailDoesNotCreateMailLog() {
		RecordingDelegate delegate = new RecordingDelegate();
		MailService service = new PreProcessingMailService(new LoggingMailService(delegate));

		service.writeMail(new Mail().from("sender@skyve.org")
									.addTo("to@skyve.org")
									.subject("Subject")
									.body("Body"),
						new ByteArrayOutputStream());

		assertThat(delegate.writeCount, is(1));
		assertThat(entries.size(), is(0));
	}

	private static class RecordingDelegate implements MailService {
		private MailDispatchOutcome singleOutcome = MailDispatchOutcome.sent("delegate");
		private MailDispatchOutcome bulkOutcome = MailDispatchOutcome.sent("delegate");
		private RuntimeException singleException;
		private RuntimeException bulkException;
		private int singleDispatchCount;
		private int bulkDispatchCount;
		private int writeCount;

		@Override
		public void writeMail(Mail mail, OutputStream out) {
			writeCount++;
		}

		@Override
		public void sendMail(Mail mail) {
			// no-op
		}

		@Override
		public void sendBulkMail(List<Mail> mails) {
			// no-op
		}

		@Override
		public MailDispatchOutcome dispatchMail(Mail mail) {
			singleDispatchCount++;
			if (singleException != null) {
				throw singleException;
			}
			return singleOutcome;
		}

		@Override
		public MailDispatchOutcome dispatchBulkMail(List<Mail> mails) {
			bulkDispatchCount++;
			if (bulkException != null) {
				throw bulkException;
			}
			return bulkOutcome;
		}
	}
}
