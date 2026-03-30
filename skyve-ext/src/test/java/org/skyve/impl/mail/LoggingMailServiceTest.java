package org.skyve.impl.mail;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
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

	@SuppressWarnings("boxing")
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
		assertThat(entry.getBodyExcerpt(), is("[REDACTED]"));
		assertThat(entry.getAttachmentFileNames(), is("a.txt"));
		assertThat(entry.getIsBulk(), is(Boolean.FALSE));
	}

	@SuppressWarnings("boxing")
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

	@SuppressWarnings("boxing")
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

	@SuppressWarnings("boxing")
	@Test
	void testDispatchMailNormalisesNullProviderOutcomes() {
		RecordingDelegate delegate = new RecordingDelegate();
		MailService service = new LoggingMailService(delegate);
		Mail mail = new Mail().from("sender@skyve.org")
								.addTo("to@skyve.org")
								.subject("Subject")
								.body("Body");

		delegate.singleOutcome = null;
		MailDispatchOutcome nullOutcome = service.dispatchMail(mail);
		assertThat(nullOutcome.getStatus(), is(MailDispatchOutcome.DispatchStatus.SENT));
		assertThat(nullOutcome.getProvider(), is("RecordingDelegate"));

		delegate.singleOutcome = MailDispatchOutcome.failed(null, "relay failed");
		MailDispatchOutcome failedOutcome = service.dispatchMail(mail);
		assertThat(failedOutcome.getStatus(), is(MailDispatchOutcome.DispatchStatus.FAILED));
		assertThat(failedOutcome.getProvider(), is("RecordingDelegate"));
		assertThat(failedOutcome.getFailureDetail(), is("relay failed"));

		delegate.singleOutcome = MailDispatchOutcome.skipped(null, "test skip");
		MailDispatchOutcome skippedOutcome = service.dispatchMail(mail);
		assertThat(skippedOutcome.getStatus(), is(MailDispatchOutcome.DispatchStatus.SKIPPED));
		assertThat(skippedOutcome.getProvider(), is("RecordingDelegate"));
		assertThat(skippedOutcome.getRelayDetail(), is("test skip"));

		delegate.singleOutcome = MailDispatchOutcome.sent(null, "message-9", "accepted", "queued");
		MailDispatchOutcome sentOutcome = service.dispatchMail(mail);
		assertThat(sentOutcome.getStatus(), is(MailDispatchOutcome.DispatchStatus.SENT));
		assertThat(sentOutcome.getProvider(), is("RecordingDelegate"));
		assertThat(sentOutcome.getProviderMessageId(), is("message-9"));
		assertThat(sentOutcome.getRelayStatus(), is("accepted"));
		assertThat(sentOutcome.getRelayDetail(), is("queued"));
	}

	@SuppressWarnings("boxing")
	@Test
	void testDispatchBulkMailDefaultsNullOutcomeAndLogsBulkEntry() {
		RecordingDelegate delegate = new RecordingDelegate();
		MailService service = new LoggingMailService(delegate);
		delegate.bulkOutcome = null;

		Mail first = new Mail().from("sender@skyve.org")
								.addTo("a@skyve.org")
								.subject("Subject 1")
								.body("Body 1");
		Mail second = new Mail().from("sender@skyve.org")
								.addTo("b@skyve.org")
								.subject("Subject 2")
								.body("Body 2");

		MailDispatchOutcome outcome = service.dispatchBulkMail(Arrays.asList(first, second));

		assertThat(outcome.getStatus(), is(MailDispatchOutcome.DispatchStatus.SENT));
		assertThat(outcome.getProvider(), is("RecordingDelegate"));
		assertThat(delegate.bulkDispatchCount, is(1));
		assertThat(entries.size(), is(1));
		assertThat(entries.get(0).getIsBulk(), is(Boolean.TRUE));
		assertThat(entries.get(0).getMailCount(), is(Long.valueOf(2)));
		assertThat(entries.get(0).getProvider(), is("RecordingDelegate"));
	}

	@SuppressWarnings("boxing")
	@Test
	void testDispatchMailBlankFailureMessageFallsBackToExceptionClassName() {
		RecordingDelegate delegate = new RecordingDelegate();
		delegate.singleException = new IllegalStateException("");
		MailService service = new LoggingMailService(delegate);

		IllegalStateException e = assertThrows(IllegalStateException.class,
				() -> service.dispatchMail(new Mail().from("sender@skyve.org")
													.addTo("to@skyve.org")
													.subject("Subject")
													.body("Body")));
		assertThat(e.getMessage(), is(""));
		assertThat(entries.size(), is(1));
		assertThat(entries.get(0).getDispatchStatus(), is("FAILED"));
		assertThat(entries.get(0).getErrorDetail(), is(IllegalStateException.class.getName()));
		assertThat(entries.get(0).getProvider(), is("RecordingDelegate"));
	}

	@SuppressWarnings("boxing")
	@Test
	void testSendBulkMailBlankFailureMessageFallsBackToExceptionClassName() {
		RecordingDelegate delegate = new RecordingDelegate();
		delegate.bulkException = new IllegalStateException("");
		MailService service = new LoggingMailService(delegate);

		IllegalStateException e = assertThrows(IllegalStateException.class,
				() -> service.sendBulkMail(Arrays.asList(new Mail().from("sender@skyve.org")
																.addTo("to@skyve.org")
																.subject("Subject")
																.body("Body"))));
		assertThat(e.getMessage(), is(""));
		assertThat(entries.size(), is(1));
		assertThat(entries.get(0).getDispatchStatus(), is("FAILED"));
		assertThat(entries.get(0).getErrorDetail(), is(IllegalStateException.class.getName()));
		assertThat(entries.get(0).getProvider(), is("RecordingDelegate"));
	}

	@SuppressWarnings("boxing")
	@Test
	void testDispatchBulkMailBlankFailureMessageFallsBackToExceptionClassName() {
		RecordingDelegate delegate = new RecordingDelegate();
		delegate.bulkException = new IllegalStateException("");
		MailService service = new LoggingMailService(delegate);

		IllegalStateException e = assertThrows(IllegalStateException.class,
				() -> service.dispatchBulkMail(Arrays.asList(new Mail().from("sender@skyve.org")
																	.addTo("to@skyve.org")
																	.subject("Subject")
																	.body("Body"))));
		assertThat(e.getMessage(), is(""));
		assertThat(entries.size(), is(1));
		assertThat(entries.get(0).getDispatchStatus(), is("FAILED"));
		assertThat(entries.get(0).getErrorDetail(), is(IllegalStateException.class.getName()));
		assertThat(entries.get(0).getProvider(), is("RecordingDelegate"));
	}

	@SuppressWarnings("boxing")
	@Test
	void testSendMailBlankFailureMessageFallsBackToExceptionClassName() {
		RecordingDelegate delegate = new RecordingDelegate();
		delegate.singleException = new IllegalStateException("");
		MailService service = new LoggingMailService(delegate);

		IllegalStateException e = assertThrows(IllegalStateException.class,
				() -> service.sendMail(new Mail().from("sender@skyve.org")
												.addTo("to@skyve.org")
												.subject("Subject")
												.body("Body")));
		assertThat(e.getMessage(), is(""));
		assertThat(entries.size(), is(1));
		assertThat(entries.get(0).getDispatchStatus(), is("FAILED"));
		assertThat(entries.get(0).getErrorDetail(), is(IllegalStateException.class.getName()));
		assertThat(entries.get(0).getProvider(), is("RecordingDelegate"));
	}

	@SuppressWarnings("boxing")
	@Test
	void testBogusSendEmptyBulkMailLogsSkippedEntry() {
		RecordingDelegate delegate = new RecordingDelegate();
		MailService service = new LoggingMailService(delegate);
		UtilImpl.SMTP_TEST_BOGUS_SEND = true;

		service.sendBulkMail(Collections.emptyList());

		assertThat(delegate.bulkDispatchCount, is(0));
		assertThat(entries.size(), is(1));
		assertThat(entries.get(0).getDispatchStatus(), is("SKIPPED"));
		assertThat(entries.get(0).getMailCount(), is(Long.valueOf(0)));
		assertThat(entries.get(0).getProvider(), is("RecordingDelegate"));
	}

	@SuppressWarnings({ "boxing", "deprecation" })
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

	@SuppressWarnings("boxing")
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
		assertThat(entry.getBodyExcerpt(), is("[REDACTED]"));
		assertThat(entry.getHasMultipleSubjects(), is(Boolean.TRUE));
		assertThat(entry.getSubjectVariantCount(), is(Long.valueOf(2)));
		assertThat(entry.getHasMultipleBodies(), is(Boolean.TRUE));
		assertThat(entry.getBodyVariantCount(), is(Long.valueOf(2)));
	}

	@SuppressWarnings("static-method")
	@Test
	void testBodyExcerptAlwaysRedacted() {
		assertThat(MailLogUtil.bodyExcerpt("<p>Hello <b>World</b></p>"), is("[REDACTED]"));
		assertThat(MailLogUtil.bodyExcerpt("Line one<br />Line two"), is("[REDACTED]"));
		assertThat(MailLogUtil.bodyExcerpt("Your verification code is: 123456"), is("[REDACTED]"));
		assertThat(MailLogUtil.bodyExcerpt("   "), is((String) null));
		assertThat(MailLogUtil.bodyExcerpt(null), is((String) null));
	}

	@SuppressWarnings("boxing")
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
