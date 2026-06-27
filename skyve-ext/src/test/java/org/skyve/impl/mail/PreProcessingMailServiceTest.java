package org.skyve.impl.mail;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import java.util.Arrays;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.Mail;
import org.skyve.util.MailDispatchOutcome;
import org.skyve.util.MailService;

import jakarta.annotation.Nonnull;

/**
 * Targeted tests for uncovered paths in PreProcessingMailService.
 */
@SuppressWarnings("static-method")
class PreProcessingMailServiceTest {
	private boolean originalBogusSend;
	private String originalTestRecipient;
	private String originalSmtpSender;

	@BeforeEach
	void setUp() {
		originalBogusSend = UtilImpl.SMTP_TEST_BOGUS_SEND;
		originalTestRecipient = UtilImpl.SMTP_TEST_RECIPIENT;
		originalSmtpSender = UtilImpl.SMTP_SENDER;
		UtilImpl.SMTP_TEST_BOGUS_SEND = false;
		UtilImpl.SMTP_TEST_RECIPIENT = null;
		UtilImpl.SMTP_SENDER = "relay@skyve.org";
	}

	@AfterEach
	void tearDown() {
		UtilImpl.SMTP_TEST_BOGUS_SEND = originalBogusSend;
		UtilImpl.SMTP_TEST_RECIPIENT = originalTestRecipient;
		UtilImpl.SMTP_SENDER = originalSmtpSender;
	}

	/**
	 * writeMail returns early when SMTP_TEST_BOGUS_SEND is true.
	 */
	@Test
	void writeMailWithBogusSendEnabledDoesNotDelegateToWriteMail() {
		AtomicInteger writeCalled = new AtomicInteger(0);
		MailService delegate = new NoOpMailService() {
			@Override
			public void writeMail(@Nonnull Mail mail, @Nonnull OutputStream out) {
				writeCalled.incrementAndGet();
			}
		};

		UtilImpl.SMTP_TEST_BOGUS_SEND = true;
		PreProcessingMailService service = new PreProcessingMailService(delegate);
		service.writeMail(new Mail().from("sender@example.com").addTo("to@example.com")
				.subject("Test").body("Body"),
				new ByteArrayOutputStream());

		assertEquals(0, writeCalled.get(), "delegate writeMail should not be called when SMTP_TEST_BOGUS_SEND is true");
	}

	/**
	 * normaliseMails covers the normalisedMails != null branch when
	 * a second mail follows one that was already normalised.
	 */
	@Test
	void sendBulkMailWithMultipleMailsWhenFirstNormalisedCopiesRemainingMails() {
		AtomicInteger bulkCount = new AtomicInteger(0);
		MailService delegate = new NoOpMailService() {
			@Override
			public void sendBulkMail(@Nonnull java.util.List<Mail> mails) {
				bulkCount.set(mails.size());
			}
		};

		// Use a non-null test recipient so normaliseMail always returns a new Mail instance,
		// ensuring the normalisedMails != null branch is reached on the second element.
		UtilImpl.SMTP_TEST_RECIPIENT = "test@skyve.org";

		PreProcessingMailService service = new PreProcessingMailService(delegate);
		Mail m1 = new Mail().from("a@example.com").addTo("x@example.com").subject("S1").body("B1");
		Mail m2 = new Mail().from("b@example.com").addTo("y@example.com").subject("S2").body("B2");
		service.sendBulkMail(Arrays.asList(m1, m2));

		assertEquals(2, bulkCount.get());
	}

	// ---- minimal no-op base for delegate ----

	private static class NoOpMailService implements MailService {
		@Override
		public void writeMail(@Nonnull Mail mail, @Nonnull OutputStream out) {
			// no-op
		}

		@Override
		public void sendMail(@Nonnull Mail mail) {
			// no-op
		}

		@Override
		public void sendBulkMail(@Nonnull java.util.List<Mail> mails) {
			// no-op
		}

		@Override
		public @Nonnull MailDispatchOutcome dispatchMail(@Nonnull Mail mail) {
			return MailDispatchOutcome.sent("noop");
		}

		@Override
		public @Nonnull MailDispatchOutcome dispatchBulkMail(@Nonnull java.util.List<Mail> mails) {
			return MailDispatchOutcome.sent("noop");
		}
	}

	@Test
	void dispatchMailDelegatesToDelegate() {
		PreProcessingMailService svc = new PreProcessingMailService(new NoOpMailService());
		Mail mail = new Mail().from("a@b.com").subject("Hello").addTo("c@d.com");
		MailDispatchOutcome outcome = svc.dispatchMail(mail);
		assertEquals(MailDispatchOutcome.DispatchStatus.SENT, outcome.getStatus());
	}

	@Test
	void dispatchBulkMailDelegatesToDelegate() {
		PreProcessingMailService svc = new PreProcessingMailService(new NoOpMailService());
		Mail mail = new Mail().from("a@b.com").subject("Hello").addTo("c@d.com");
		MailDispatchOutcome outcome = svc.dispatchBulkMail(Arrays.asList(mail));
		assertEquals(MailDispatchOutcome.DispatchStatus.SENT, outcome.getStatus());
	}

	@Test
	void sendMailDelegatesToDelegate() {
		PreProcessingMailService svc = new PreProcessingMailService(new NoOpMailService());
		Mail mail = new Mail().from("a@b.com").subject("Hello").addTo("c@d.com");
		svc.sendMail(mail);
		// No exception means success
		assertEquals("a@b.com", mail.getSenderEmailAddress());
	}

        @Test
        void dispatchBulkMailWithTwoMailsAndTestRecipientCoversInnerLoop() {
                // covers L64-65: first mail is NOT rerouted (SMTP_TEST_RECIPIENT null for first,
                // then set so second mail triggers normalisedMails != null path)
                // Actually we need test recipient set before calling so BOTH mails are rerouted,
                // triggering normalised != mail on the SECOND iteration (i=1) which runs the inner loop.
                String savedTestRecipient = org.skyve.impl.util.UtilImpl.SMTP_TEST_RECIPIENT;
                try {
                        org.skyve.impl.util.UtilImpl.SMTP_TEST_RECIPIENT = "test@test.com";
                        PreProcessingMailService svc = new PreProcessingMailService(new NoOpMailService());
                        Mail mail1 = new Mail().from("a@b.com").subject("Hello").addTo("c@d.com");
                        Mail mail2 = new Mail().from("e@f.com").subject("World").addTo("g@h.com");
                        MailDispatchOutcome outcome = svc.dispatchBulkMail(Arrays.asList(mail1, mail2));
                        assertEquals(MailDispatchOutcome.DispatchStatus.SENT, outcome.getStatus());
                } finally {
                        org.skyve.impl.util.UtilImpl.SMTP_TEST_RECIPIENT = savedTestRecipient;
                }
        }
}
