package org.skyve.impl.mail;

import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import org.skyve.impl.util.UtilImpl;
import org.skyve.util.Mail;
import org.skyve.util.MailDispatchOutcome;
import org.skyve.util.MailService;

import jakarta.annotation.Nonnull;

/**
 * Applies Skyve-wide mail pre-processing before delegating to the configured mail service.
 */
public class PreProcessingMailService implements MailService {
	private final MailService delegate;

	public PreProcessingMailService(@Nonnull MailService delegate) {
		this.delegate = delegate;
	}

	@Override
	public void writeMail(@Nonnull Mail mail, @Nonnull OutputStream out) {
		if (UtilImpl.SMTP_TEST_BOGUS_SEND) {
			return;
		}

		delegate.writeMail(normaliseMail(mail), out);
	}

	@Override
	public void sendMail(@Nonnull Mail mail) {
		delegate.sendMail(normaliseMail(mail));
	}

	@Override
	public void sendBulkMail(@Nonnull List<Mail> mails) {
		delegate.sendBulkMail(normaliseMails(mails));
	}

	@Override
	public @Nonnull MailDispatchOutcome dispatchMail(@Nonnull Mail mail) {
		return delegate.dispatchMail(normaliseMail(mail));
	}

	@Override
	public @Nonnull MailDispatchOutcome dispatchBulkMail(@Nonnull List<Mail> mails) {
		return delegate.dispatchBulkMail(normaliseMails(mails));
	}

	private static @Nonnull List<Mail> normaliseMails(@Nonnull List<Mail> mails) {
		List<Mail> normalisedMails = null;
		for (int i = 0, size = mails.size(); i < size; i++) {
			Mail mail = mails.get(i);
			Mail normalised = normaliseMail(mail);
			if (normalisedMails != null) {
				normalisedMails.add(normalised);
			}
			else if (normalised != mail) {
				normalisedMails = new ArrayList<>(size);
				for (int j = 0; j < i; j++) {
					normalisedMails.add(mails.get(j));
				}
				normalisedMails.add(normalised);
			}
		}
		return (normalisedMails == null) ? mails : normalisedMails;
	}

	private static @Nonnull Mail normaliseMail(@Nonnull Mail mail) {
		String originalSender = UtilImpl.processStringValue(mail.getSenderEmailAddress());
		String sender = (originalSender == null) ? UtilImpl.SMTP_SENDER : originalSender;
		String testRecipient = UtilImpl.processStringValue(UtilImpl.SMTP_TEST_RECIPIENT);

		if (testRecipient == null) {
			if (!Objects.equals(originalSender, sender)) {
				mail.from(sender);
			}
			return mail;
		}

		// Keep the original mail unchanged when overriding recipients for test routing.
		Mail result = new Mail().from(sender)
								.subject(mail.getSubject())
								.body(mail.getBody())
								.contentType(mail.getContentType())
								.attach(mail.getAttachments())
								.header(mail.getHeaders());

		// Legacy contract from MailUtil: when test-recipient routing is enabled,
		// all outbound mail is redirected to the single test inbox and CC/BCC are suppressed.
		result.addTo(testRecipient);

		return result;
	}
}
