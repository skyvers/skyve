package org.skyve.impl.mail;

import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;

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
		List<Mail> normalisedMails = new ArrayList<>(mails.size());
		for (Mail mail : mails) {
			normalisedMails.add(normaliseMail(mail));
		}
		delegate.sendBulkMail(normalisedMails);
	}

	@Override
	public @Nonnull MailDispatchOutcome dispatchMail(@Nonnull Mail mail) {
		return delegate.dispatchMail(normaliseMail(mail));
	}

	@Override
	public @Nonnull MailDispatchOutcome dispatchBulkMail(@Nonnull List<Mail> mails) {
		List<Mail> normalisedMails = new ArrayList<>(mails.size());
		for (Mail mail : mails) {
			normalisedMails.add(normaliseMail(mail));
		}
		return delegate.dispatchBulkMail(normalisedMails);
	}

	private static @Nonnull Mail normaliseMail(@Nonnull Mail mail) {
		String sender = UtilImpl.processStringValue(mail.getSenderEmailAddress());
		if (sender == null) {
			sender = UtilImpl.SMTP_SENDER;
		}

		Mail result = new Mail().from(sender)
								.subject(mail.getSubject())
								.body(mail.getBody())
								.contentType(mail.getContentType())
								.attach(mail.getAttachments())
								.header(mail.getHeaders());

		String testRecipient = UtilImpl.processStringValue(UtilImpl.SMTP_TEST_RECIPIENT);
		if (testRecipient != null) {
			result.addTo(testRecipient);
		}
		else {
			result.addTo(mail.getRecipientEmailAddresses());
			result.addCC(mail.getCcEmailAddresses());
			result.addBCC(mail.getBccEmailAddresses());
		}

		return result;
	}
}
