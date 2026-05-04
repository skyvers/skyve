package org.skyve.impl.mail;

import java.io.OutputStream;
import java.util.List;

import org.skyve.util.Mail;
import org.skyve.util.MailService;

import jakarta.annotation.Nonnull;

/**
 * Azure Communication mail service scaffold.
 * <p>
 * To enable this implementation, set the mail service factory in `skyve.json`:
 * </p>
 * <pre>
 * "factories": {
 *   "mailServiceClass": "org.skyve.impl.mail.AzureCommunication"
 * }
 * </pre>
 * Mask this class in a Skyve project with a real implementation which makes use
 * of the Azure Communication SDK.
 */
public class AzureCommunication implements MailService {
	private static final String MESSAGE = "AzureCommunication is scaffold-only and does not yet implement mail sending.";

	@Override
	public void writeMail(@Nonnull Mail mail, @Nonnull OutputStream out) {
		throw new IllegalStateException(MESSAGE);
	}

	@Override
	public void sendMail(@Nonnull Mail mail) {
		throw new IllegalStateException(MESSAGE);
	}

	@Override
	public void sendBulkMail(@Nonnull List<Mail> mails) {
		throw new IllegalStateException(MESSAGE);
	}
}
