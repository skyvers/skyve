package org.skyve.util;

import java.io.OutputStream;
import java.util.List;

import jakarta.annotation.Nonnull;

/**
 * A mail service interface for Skyve.
 */
public interface MailService {

	/**
	 * Write an email to the output stream in MIME RFC 822 format.
	 * 
	 * Outlook <em>may</em> be able to load this format. Note - "on behalf of"... If the sender
	 * email address differs from the Skyve configured email credentials then,
	 * depending on the email client receiving the email at the destination, the
	 * email sending address may display with an indication that it wasn't
	 * actually sent from the email account the email says it was. For example,
	 * outlook displays a from addresses something like
	 * "mailer@skyve.com (on behalf of sender@foo.com)".
	 * 
	 * @param mail The email to write.
	 * @param out The stream to write to.
	 */
	void writeMail(@Nonnull Mail mail, @Nonnull OutputStream out);

	/**
	 * Send an email.
	 * 
	 * @param mail	The email to send.
	 */
	void sendMail(@Nonnull Mail mail);

	/**
	 * Dispatch an email and return dispatch metadata.
	 * <p>
	 * Implementations can override to provide provider-specific IDs and relay status.
	 * </p>
	 * 
	 * @param mail	The email to send.
	 * @return	The dispatch outcome metadata.
	 */
	default @Nonnull MailDispatchOutcome dispatchMail(@Nonnull Mail mail) {
		sendMail(mail);
		return MailDispatchOutcome.sent(null);
	}

	/**
	 * Send bulk/broadcast email messages.
	 * 
	 * @param mails	The emails to send.
	 */
	void sendBulkMail(@Nonnull List<Mail> mails);

	/**
	 * Dispatch bulk/broadcast email messages and return dispatch metadata.
	 * <p>
	 * Implementations can override to provide provider-specific IDs and relay status.
	 * </p>
	 * 
	 * @param mails	The emails to send.
	 * @return	The dispatch outcome metadata.
	 */
	default @Nonnull MailDispatchOutcome dispatchBulkMail(@Nonnull List<Mail> mails) {
		sendBulkMail(mails);
		return MailDispatchOutcome.sent(null);
	}
}
