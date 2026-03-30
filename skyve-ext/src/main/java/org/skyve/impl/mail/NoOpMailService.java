package org.skyve.impl.mail;

import java.io.OutputStream;
import java.util.List;

import org.skyve.util.Mail;
import org.skyve.util.MailDispatchOutcome;
import org.skyve.util.MailService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.annotation.Nonnull;

/**
 * No mail service implementation.
 * <p>
 * Configure this in `skyve.json` when you want mail calls to be ignored but still
 * flow through Skyve's mail service abstraction:
 * </p>
 * <pre>
 * "factories": {
 *   "mailServiceClass": "org.skyve.impl.mail.NoOpMailService"
 * }
 * </pre>
 */
public class NoOpMailService implements MailService {

	private static final Logger LOGGER = LoggerFactory.getLogger(NoOpMailService.class);

	@Override
	public void writeMail(@Nonnull Mail mail, @Nonnull OutputStream out) {
		LOGGER.debug("NoOpMailService ignored writeMail(subject={}, toCount={})",
					mail.getSubject(),
					Integer.valueOf(mail.getRecipientEmailAddresses().size()));
	}

	@Override
	public void sendMail(@Nonnull Mail mail) {
		LOGGER.debug("NoOpMailService ignored sendMail(subject={}, toCount={})",
					mail.getSubject(),
					Integer.valueOf(mail.getRecipientEmailAddresses().size()));
	}

	@Override
	public void sendBulkMail(@Nonnull List<Mail> mails) {
		LOGGER.debug("NoOpMailService ignored sendBulkMail(messageCount={})", Integer.valueOf(mails.size()));
	}

	@Override
	public @Nonnull MailDispatchOutcome dispatchMail(@Nonnull Mail mail) {
		sendMail(mail);
		return MailDispatchOutcome.skipped("noop", "NoOp mail service");
	}

	@Override
	public @Nonnull MailDispatchOutcome dispatchBulkMail(@Nonnull List<Mail> mails) {
		sendBulkMail(mails);
		return MailDispatchOutcome.skipped("noop", "NoOp mail service");
	}
}
