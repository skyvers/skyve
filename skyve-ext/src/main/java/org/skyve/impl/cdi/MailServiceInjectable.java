package org.skyve.impl.cdi;

import java.io.OutputStream;
import java.io.Serializable;
import java.util.List;

import org.skyve.EXT;
import org.skyve.util.Mail;
import org.skyve.util.MailDispatchOutcome;
import org.skyve.util.MailService;

import jakarta.enterprise.inject.Alternative;

/**
 * Stateless CDI proxy for {@link MailService}.
 *
 * <p>Delegates all operations to {@link EXT#getMailService()} so a serialized
 * reference remains valid across session passivation/activation.
 */
@Alternative
public class MailServiceInjectable implements MailService, Serializable {
	private static final long serialVersionUID = 1022932117847703726L;

	/**
	 * Writes the MIME payload for the supplied mail object.
	 *
	 * @param mail the mail definition to render.
	 * @param out the output stream receiving the MIME payload.
	 */
	@Override
	public void writeMail(Mail mail, OutputStream out) {
		EXT.getMailService().writeMail(mail, out);
	}

	/**
	 * Sends a single mail message.
	 *
	 * <p>Side effects: performs outbound mail delivery through the configured mail service.
	 *
	 * @param mail the mail definition to send.
	 */
	@Override
	public void sendMail(Mail mail) {
		EXT.getMailService().sendMail(mail);
	}

	/**
	 * Sends multiple mail messages in a single dispatch operation.
	 *
	 * <p>Side effects: performs outbound mail delivery for each entry in {@code mails}.
	 *
	 * @param mails the mail definitions to send.
	 */
	@Override
	public void sendBulkMail(List<Mail> mails) {
		EXT.getMailService().sendBulkMail(mails);
	}

	/**
	 * Dispatches a single message and returns delivery telemetry.
	 *
	 * @param mail the mail definition to dispatch.
	 * @return the dispatch outcome for the submitted message.
	 */
	@Override
	public MailDispatchOutcome dispatchMail(Mail mail) {
		return EXT.getMailService().dispatchMail(mail);
	}

	/**
	 * Dispatches multiple messages and returns aggregate delivery telemetry.
	 *
	 * @param mails the mail definitions to dispatch.
	 * @return the aggregate dispatch outcome for all submitted messages.
	 */
	@Override
	public MailDispatchOutcome dispatchBulkMail(List<Mail> mails) {
		return EXT.getMailService().dispatchBulkMail(mails);
	}
}
