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
 * A proxy that can be Serialized with no state and continue to work after deserialization.
 */
@Alternative
public class MailServiceInjectable implements MailService, Serializable {
	private static final long serialVersionUID = 1022932117847703726L;

	@Override
	public void writeMail(Mail mail, OutputStream out) {
		EXT.getMailService().writeMail(mail, out);
	}

	@Override
	public void sendMail(Mail mail) {
		EXT.getMailService().sendMail(mail);
	}

	@Override
	public void sendBulkMail(List<Mail> mails) {
		EXT.getMailService().sendBulkMail(mails);
	}

	@Override
	public MailDispatchOutcome dispatchMail(Mail mail) {
		return EXT.getMailService().dispatchMail(mail);
	}

	@Override
	public MailDispatchOutcome dispatchBulkMail(List<Mail> mails) {
		return EXT.getMailService().dispatchBulkMail(mails);
	}
}
