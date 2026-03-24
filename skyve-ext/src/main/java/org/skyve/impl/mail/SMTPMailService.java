package org.skyve.impl.mail;

import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.Map.Entry;
import java.util.List;
import java.util.Properties;
import java.util.Set;

import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.Mail;
import org.skyve.util.MailAttachment;
import org.skyve.util.MailDispatchOutcome;
import org.skyve.util.MailService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.annotation.Nonnull;
import jakarta.activation.DataHandler;
import jakarta.activation.DataSource;
import jakarta.mail.Address;
import jakarta.mail.Message;
import jakarta.mail.MessagingException;
import jakarta.mail.Multipart;
import jakarta.mail.PasswordAuthentication;
import jakarta.mail.Session;
import jakarta.mail.Transport;
import jakarta.mail.internet.AddressException;
import jakarta.mail.internet.InternetAddress;
import jakarta.mail.internet.MimeBodyPart;
import jakarta.mail.internet.MimeMessage;
import jakarta.mail.internet.MimeMultipart;
import jakarta.mail.util.ByteArrayDataSource;

/**
 * SMTP mail service implementation.
 */
public class SMTPMailService implements MailService {
	private static final Logger LOGGER = LoggerFactory.getLogger(SMTPMailService.class);

	@Override
	public void writeMail(@Nonnull Mail mail, @Nonnull OutputStream out) {
		try {
			MimeMessage message = createMail(mail, true);
			message.writeTo(out);
		}
		catch (Exception e) {
			LOGGER.error("Email was not written", e);
			throw new ValidationException(new org.skyve.domain.messages.Message("Email was not written..."));
		}
	}

	@Override
	public void sendMail(@Nonnull Mail mail) {
		try {
			MimeMessage message = createMail(mail, false);
			Transport.send(message);
		}
		catch (Exception e) {
			LOGGER.error("Email was not sent", e);
			throw new ValidationException(new org.skyve.domain.messages.Message("Email was not sent..."));
		}
	}

	@Override
	public @Nonnull MailDispatchOutcome dispatchMail(@Nonnull Mail mail) {
		sendMail(mail);
		return MailDispatchOutcome.sent("smtp");
	}

	@Override
	public void sendBulkMail(@Nonnull List<Mail> mails) {
		for (Mail mail : mails) {
			sendMail(mail);
		}
	}

	@Override
	public @Nonnull MailDispatchOutcome dispatchBulkMail(@Nonnull List<Mail> mails) {
		sendBulkMail(mails);
		return MailDispatchOutcome.sent("smtp");
	}

	private static MimeMessage createMail(Mail mail, boolean forWriting) throws AddressException, MessagingException {
		Set<String> recipientEmailAddresses = mail.getRecipientEmailAddresses();
		Set<String> ccEmailAddresses = mail.getCcEmailAddresses();
		Set<String> bccEmailAddresses = mail.getBccEmailAddresses();
		String senderEmailAddress = mail.getSenderEmailAddress();
		String subject = mail.getSubject();
		String body = mail.getBody();
		Map<String, String> headers = mail.getHeaders();
		List<MailAttachment> attachments = mail.getAttachments();

		LOGGER.info("@@@@@@@@@@@@ EMAIL @@@@@@@@@@@@");
		LOGGER.info("TO:");
		for (String to : recipientEmailAddresses) {
			LOGGER.info("    {}", to);
		}
		LOGGER.info("CC:");
		for (String cc : ccEmailAddresses) {
			LOGGER.info("    {}", cc);
		}
		LOGGER.info("BCC:");
		for (String bcc : bccEmailAddresses) {
			LOGGER.info("    {}", bcc);
		}
		LOGGER.info("SENDER: {}", senderEmailAddress);
		LOGGER.info("SUBJECT {}", subject);
		LOGGER.info("BODY {}", body);
		LOGGER.info("CONTENT TYPE: {}", mail.getContentType());
		LOGGER.info("@@@@@@@@@@@@ EMAIL @@@@@@@@@@@@");

		Session session;
		if (UtilImpl.processStringValue(UtilImpl.SMTP_UID) == null) {
			Properties props = new Properties();
			props.setProperty("mail.smtp.auth", "false");
			props.setProperty("mail.smtp.port", String.valueOf(UtilImpl.SMTP_PORT));
			props.setProperty("mail.smtp.host", UtilImpl.SMTP);
			if (UtilImpl.SMTP_PROPERTIES != null) {
				for (Entry<String, String> entry : UtilImpl.SMTP_PROPERTIES.entrySet()) {
					props.setProperty(entry.getKey(), entry.getValue());
				}
			}
			session = Session.getInstance(props);
		}
		else {
			Properties props = System.getProperties();
			props.setProperty("mail.smtp.auth", "true");
			props.setProperty("mail.smtp.port", String.valueOf(UtilImpl.SMTP_PORT));
			props.setProperty("mail.smtp.host", UtilImpl.SMTP);
			if (UtilImpl.SMTP_PROPERTIES != null) {
				for (Entry<String, String> entry : UtilImpl.SMTP_PROPERTIES.entrySet()) {
					props.setProperty(entry.getKey(), entry.getValue());
				}
			}
			session = Session.getInstance(props, new Authenticator());
		}

		MimeMessage message = new MimeMessage(session);
		InternetAddress senderAddress = new InternetAddress(senderEmailAddress);
		message.setFrom(senderAddress);
		message.setReplyTo(new Address[] { senderAddress });
		message.setSender(forWriting ? senderAddress : new InternetAddress(UtilImpl.SMTP_SENDER));
		addAddresses(message, bccEmailAddresses, Message.RecipientType.BCC);
		addAddresses(message, ccEmailAddresses, Message.RecipientType.CC);
		addAddresses(message, recipientEmailAddresses, Message.RecipientType.TO);
		message.setSubject(subject);

		MimeBodyPart messageBodyPart = new MimeBodyPart();
		Multipart multipart = new MimeMultipart();

		String type = mail.getContentType().toString() + "; charset=" + StandardCharsets.UTF_8.name();
		messageBodyPart.setContent(body, type);
		multipart.addBodyPart(messageBodyPart);

		if (UtilImpl.SMTP_HEADERS != null) {
			for (Entry<String, String> entry : UtilImpl.SMTP_HEADERS.entrySet()) {
				message.setHeader(entry.getKey(), entry.getValue());
			}
		}
		if (headers != null) {
			for (Entry<String, String> header : headers.entrySet()) {
				message.setHeader(header.getKey(), header.getValue());
			}
		}

		if (attachments != null) {
			for (MailAttachment attachment : attachments) {
				MimeBodyPart bodyPart = addAttachment(attachment);
				if (bodyPart != null) {
					multipart.addBodyPart(bodyPart);
				}
			}
		}

		message.setContent(multipart);
		return message;
	}

	private static void addAddresses(MimeMessage message, Set<String> addresses, Message.RecipientType type)
	throws AddressException, MessagingException {
		if (addresses != null) {
			for (String address : addresses) {
				message.addRecipient(type, new InternetAddress(address));
			}
		}
	}

	private static MimeBodyPart addAttachment(MailAttachment mailAttachment) throws MessagingException {
		MimeBodyPart result = null;

		if (mailAttachment != null) {
			byte[] bytes = mailAttachment.getAttachment();
			if (bytes != null) {
				result = new MimeBodyPart();
				DataSource source = new ByteArrayDataSource(bytes, mailAttachment.getAttachmentContentType());
				result.setDataHandler(new DataHandler(source));
				result.setFileName(mailAttachment.getAttachmentFileName());
			}
		}

		return result;
	}

	private static class Authenticator extends jakarta.mail.Authenticator {
		private final PasswordAuthentication authentication;

		private Authenticator() {
			authentication = new PasswordAuthentication(UtilImpl.SMTP_UID, UtilImpl.SMTP_PWD);
		}

		@Override
		protected PasswordAuthentication getPasswordAuthentication() {
			return authentication;
		}
	}
}
