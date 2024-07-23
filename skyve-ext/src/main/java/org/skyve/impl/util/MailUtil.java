package org.skyve.impl.util;

import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.Set;
import java.util.logging.Level;

import org.skyve.content.MimeType;
import org.skyve.domain.messages.ValidationException;
import org.skyve.util.Mail;
import org.skyve.util.MailAttachment;

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

public class MailUtil {
	private MailUtil() {
		// no-op
	}

	public static final void writeMail(Mail mail, OutputStream out) {
		try {
			MimeMessage message = createMail(mail, true);
			// Write the message
			if (! UtilImpl.SMTP_TEST_BOGUS_SEND) {
				message.writeTo(out);
			}
		}
		catch (Exception e) {
			UtilImpl.LOGGER.log(Level.SEVERE, "Email was not written", e);
			throw new ValidationException(new org.skyve.domain.messages.Message("Email was not written..."));
		}
	}

	public static final void sendMail(Mail mail) {
		try {
			MimeMessage message = createMail(mail, false);
			// Send the message
			if (! UtilImpl.SMTP_TEST_BOGUS_SEND) { // if we are not in test mode
				Transport.send(message);
			}
		}
		catch (Exception e) {
			UtilImpl.LOGGER.log(Level.SEVERE, "Email was not sent", e);
			throw new ValidationException(new org.skyve.domain.messages.Message("Email was not sent..."));
		}
	}
	
	private static final MimeMessage createMail(Mail mail, boolean forWriting)
	throws AddressException, MessagingException {
		Set<String> recipientEmailAddresses = mail.getRecipientEmailAddresses();
		Set<String> ccEmailAddresses = mail.getCcEmailAddresses();
		Set<String> bccEmailAddresses = mail.getBccEmailAddresses();
		String senderEmailAddress = mail.getSenderEmailAddress();
		String subject = mail.getSubject();
		String body = mail.getBody();
		MimeType contentType = mail.getContentType();
		Map<String, String> headers = mail.getHeaders();
		List<MailAttachment> attachments = mail.getAttachments();
		
		UtilImpl.LOGGER.info("@@@@@@@@@@@@ EMAIL @@@@@@@@@@@@");
		UtilImpl.LOGGER.info("TO:");
		if (UtilImpl.SMTP_TEST_RECIPIENT != null) {
			UtilImpl.LOGGER.info("    SMTP_TEST_RECIPIENT - " + UtilImpl.SMTP_TEST_RECIPIENT);
		}
		else {
			if (recipientEmailAddresses != null) {
				for (String to : recipientEmailAddresses) {
					UtilImpl.LOGGER.info("    " + to);
				}
			}
		}
		UtilImpl.LOGGER.info("CC:");
		if (UtilImpl.SMTP_TEST_RECIPIENT == null) {
			UtilImpl.LOGGER.info("    " + UtilImpl.SMTP_TEST_RECIPIENT);
			if (ccEmailAddresses != null) {
				for (String cc : ccEmailAddresses) {
					UtilImpl.LOGGER.info("    " + cc);
				}
			}
		}
		UtilImpl.LOGGER.info("BCC:");
		if (UtilImpl.SMTP_TEST_RECIPIENT == null) {
			UtilImpl.LOGGER.info("    " + UtilImpl.SMTP_TEST_RECIPIENT);
			if (bccEmailAddresses != null) {
				for (String bcc : bccEmailAddresses) {
					UtilImpl.LOGGER.info("    " + bcc);
				}
			}
		}

		UtilImpl.LOGGER.info("SENDER: " + senderEmailAddress);
		UtilImpl.LOGGER.info("SUBJECT " + subject);
		UtilImpl.LOGGER.info("BODY " + body);
		UtilImpl.LOGGER.info("CONTENT TYPE: " + contentType);
		UtilImpl.LOGGER.info("@@@@@@@@@@@@ EMAIL @@@@@@@@@@@@");

		// Get system properties and add our mail server
		Session session = null;
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
			Authenticator authenticator = new Authenticator();
			Properties props = System.getProperties();
			props.setProperty("mail.smtp.auth", "true");
			props.setProperty("mail.smtp.port", String.valueOf(UtilImpl.SMTP_PORT));
			props.setProperty("mail.smtp.host", UtilImpl.SMTP);
			if (UtilImpl.SMTP_PROPERTIES != null) {
				for (Entry<String, String> entry : UtilImpl.SMTP_PROPERTIES.entrySet()) {
					props.setProperty(entry.getKey(), entry.getValue());
				}
			}
			session = Session.getInstance(props, authenticator);
		}

		// Define message
		MimeMessage message = new MimeMessage(session);
		InternetAddress senderAddress = new InternetAddress(senderEmailAddress);
		message.setFrom(senderAddress);
		message.setReplyTo(new Address[] {senderAddress});
		message.setSender(forWriting ? senderAddress : new InternetAddress(UtilImpl.SMTP_SENDER));
		
		if (UtilImpl.SMTP_TEST_RECIPIENT != null) {
			addAddresses(message, Collections.singleton(UtilImpl.SMTP_TEST_RECIPIENT), Message.RecipientType.TO);
		}
		else {
			addAddresses(message, bccEmailAddresses, Message.RecipientType.BCC);
			addAddresses(message, ccEmailAddresses, Message.RecipientType.CC);
			addAddresses(message, recipientEmailAddresses, Message.RecipientType.TO);
		}		
		message.setSubject(subject);

		// create the message part
		MimeBodyPart messageBodyPart = new MimeBodyPart();

		// fill message
		Multipart multipart = new MimeMultipart();
		
		// informs the java email libraries how to encode the text.
		// This is a fix for being able to email Japanese characters (and will likely fix some other languages as well)
		// tyoe = html/text; charset=UTF-8, 
		String type = contentType.toString() + "; charset=" + StandardCharsets.UTF_8.name();
		messageBodyPart.setContent(body, type);
		multipart.addBodyPart(messageBodyPart);

		// add headers
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

		// add attachments
		if (attachments != null) {
			for (MailAttachment attachment: attachments) {
				MimeBodyPart bodyPart = addAttachment(attachment);
				if (bodyPart != null) {
					multipart.addBodyPart(bodyPart);
				}
			}
		}
		
		// Put all parts into the message
		message.setContent(multipart);

		return message;
	}

	private static final void addAddresses(MimeMessage message, Set<String> addresses, Message.RecipientType type)
	throws AddressException, MessagingException {
		if (addresses != null) {
			for (String address : addresses) {
				message.addRecipient(type, new InternetAddress(address));
			}
		}
	}
	
	private static final MimeBodyPart addAttachment(MailAttachment mailAttachment)
	throws MessagingException {
		MimeBodyPart result = null;
		
		// if there is an attachment to send 
		if (mailAttachment != null) {
			byte[] bytes = mailAttachment.getAttachment();
			if (bytes != null) {
				// add attachment
				result = new MimeBodyPart();
				DataSource source = new ByteArrayDataSource(bytes, mailAttachment.getAttachmentMimeType().toString());
				result.setDataHandler(new DataHandler(source));
				result.setFileName(mailAttachment.getAttachmentFileName());
			}
		}
		
		return result;
	}

	private static class Authenticator extends jakarta.mail.Authenticator {
		private PasswordAuthentication authentication;

		public Authenticator() {
			String username = UtilImpl.SMTP_UID;
			String password = UtilImpl.SMTP_PWD;
			authentication = new PasswordAuthentication(username, password);
		}

		@Override
		protected PasswordAuthentication getPasswordAuthentication() {
			return authentication;
		}
	}
}
