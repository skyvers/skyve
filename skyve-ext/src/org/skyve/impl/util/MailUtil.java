package org.skyve.impl.util;

import java.io.OutputStream;
import java.util.Properties;
import java.util.logging.Level;

import javax.activation.DataHandler;
import javax.activation.DataSource;
import javax.mail.Address;
import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.Multipart;
import javax.mail.PasswordAuthentication;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.AddressException;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeBodyPart;
import javax.mail.internet.MimeMessage;
import javax.mail.internet.MimeMultipart;
import javax.mail.util.ByteArrayDataSource;

import org.skyve.content.MimeType;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.MailAttachment;

public class MailUtil {
	private MailUtil() {
		// no-op
	}

	public static final void writeMail(String[] recipientEmailAddresses,
											String[] ccEmailAddresses,
											String[] bccEmailAddresses,
											String senderEmailAddress,
											String subject,
											String body,
											MimeType contentType, 
											OutputStream out,
											MailAttachment... attachments)
	throws ValidationException {
		try {
			MimeMessage message = createMail(recipientEmailAddresses,
												ccEmailAddresses,
												bccEmailAddresses,
												senderEmailAddress,
												subject,
												body,
												contentType,
												true,
												attachments);
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

	public static final void sendMail(String[] recipientEmailAddresses,
										String[] ccEmailAddresses,
										String[] bccEmailAddresses,
										String senderEmailAddress,
										String subject,
										String body,
										MimeType contentType, 
										MailAttachment... attachments)
	throws ValidationException {
		try {
			MimeMessage message = createMail(recipientEmailAddresses,
												ccEmailAddresses,
												bccEmailAddresses,
												senderEmailAddress,
												subject,
												body,
												contentType,
												false,
												attachments);
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
	
	private static final MimeMessage createMail(String[] recipientEmailAddresses,
													String[] ccEmailAddresses,
													String[] bccEmailAddresses,
													String senderEmailAddress,
													String subject,
													String body,
													MimeType contentType,
													boolean forWriting,
													MailAttachment... attachments)
	throws AddressException, MessagingException {
		UtilImpl.LOGGER.info("@@@@@@@@@@@@ EMAIL @@@@@@@@@@@@");
		UtilImpl.LOGGER.info("TO:");
		if (UtilImpl.SMTP_TEST_RECIPIENT != null) {
			UtilImpl.LOGGER.info("    SMTP_TEST_RECIPIENT IN WEB.XML - " + UtilImpl.SMTP_TEST_RECIPIENT);
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
			Properties props = System.getProperties();
			props.setProperty("mail.smtp.auth", "false");
			props.setProperty("mail.smtp.port", UtilImpl.SMTP_PORT);
			props.setProperty("mail.smtp.host", UtilImpl.SMTP);
			session = Session.getInstance(props);
		}
		else {
			Authenticator authenticator = new Authenticator();
			Properties props = System.getProperties();
			props.setProperty("mail.smtp.submitter", authenticator.getPasswordAuthentication().getUserName());
			props.setProperty("mail.smtp.auth", "true");
			props.setProperty("mail.smtp.port", UtilImpl.SMTP_PORT);
			props.setProperty("mail.smtp.host", UtilImpl.SMTP);
			session = Session.getInstance(props, authenticator);
		}

		// Define message
		MimeMessage message = new MimeMessage(session);
		InternetAddress senderAddress = new InternetAddress(senderEmailAddress);
		message.setFrom(senderAddress);
		message.setReplyTo(new Address[] {senderAddress});
		message.setSender(forWriting ? senderAddress : new InternetAddress(UtilImpl.SMTP_SENDER));
		
		if (UtilImpl.SMTP_TEST_RECIPIENT != null) {
			addAddresses(message, new String[] {UtilImpl.SMTP_TEST_RECIPIENT}, Message.RecipientType.TO);
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
		messageBodyPart.setContent(body, contentType.toString());
		multipart.addBodyPart(messageBodyPart);

		// add attachments
		for(MailAttachment attachment: attachments){
			MimeBodyPart bodyPart = addAttachment(attachment);
			if(null != bodyPart) {
				multipart.addBodyPart(bodyPart);
			}
		}

		// Put all parts into the message
		message.setContent(multipart);

		return message;
	}

	private static final void addAddresses(MimeMessage message, String[] addresses, 
			Message.RecipientType type) throws AddressException, MessagingException {
		
		if(null == addresses) {
			return;
		}
		
		for(String address : addresses) {
			message.addRecipient(type, new InternetAddress(address));
		}
	}
	
	private static final MimeBodyPart addAttachment(MailAttachment mailAttachment) throws MessagingException {
		
		// if there is an attachement to send 
		if(null != mailAttachment && mailAttachment.getAttachment()!=null) {
			
			// add attachment
			MimeBodyPart messageBodyPart = new MimeBodyPart();
			DataSource source = new ByteArrayDataSource(mailAttachment.getAttachment(),
					mailAttachment.getAttachmentMimeType().toString());
			messageBodyPart.setDataHandler(new DataHandler(source));
			messageBodyPart.setFileName(mailAttachment.getAttachmentFileName());
			return messageBodyPart;
		}
		
		return  null;
	}

	private static class Authenticator extends javax.mail.Authenticator {
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
	}}
