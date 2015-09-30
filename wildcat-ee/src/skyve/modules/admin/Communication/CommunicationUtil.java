package modules.admin.Communication;

import java.io.File;
import java.io.FileOutputStream;
import java.util.logging.Level;

import modules.admin.Communication.actions.GetResults;
import modules.admin.domain.Communication;
import modules.admin.domain.Communication.FormatType;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Job;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;
import org.skyve.util.Util;
import org.skyve.wildcat.content.AttachmentContent;
import org.skyve.wildcat.content.ContentManager;
import org.skyve.wildcat.util.UtilImpl;

public class CommunicationUtil {

	/**
	 * Whether to throw or log an exception if one occurs.
	 * 
	 * SILENT logs any exception stack EXPLICIT throws any exception
	 */
	public static enum ResponseMode {
		SILENT, EXPLICIT;
	}

	/**
	 * Whether to attempt the action requested, or simply test that the action
	 * can be performed.
	 * 
	 * ACTION attempts the requested action TEST tests whether the requested
	 * action can be performed (for example to test whether a communication
	 * message is valid and can be bound with the beans provided)
	 */
	public static enum RunMode {
		ACTION, TEST;
	}

	/**
	 * Sends a communication, binding sendTo, subject and body from the beans
	 * 
	 * @param communication
	 * @param runMode
	 * @param responseMode
	 * @param beans
	 * @throws Exception
	 */
	public static void send(Communication communication, RunMode runMode, ResponseMode responseMode, Bean... beans) throws Exception {

		Persistence pers = CORE.getPersistence();
		User user = pers.getUser();
		Customer customer = user.getCustomer();

		String sendTo = Binder.formatMessage(customer, communication.getSendTo(), beans);
		String[] sendOverride = new String[] { sendTo };

		sendOverrideTo(communication, runMode, responseMode, sendOverride, beans);
	}

	/**
	 * Generates a file corresponding to the communication, e.g. creating .eml
	 * files on the filesystem
	 * 
	 * @param communication
	 * @param runMode
	 * @param responseMode
	 * @param beans
	 * @throws Exception
	 */
	public static void generate(Communication communication, RunMode runMode, ResponseMode responseMode, Bean... beans) throws Exception {

		Persistence pers = CORE.getPersistence();
		User user = pers.getUser();
		Customer customer = user.getCustomer();

		String sendFrom = communication.getSendFrom();
		if (sendFrom == null && Boolean.TRUE.equals(communication.getSystem())) {
			// use the SMTP Sender property
			sendFrom = UtilImpl.SMTP_SENDER;
		}

		try {
			validatePath(communication);
		} catch (Exception e) {
			if (ResponseMode.SILENT.equals(responseMode)) {
				Util.LOGGER.log(Level.WARNING, e.getStackTrace().toString());
			} else {
				throw e;
			}
		}

		String sendTo = Binder.formatMessage(customer, communication.getSendTo(), beans);
		String emailSubject = Binder.formatMessage(customer, communication.getSubject(), beans);
		String emailBody = communication.getBody();
		emailBody = Binder.formatMessage(customer, emailBody, beans);

		try (FileOutputStream fos = new FileOutputStream(communication.getFilePath())) {

			// Generate file name - Communication_Description_To
			StringBuilder fileName = new StringBuilder();
			fileName.append(Communication.descriptionPropertyName);
			fileName.append('_');
			fileName.append(communication.getDescription());
			fileName.append('_');
			fileName.append(sendTo).append(".eml");

			// add attachment
			if (communication.getAttachment() != null) {

				try (ContentManager cm = EXT.newContentManager()) {
					AttachmentContent content = cm.get(communication.getAttachment());
					byte[] fileBytes = content.getContentBytes();
					String attachmentName = (communication.getAttachmentFileName() == null ? "attachment" : communication.getAttachmentFileName());
					if (RunMode.ACTION.equals(runMode)) {
						EXT.writeMail(new String[] { sendTo }, null, sendFrom, emailSubject, htmlEnclose(emailBody), MimeType.html, attachmentName, fileBytes, content.getMimeType(), fos);
					}
				}
			} else {
				if (RunMode.ACTION.equals(runMode)) {
					EXT.writeMail(new String[] { sendTo }, null, sendFrom, emailSubject, htmlEnclose(emailBody), MimeType.html, null, null, null, fos);
				}
			}

			fos.flush();
		} catch (Exception e) {
			if (ResponseMode.SILENT.equals(responseMode)) {
				Util.LOGGER.log(Level.WARNING, e.getStackTrace().toString());
			} else {
				throw e;
			}
		}

	}

	/**
	 * Sends the communication, but overrides the communication sendTo attribute
	 * 
	 * @param communication
	 * @param runMode
	 * @param responseMode
	 * @param sendTo
	 * @param beans
	 * @throws Exception
	 */
	public static void sendOverrideTo(Communication communication, RunMode runMode, ResponseMode responseMode, String[] sendTo, Bean... beans) throws Exception {

		Persistence pers = CORE.getPersistence();
		User user = pers.getUser();
		Customer customer = user.getCustomer();

		String sendFrom = communication.getSendFrom();
		if (sendFrom == null && Boolean.TRUE.equals(communication.getSystem())) {
			// use the SMTP Sender property
			sendFrom = UtilImpl.SMTP_SENDER;
		}

		String emailSubject = Binder.formatMessage(customer, communication.getSubject(), beans);
		String emailBody = communication.getBody();
		emailBody = Binder.formatMessage(customer, emailBody, beans);

		try {

			if (RunMode.ACTION.equals(runMode)) {
				EXT.sendMail(sendTo, null, sendFrom, emailSubject, emailBody, MimeType.html, null, null, null);
			}
		} catch (Exception e) {
			if (ResponseMode.SILENT.equals(responseMode)) {
				Util.LOGGER.log(Level.WARNING, e.getStackTrace().toString());
			}
		}
	}

	/**
	 * Sends the communication, but overrides the communication sendTo attribute
	 * with a string array formed from the sendTo parameter
	 * 
	 * @param communication
	 * @param runMode
	 * @param responseMode
	 * @param sendTo
	 * @param beans
	 * @throws Exception
	 */
	public static void sendOverrideTo(Communication communication, RunMode runMode, ResponseMode responseMode, String sendTo, Bean... beans) throws Exception {
		String[] sendToArray = new String[] { sendTo };
		sendOverrideTo(communication, runMode, responseMode, sendToArray, beans);
	}

	/**
	 * Validates the nominated path exists and/or can be created
	 * 
	 * @param communication
	 * @throws Exception
	 */
	public static void validatePath(Communication communication) throws Exception {
		if (communication == null) {
			throw new Exception("There communication item is null.");
		}

		String path = communication.getFilePath();
		if (path == null) {
			throw new ValidationException(new Message(Communication.filePathPropertyName, "The directory is empty - you must specify where reports can be created."));
		}
		File reportDir = new File(path);
		if (!reportDir.exists()) {
			reportDir.mkdirs();
			if (!reportDir.exists()) {
				throw new ValidationException(new Message(Communication.filePathPropertyName, "The directory " + reportDir.getAbsolutePath() + " does not exist & can not be created."));
			}
		}

		if (!reportDir.isDirectory()) {
			throw new ValidationException(new Message(Communication.filePathPropertyName, "The directory " + reportDir.getAbsolutePath() + " is not a directory."));
		}
	}

	/**
	 * Returns a safe filename from the unsafeName
	 * 
	 * @param unsafeName
	 * @return
	 */
	public static String fileNameSafe(String unsafeName) {

		return unsafeName.replace(',', '_').replace('&', '_').replace('/', '_').replace('\\', '_').replace(" ", "");
	}

	/**
	 * Returns a String with wrapped with <html><body> tags
	 * 
	 * @param html
	 * @return
	 */
	public static String htmlEnclose(String html) {
		StringBuilder sb = new StringBuilder();
		sb.append("<html><body>").append(html).append("</body></html>");
		return sb.toString();
	}

	/**
	 * Retrieves a persisted system communication with the nominated description
	 * 
	 * @param description
	 * @return
	 * @throws Exception
	 */
	public static Communication getSystemCommunicationByDescription(String description) throws Exception {
		Persistence pers = CORE.getPersistence();
		DocumentQuery query = pers.newDocumentQuery(Communication.MODULE_NAME, Communication.DOCUMENT_NAME);
		query.getFilter().addEquals(Communication.systemPropertyName, Boolean.TRUE);
		query.getFilter().addEquals(Communication.descriptionPropertyName, description);

		Communication result = query.beanResult();

		return result;
	}

	/**
	 * Send a formatType communication to sendTo with subject and message using
	 * binding expressions relative to bean.
	 * 
	 * This is effectively a wrapper for EXT.sendMail using the SMTP Sender
	 * property
	 * 
	 * @param sendTo
	 * @param subject
	 * @param body
	 * @param bean
	 * @param responseMode
	 * @param formatType
	 * @throws Exception
	 */
	public static void sendSimpleBeanCommunication(String sendTo, String subject, String body, Bean bean, ResponseMode responseMode, FormatType formatType) throws Exception {
		Communication c = Communication.newInstance();

		c.setDescription("Simple Bean Communication");
		c.setSendTo(sendTo);
		c.setModuleName(bean.getBizModule());
		c.setDocumentName(bean.getBizDocument());
		c.setSubject(subject);
		c.setBody(body);
		c.setFormatType(formatType);
		c.setSystem(Boolean.TRUE);

		send(c, RunMode.ACTION, responseMode, bean);
	}

	/**
	 * wraps retrieving the communication by description and sends it
	 * 
	 * @param description
	 * @param responseMode
	 * @param bean
	 * @throws Exception
	 */
	public static void sendSystemCommunicationByDescription(String description, ResponseMode responseMode, Bean bean) throws Exception {
		Communication c = getSystemCommunicationByDescription(description);
		send(c, RunMode.ACTION, responseMode, bean);
	}

	/**
	 * Kicks off a one-shot-job which is implied by the attributes set in the
	 * communication.
	 * 
	 * Using the tag, format and action type, run the one shot job to either
	 * test, send or generate the communication files.
	 * 
	 * @param communication
	 * @return
	 * @throws Exception
	 */
	public static Communication kickOffJob(Communication communication) throws Exception {

		String results = GetResults.getResults(communication);

		Persistence persistence = CORE.getPersistence();
		User user = persistence.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(Communication.MODULE_NAME);
		Job job = module.getJob("jProcessCommunicationsForTag");

		EXT.runOneShotJob(job, communication, user);

		StringBuilder sb = new StringBuilder(results);
		sb.append("\nThe job has been commenced - check Admin->Jobs for the log.");
		communication.setResults(sb.toString());

		communication.originalValues().remove(Communication.resultsPropertyName);

		return communication;
	}

}
