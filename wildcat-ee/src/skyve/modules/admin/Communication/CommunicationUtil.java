package modules.admin.Communication;

import java.io.FileOutputStream;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.logging.Level;

import modules.admin.Communication.actions.GetResults;
import modules.admin.domain.Communication;
import modules.admin.domain.Communication.FormatType;
import modules.admin.domain.Subscription;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Job;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;
import org.skyve.util.Util;
import org.skyve.wildcat.bind.BindUtil;
import org.skyve.wildcat.content.AttachmentContent;
import org.skyve.wildcat.content.ContentManager;
import org.skyve.wildcat.util.FileUtil;
import org.skyve.wildcat.util.MailAttachment;
import org.skyve.wildcat.util.TimeUtil;
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
		Module module = customer.getModule(Communication.MODULE_NAME);
		Document document = module.getDocument(customer, Communication.DOCUMENT_NAME);
		Document subDoc = module.getDocument(customer, Subscription.DOCUMENT_NAME);

		String sendTo = Binder.formatMessage(customer, communication.getSendTo(), beans);
		FormatType format = communication.getFormatType();

		// check for Subscription
		DocumentQuery q = pers.newDocumentQuery(Subscription.MODULE_NAME, Subscription.DOCUMENT_NAME);
		q.getFilter().addEquals(Subscription.receiverIdentifierPropertyName, sendTo);
		Subscription subscription = q.beanResult();

		if (subscription != null) {
			// check for declined
			if (Boolean.TRUE.equals(subscription.getDeclined())) {
				StringBuilder msg = new StringBuilder(128);
				if (subscription.getFormatType() == null || communication.getFormatType().equals(subscription.getFormatType())) {
					msg.append(document.getSingularAlias()).append(" prevented because the recipient ");
					msg.append(sendTo).append(" has a ").append(subDoc.getSingularAlias());
					msg.append(" set ").append(Subscription.declinedPropertyName);
					if (subscription.getFormatType() != null) {
						msg.append(" for ").append(subscription.getFormatType().toDescription());
					}

					// block the communication
					Util.LOGGER.info(msg.toString());
					throw new Exception(msg.toString());
				}
			} else {
				format = subscription.getFormatType();
				sendTo = subscription.getPreferredReceiverIdentifier();
				Util.LOGGER.info(document.getSingularAlias() + " redirected to " + format.toDescription() + ' ' + sendTo);
			}
		}

		String sendFrom = communication.getSendFrom();
		if (sendFrom == null && Boolean.TRUE.equals(communication.getSystem())) {
			// use the SMTP Sender property
			sendFrom = UtilImpl.SMTP_SENDER;
		}

		String emailSubject = Binder.formatMessage(customer, communication.getSubject(), beans);
		String emailBodyMain = communication.getBody();
		emailBodyMain = Binder.formatMessage(customer, emailBodyMain, beans);

		// calendar items
		StringBuilder emailBody = new StringBuilder(emailBodyMain);
		CommunicationCalendarItem calendarItem = null;
		if (Boolean.TRUE.equals(communication.getIncludeCalendar())) {
			calendarItem = generateCalendarAttachments(customer, communication, beans);
			if (calendarItem != null) {
				emailBody.append("\n").append(calendarItem.getGoogleCalendarLink());
				emailBody.append("\n").append(calendarItem.getYahooCalendarLink());
			}
		}

		// Generate file name - Communication_Description_To
		StringBuilder subFolder = new StringBuilder();
		SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMddHHmmss");
		subFolder.append(sdf.format(new Date()));

		String customerName = CORE.getUser().getCustomerName();
		String batchDirPrefix = UtilImpl.CONTENT_DIRECTORY + "batch_" + customerName;

		String filePath = FileUtil.constructSafeFilePath(batchDirPrefix, sendTo, ".eml", true, subFolder.toString());

		try (FileOutputStream fos = new FileOutputStream(filePath)) {

			// add attachments

			try (ContentManager cm = EXT.newContentManager()) {

				MailAttachment[] attachments = getAttachments(cm, communication, calendarItem);

				if (RunMode.ACTION.equals(runMode)) {
					switch (format) {
					case email:
						EXT.writeMail(new String[] { sendTo }, null, sendFrom, emailSubject, htmlEnclose(emailBody.toString()), MimeType.html, fos, attachments);
						break;
					default:
						break;
					}
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
		String emailBodyMain = communication.getBody();
		emailBodyMain = Binder.formatMessage(customer, emailBodyMain, beans);

		// calendar items
		StringBuilder emailBody = new StringBuilder(emailBodyMain);
		CommunicationCalendarItem calendarItem = null;
		if (Boolean.TRUE.equals(communication.getIncludeCalendar())) {
			calendarItem = generateCalendarAttachments(customer, communication, beans);
			if (calendarItem != null) {
				emailBody.append("\n").append(calendarItem.getGoogleCalendarLink());
				emailBody.append("\n").append(calendarItem.getYahooCalendarLink());
			}
		}

		try (ContentManager cm = EXT.newContentManager()) {

			MailAttachment[] attachments = getAttachments(cm, communication, calendarItem);

			try {
				if (RunMode.ACTION.equals(runMode)) {
					EXT.sendMail(sendTo, null, sendFrom, emailSubject, emailBody.toString(), MimeType.html, attachments);
				}
			} catch (Exception e) {
				if (ResponseMode.SILENT.equals(responseMode)) {
					Util.LOGGER.log(Level.WARNING, e.getStackTrace().toString());
				} else {
					throw e;
				}
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

	/**
	 * Generate calendar items for communication
	 * 
	 * returns links for google and yahoo calendar, and .ics file attachment
	 * 
	 * @param customer
	 * @param communication
	 * @param beans
	 * @throws Exception
	 */
	public static CommunicationCalendarItem generateCalendarAttachments(Customer customer, Communication communication, Bean... beans) throws Exception {
		CommunicationCalendarItem result = new CommunicationCalendarItem();

		String google = null;
		String yahoo = null;

		// set Title
		StringBuilder sb = new StringBuilder(512);
		// sb.append(row[1]).append(" - ").append(row[2]);
		String eventTitle = BindUtil.formatMessage(customer, communication.getCalendarTitleExpression(), beans);
		String eventDescription = BindUtil.formatMessage(customer, communication.getCalendarDescriptionExpression(), beans);

		String start = TimeUtil.formatISODate(communication.getCalendarStartTime(), true).replace(".000+00:00", "Z").replace("-", "").replace(":", "");
		String end = TimeUtil.formatISODate(communication.getCalendarEndTime(), true).replace(".000+00:00", "Z").replace("-", "").replace(":", "");

		sb.setLength(0);
		sb.append("<a href=\"");
		sb.append("https://www.google.com/calendar/render?action=TEMPLATE&text=");
		sb.append(eventTitle);
		sb.append("&dates=").append(start).append('/').append(end);
		sb.append("&details=").append(eventDescription);
		sb.append("&location=").append("My test location");
		sb.append("&location&pli=1&sf=true&output=xml#eventpage_6");
		sb.append("\">Add to Event Google Calendar</a>");
		google = sb.toString();

		sb.setLength(0);
		sb.append("<a href=\"");
		sb.append("https://calendar.yahoo.com/?v=60&view=d&type=20&title=");
		sb.append(eventTitle);
		sb.append("&st=").append(start);
		sb.append("&et=").append(end);
		// sb.append("&dur=").append(totalRunTime.toString().replace(":",
		// "").substring(0, 4));
		sb.append("&in_loc=").append("My test; location");
		sb.append("&desc=").append(eventDescription);
		sb.append("\">Add to Event Yahoo Calendar</a>");
		yahoo = sb.toString();

		sb.setLength(0);
		sb.append("BEGIN:VCALENDAR\n");
		sb.append("VERSION:2.0\n");
		sb.append("BEGIN:VEVENT\n");
		sb.append("URL:").append("").append('\n');
		sb.append("DTSTART:").append(start).append('\n');
		sb.append("DTEND:").append(end).append('\n');
		sb.append("SUMMARY:").append(eventTitle).append('\n');
		sb.append("DESCRIPTION:").append(eventDescription);
		sb.append('\n');
		sb.append("LOCATION:").append("My test location").append("\n");
		sb.append("END:VEVENT\n");
		sb.append("END:VCALENDAR\n");

		result.setGoogleCalendarLink(google);
		result.setYahooCalendarLink(yahoo);
		result.setIcsFileAttachment(sb.toString().getBytes("utf-8"));

		return result;
	}

	/**
	 * getAttachments - compile an array of all possible attachments for the
	 * communication iem
	 * 
	 * @param cm
	 * @param communication
	 * @param calendarItem
	 * @return
	 * @throws Exception
	 */
	public static MailAttachment[] getAttachments(ContentManager cm, Communication communication, CommunicationCalendarItem calendarItem) throws Exception {

		MailAttachment ma1 = null;
		MailAttachment ma2 = null;
		MailAttachment ma3 = null;
		MailAttachment ma4 = null;
		MailAttachment ma5 = null;

		// Attachment 1
		if (communication.getAttachmentFileName1() != null && communication.getAttachment1() != null) {
			AttachmentContent content1 = cm.get(communication.getAttachment1());
			byte[] fileBytes1 = content1.getContentBytes();
			String attachmentName1 = (communication.getAttachmentFileName1() == null ? "attachment" : communication.getAttachmentFileName1());
			ma1 = new MailAttachment(attachmentName1, fileBytes1, content1.getMimeType());
		}

		// Attachment 2
		if (communication.getAttachmentFileName2() != null && communication.getAttachment2() != null) {
			AttachmentContent content2 = cm.get(communication.getAttachment2());
			byte[] fileBytes2 = content2.getContentBytes();
			String attachmentName2 = (communication.getAttachmentFileName2() == null ? "attachment" : communication.getAttachmentFileName2());
			ma2 = new MailAttachment(attachmentName2, fileBytes2, content2.getMimeType());
		}

		// Attachment 3
		if (communication.getAttachmentFileName3() != null && communication.getAttachment3() != null) {
			AttachmentContent content3 = cm.get(communication.getAttachment3());
			byte[] fileBytes3 = content3.getContentBytes();
			String attachmentName3 = (communication.getAttachmentFileName3() == null ? "attachment" : communication.getAttachmentFileName3());
			ma3 = new MailAttachment(attachmentName3, fileBytes3, content3.getMimeType());
		}

		// Attachment calendar Items
		if (calendarItem != null) {
			ma4 = new MailAttachment("OutlookCalendarEvent.ics", calendarItem.getIcsFileAttachment(), MimeType.tex);
			ma5 = new MailAttachment("iCalCalendarEvent.ics", calendarItem.getIcsFileAttachment(), MimeType.tex);
		}

		// construct array
		MailAttachment[] attachments = { ma1, ma2, ma3, ma4, ma5 };

		return attachments;
	}
}