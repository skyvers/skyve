package modules.admin.Communication;

import java.io.FileOutputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.logging.Level;

import modules.ModulesUtil;
import modules.admin.Communication.actions.GetResults;
import modules.admin.domain.Communication;
import modules.admin.domain.Communication.FormatType;
import modules.admin.domain.Subscription;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.content.AttachmentContent;
import org.skyve.content.ContentManager;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.impl.util.TimeUtil;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;
import org.skyve.util.FileUtil;
import org.skyve.util.MailAttachment;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

public class CommunicationUtil {

	public static final String SPECIAL_BEAN_URL = "{#url}";
	public static final String SPECIAL_CONTEXT = "{#context}";
	public static final String DEFAULT_SENDER = "default.sender@skyve.org";
	public static final String SENT_SUCCESSFULLY_MESSAGE = "Communication sent";

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

	public static enum ActionType {
		FILE, SMTP;
	}

	/**
	 * actionCommunicationRequest
	 * 
	 * @param communication
	 * @param runMode
	 * @param responseMode
	 * @param beans
	 * @throws Exception
	 */
	private static String actionCommunicationRequest(ActionType actionType, Communication communication, RunMode runMode, ResponseMode responseMode, MailAttachment[] additionalAttachments, Bean... specificBeans) throws Exception {

		String result = null;

		Persistence pers = CORE.getPersistence();
		User user = pers.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(Communication.MODULE_NAME);
		Document document = module.getDocument(customer, Communication.DOCUMENT_NAME);
		Document subDoc = module.getDocument(customer, Subscription.DOCUMENT_NAME);

		// augment communication specific beans to always include the
		// communication itself, and the current admin user
		modules.admin.domain.User adminUser = ModulesUtil.currentAdminUser();
		List<Bean> beanList = new ArrayList<>();
		if (specificBeans!=null && specificBeans.length > 0) {
			Collections.addAll(beanList, specificBeans);
		}
		Collections.addAll(beanList, communication, adminUser);
		Bean[] beans = beanList.toArray(new Bean[beanList.size()]);
		
		String sendFromExpression = communication.getSendFrom();
		String sendFrom = null;
		if(sendFromExpression==null){
			sendFrom = DEFAULT_SENDER;
		} else {
			//resolve biding expression
			sendFrom =  Binder.formatMessage(customer, sendFromExpression, specificBeans);
		}

		FormatType format = communication.getFormatType();

		// handle addressee with optional override
		String sendTo = formatCommunicationMessage(customer, communication.getSendTo(), beans);
		if (communication.getSendToOverride() != null) {
			sendTo = formatCommunicationMessage(customer, communication.getSendToOverride(), beans);
		} else {
			// handle Subscriptions
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

						// block the communication if explicit mode
						if (ResponseMode.EXPLICIT.equals(responseMode)) {
							throw new Exception(msg.toString());
						}
					}
				} else {
					format = subscription.getFormatType();
					sendTo = subscription.getPreferredReceiverIdentifier();
				}
			}
		}
		
		String ccTo = formatCommunicationMessage(customer, communication.getCcTo(), beans);
		if(communication.getCcToOverride()!=null){
			ccTo = formatCommunicationMessage(customer, communication.getCcToOverride(), beans);
		}
		String[] cc = null;
		if(ccTo!=null){
			cc = new String[] {ccTo};
		}
		
		
		//add myself to bcc if monitoring outgoing email
		String[] bcc = null;
		if(Boolean.TRUE.equals(communication.getMonitorBcc())){
			bcc = new String[] {adminUser.getContact().getEmail1()};
		}

		String emailSubject = formatCommunicationMessage(customer, communication.getSubject(), beans);
		String emailBodyMain = communication.getBody();
		emailBodyMain = htmlEnclose(formatCommunicationMessage(customer, emailBodyMain, beans));

		// prioritise additional attachments first, then the usual
		List<MailAttachment> attachmentList = new ArrayList<>();
		if (additionalAttachments!=null && additionalAttachments.length > 0) {
			Collections.addAll(attachmentList, additionalAttachments);
		}
		Collections.addAll(attachmentList, getDefinedAttachments(communication));

		// add calendar items
		StringBuilder emailBody = new StringBuilder(emailBodyMain);
		CommunicationCalendarItem calendarItem = null;
		if (Boolean.TRUE.equals(communication.getIncludeCalendar())) {
			calendarItem = generateCalendarAttachments(customer, communication, beans);
			if (calendarItem != null) {
				emailBody.append("\n").append(calendarItem.getGoogleCalendarLink());
				emailBody.append("\n").append(calendarItem.getYahooCalendarLink());
			}
		}
		// Attachment calendar Items
		if (calendarItem != null) {
			MailAttachment outlook = new MailAttachment("OutlookCalendarEvent.ics", calendarItem.getIcsFileAttachment(), MimeType.tex);
			MailAttachment iCal = new MailAttachment("iCalCalendarEvent.ics", calendarItem.getIcsFileAttachment(), MimeType.tex);
			Collections.addAll(attachmentList, outlook, iCal);
		}

		// compile the array for sending or saving
		MailAttachment[] attachments = attachmentList.toArray(new MailAttachment[attachmentList.size()]);

		switch (actionType) {
		case FILE:
			// Generate file name - Communication_Description_To
			StringBuilder subFolder = new StringBuilder();
			SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMddHHmmss");
			subFolder.append(sdf.format(new Date()));

			String customerName = CORE.getUser().getCustomerName();
			String batchDirPrefix = Util.getContentDirectory() + "batch_" + customerName;
			String fileName = sendTo.replace("@", "_").replace(".", "_");

			String filePath = FileUtil.constructSafeFilePath(batchDirPrefix, fileName, ".eml", true, subFolder.toString());
			result = filePath;

			try (FileOutputStream fos = new FileOutputStream(filePath)) {

				// add attachments

				if (RunMode.ACTION.equals(runMode)) {
					switch (format) {
					case email:
						EXT.writeMail(new String[] { sendTo }, cc, bcc, sendFrom, emailSubject, htmlEnclose(emailBody.toString()), MimeType.html, fos, attachments);
						break;
					default:
						break;
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
			break;
		default:
			if (RunMode.ACTION.equals(runMode)) {
				switch (format) {
				case email:
					EXT.sendMail(new String[] { sendTo }, cc, bcc, sendFrom, emailSubject, emailBody.toString(), MimeType.html, attachments);
					break;
				default:
					break;
				}
			}
			break;
		
			

		}
		
		return result;
	}

	/**
	 * Wrapper specific for sending
	 * 
	 * @param communication
	 * @param runMode
	 * @param responseMode
	 * @param additionalAttachments
	 * @param specificBeans
	 * @throws Exception
	 */
	public static void send(Communication communication, RunMode runMode, ResponseMode responseMode, MailAttachment[] additionalAttachments, Bean... specificBeans) throws Exception {
		actionCommunicationRequest(ActionType.SMTP, communication, runMode, responseMode, additionalAttachments, specificBeans);
	}

	/**
	 * Wrapper specific for sending with growl notification on success
	 * 
	 * @param communication
	 * @param runMode
	 * @param responseMode
	 * @param additionalAttachments
	 * @param specificBeans
	 * @throws Exception
	 */
	public static void send(WebContext webContext, Communication communication, RunMode runMode, ResponseMode responseMode, MailAttachment[] additionalAttachments, Bean... specificBeans) throws Exception {
		actionCommunicationRequest(ActionType.SMTP, communication, runMode, responseMode, additionalAttachments, specificBeans);
		if(RunMode.ACTION.equals(runMode) && ResponseMode.EXPLICIT.equals(responseMode) && webContext!=null){
			webContext.growl(MessageSeverity.info, SENT_SUCCESSFULLY_MESSAGE);
		}
	}

	/**
	 * Wrapper specific for generating to file
	 * 
	 * @param communication
	 * @param runMode
	 * @param responseMode
	 * @param additionalAttachments
	 * @param specificBeans
	 * @throws Exception
	 */
	public static String generate(Communication communication, RunMode runMode, ResponseMode responseMode, MailAttachment[] additionalAttachments, Bean... specificBeans) throws Exception {
		return actionCommunicationRequest(ActionType.FILE, communication, runMode, responseMode, additionalAttachments, specificBeans);
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
	public static void sendSimpleBeanCommunication(String sendTo, String subject, String body, ResponseMode responseMode, FormatType formatType, Bean... beans) throws Exception {
		Communication c = Communication.newInstance();

		c.setDescription("Simple Bean Communication");
		c.setSendTo(sendTo);
		c.setSubject(subject);
		c.setBody(body);
		c.setFormatType(formatType);
		c.setSystem(Boolean.TRUE);

		send(c, RunMode.ACTION, responseMode, null, beans);
	}

	/**
	 * Creates the required system communication if it does not exist
	 * 
	 * @return
	 */
	public static Communication initialiseSystemCommunication(String description, String sendToExpression, String ccExpression, String defaultSubject, String defaultBody) throws Exception {

		// create a default communication
		Communication result = getSystemCommunicationByDescription(description);
		if (result == null) {
			// create a basic default system email
			result = Communication.newInstance();
			result.setDescription(description);
			result.setFormatType(FormatType.email);
			result.setSystem(Boolean.TRUE);
			result.setSendFrom(DEFAULT_SENDER);
			result.setSendTo(sendToExpression);
			result.setCcTo(ccExpression);
			result.setSubject(defaultSubject);
			result.setBody(defaultBody);

			Persistence pers = CORE.getPersistence();
			result = pers.save(result);
		}

		return result;
	}
	
	public static Communication initialiseSystemCommunication(String description, String defaultSubject, String defaultBody) throws Exception {
		
		return initialiseSystemCommunication(description, "{contact.email1}",null,  defaultSubject, defaultBody);
	}


	/**
	 * Initialise system communication if it doesn't exist and then send it.
	 * 
	 * @param description
	 * @param defaultSubject
	 * @param defaultBody
	 * @param responseMode
	 * @param bean
	 * @throws Exception
	 */
	public static void sendFailSafeSystemCommunication(String description, String defaultSubject, String defaultBody, ResponseMode responseMode, MailAttachment[] additionalAttachments, Bean... beans) throws Exception {
		
		String sendTo = "{contact.email1}"; // user contact email address
		String ccTo = null;
		
		sendFailSafeSystemCommunication(description, sendTo, ccTo, defaultSubject, defaultBody, responseMode, additionalAttachments, beans);
	}

	
	/**
	 * Initialise system communication if it doesn't exist and then send it.
	 * 
	 * @param description
	 * @param sendTo
	 * @param ccTo
	 * @param defaultSubject
	 * @param defaultBody
	 * @param responseMode
	 * @param bean
	 * @throws Exception
	 */
	public static void sendFailSafeSystemCommunication(String description, String sendTo, String ccTo, String defaultSubject, String defaultBody, ResponseMode responseMode, MailAttachment[] additionalAttachments, Bean... beans) throws Exception {
		Communication c = initialiseSystemCommunication(description, sendTo, ccTo, defaultSubject, defaultBody);
		actionCommunicationRequest(ActionType.SMTP, c, RunMode.ACTION, responseMode, additionalAttachments, beans);
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
		JobMetaData job = module.getJob("jProcessCommunicationsForTag");

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
	private static CommunicationCalendarItem generateCalendarAttachments(Customer customer, Communication communication, Bean... beans) throws Exception {
		CommunicationCalendarItem result = new CommunicationCalendarItem();

		String google = null;
		String yahoo = null;

		// set Title
		StringBuilder sb = new StringBuilder(512);
		// sb.append(row[1]).append(" - ").append(row[2]);
		String eventTitle = formatCommunicationMessage(customer, communication.getCalendarTitleExpression(), beans);
		String eventDescription = formatCommunicationMessage(customer, communication.getCalendarDescriptionExpression(), beans);

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
	private static MailAttachment[] getDefinedAttachments(Communication communication) throws Exception {

		MailAttachment ma1 = null;
		MailAttachment ma2 = null;
		MailAttachment ma3 = null;

		try (ContentManager cm = EXT.newContentManager()) {

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
		}

		// construct array
		MailAttachment[] attachments = { ma1, ma2, ma3 };

		return attachments;
	}

	/**
	 * formatCommunicationMessage
	 * 
	 * Special case handling of Binder.formatMessage for communications
	 * 
	 * @param customer
	 * @param expression
	 * @param beans
	 * @return
	 * @throws Exception
	 */
	public static String formatCommunicationMessage(Customer customer, String expression, Bean... beans) throws Exception {
		String result = expression;

		// default url binding to first bean
		if (beans != null && beans.length > 0 && expression!=null) {
			result = expression.replace(SPECIAL_BEAN_URL, Util.getDocumentUrl(beans[0]));
			result = result.replace(SPECIAL_CONTEXT, Util.getHomeUrl());
			result = Binder.formatMessage(customer, result, beans);
		}
		return result;
	}

	/**
	 * Returns a String with wrapped with <html><body> tags
	 * 
	 * @param html
	 * @return
	 */
	private static String htmlEnclose(String html) {
		StringBuilder sb = new StringBuilder();
		sb.append("<html><body>").append(html).append("</body></html>");
		return sb.toString();
	}
}
