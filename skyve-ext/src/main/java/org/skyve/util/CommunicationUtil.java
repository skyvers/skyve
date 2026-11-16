package org.skyve.util;

import java.io.FileOutputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.content.AttachmentContent;
import org.skyve.content.ContentManager;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.domain.app.AppConstants;
import org.skyve.domain.app.admin.Communication;
import org.skyve.domain.app.admin.Communication.FormatType;
import org.skyve.domain.app.admin.CommunicationTemplate;
import org.skyve.domain.app.admin.Contact;
import org.skyve.domain.app.admin.Subscription;
import org.skyve.domain.app.admin.Tag;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.domain.messages.NoResultsException;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.types.DateTime;
import org.skyve.impl.metadata.model.document.field.validator.TextValidator;
import org.skyve.impl.metadata.model.document.field.validator.TextValidator.ValidatorType;
import org.skyve.impl.util.TimeUtil;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.web.WebContext;
import org.slf4j.Logger;
import org.skyve.util.logging.SkyveLoggerFactory;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Executes communication-template workflows for {@code admin.Communication} documents.
 *
 * <p>This utility resolves recipients, applies template/body bindings, assembles
 * attachments (including optional calendar payloads), and dispatches via SMTP or
 * writes message files depending on the configured action path.
 */
public class CommunicationUtil {

    private static final Logger LOGGER = SkyveLoggerFactory.getLogger(CommunicationUtil.class);

	private static final String EMAIL_ADDRESS_DELIMETERS = "[,;]";
	private static final String INVALID_RESOLVED_EMAIL_ADDRESS = "The sendTo address could not be resolved to a valid email address";
	private static final String BODY_ATTRIBUTE_NAME = "body";
	private static final String SEND_TO_ATTRIBUTE_NAME = "sendTo";
	private static final String FORMAT_TYPE_ATTRIBUTE_NAME = "formatType";
	private static final String CALENDAR_START_TIME_ATTRIBUTE_NAME = "calendarStartTime";
	private static final String CALENDAR_END_TIME_ATTRIBUTE_NAME = "calendarEndTime";
	public static final String SPECIAL_BEAN_URL = "{#url}";
	public static final String SPECIAL_CONTEXT = "{#context}";
	public static final String SPECIAL_LOGOUT_URL = "{#resetPasswordUrl}";
	public static final String SENT_SUCCESSFULLY_MESSAGE = "Communication sent";

	/**
	 * Whether to throw or log an exception if one occurs.
	 * 
	 * SILENT logs any exception stack EXPLICIT throws any exception
	 */
	public enum ResponseMode {
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
	public enum RunMode {
		ACTION, TEST;
	}

	public enum ActionType {
		FILE, SMTP;
	}

	/**
	 * Holds generated calendar links and iCalendar payload bytes for a communication.
	 */
	public static class CommunicationCalendarItem {
		private String googleCalendarLink;
		private String yahooCalendarLink;
		private byte[] icsFileAttachment;
		
		public @Nullable String getGoogleCalendarLink() {
			return googleCalendarLink;
		}
		public void setGoogleCalendarLink(@Nullable String googleCalendarLink) {
			this.googleCalendarLink = googleCalendarLink;
		}
		public @Nullable String getYahooCalendarLink() {
			return yahooCalendarLink;
		}
		public void setYahooCalendarLink(@Nullable String yahooCalendarLink) {
			this.yahooCalendarLink = yahooCalendarLink;
		}
		public @Nullable byte[] getIcsFileAttachment() {
			return icsFileAttachment;
		}
		public void setIcsFileAttachment(@Nullable byte[] icsFileAttachment) {
			this.icsFileAttachment = icsFileAttachment;
		}
		public CommunicationCalendarItem(@Nullable String googleCalendarLink, @Nullable String yahooCalendarLink, @Nullable byte[] icsFileAttachment) {
			this.googleCalendarLink = googleCalendarLink;
			this.yahooCalendarLink = yahooCalendarLink;
			this.icsFileAttachment = icsFileAttachment;
		}
		public CommunicationCalendarItem() {
		}
	}

	/**
	 * actionCommunicationRequest
	 * 
	 * @param communication
	 * @param runMode
	 * @param responseMode
	 * @param beans
	 * 
	 *            returns the filePath of the written email file (if this option is chosen and write is successful)
	 * 
	 * @throws Exception
	 */
	@SuppressWarnings("java:S3776") // Complexity OK
	private static @Nullable String actionCommunicationRequest(@Nullable WebContext webContext, @Nonnull ActionType actionType, @Nonnull Communication communication, @Nonnull RunMode runMode, @Nonnull ResponseMode responseMode,
			@Nullable MailAttachment[] additionalAttachments, @Nonnull Bean... specificBeans) throws Exception {

		String resultingFilePath = null;

		Persistence pers = CORE.getPersistence();
		User user = pers.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(AppConstants.ADMIN_MODULE_NAME);
		Document communicationDocument = module.getDocument(customer, AppConstants.COMMUNICATION_DOCUMENT_NAME);
		Document subscriptionDocument = module.getDocument(customer, AppConstants.SUBSCRIPTION_DOCUMENT_NAME);

		// augment communication specific beans to always include the
		// communication itself, and the current admin user
		org.skyve.domain.app.admin.User adminUser = pers.retrieve(AppConstants.ADMIN_MODULE_NAME, AppConstants.USER_DOCUMENT_NAME, user.getId());
		if (adminUser == null) {
			throw new NoResultsException();
		}
		List<Bean> beanList = new ArrayList<>();
		Collections.addAll(beanList, specificBeans);
		Collections.addAll(beanList, communication, adminUser);
		Bean[] beans = beanList.toArray(new Bean[beanList.size()]);

		String sendFromExpression = communication.getSendFrom();
		String sendFrom = null;
		if (sendFromExpression == null) {
			sendFrom = UtilImpl.SMTP_SENDER;
		} else {
			// resolve binding expression
			sendFrom = Binder.formatMessage(sendFromExpression, specificBeans);
		}

		// handle addressee with optional override
		String sendTo = null;
		if (communication.getSendToOverride() == null) {
			sendTo = formatCommunicationMessage(customer, communication.getSendTo(), beans);
		} else {
			sendTo = formatCommunicationMessage(customer, communication.getSendToOverride(), beans);
		}
		List<String> sendToAddresses = resolveAndValidateEmailAddressList(sendTo, responseMode, communicationDocument, subscriptionDocument);

		// Resolve email message contents
		String ccTo = formatCommunicationMessage(customer, communication.getCcTo(), beans);
		if (communication.getCcToOverride() != null) {
			ccTo = formatCommunicationMessage(customer, communication.getCcToOverride(), beans);
		}
		List<String> ccToAddresses = resolveAndValidateEmailAddressList(ccTo, responseMode, communicationDocument, subscriptionDocument);

		// add the current admin user to bcc if monitoring outgoing email
		String[] bcc = null;
		if (Boolean.TRUE.equals(communication.getMonitorBcc())) {
			Contact adminContact = adminUser.getContact();
			if (adminContact == null) {
				throw new IllegalStateException("Admin user " + user.getId() + " has no contact");
			}
			bcc = new String[] { adminContact.getEmail1() };
		}

		String emailSubject = formatCommunicationMessage(customer, communication.getSubject(), beans);

		// process email body
		String emailBodyMain = communication.getBody();
		
		emailBodyMain = formatCommunicationMessage(customer, emailBodyMain, beans);
		if (emailBodyMain == null) {
			throw new ValidationException(new Message(BODY_ATTRIBUTE_NAME, "A body is required for the communication."));
		}

		CommunicationTemplate template = communication.getTemplate();
		if (template != null) {
			// attempt to use the template if there is one
			String templateMarkup = template.getTemplate();
			if (templateMarkup == null) { // required
				throw new IllegalStateException("The communication template has no template");
			}
			if (templateMarkup.indexOf("{body}") >= 0) {
				emailBodyMain = templateMarkup.replace("{body}", emailBodyMain);
			}
		} else {
			// if not using a template, enclose the html
			emailBodyMain = htmlEnclose(emailBodyMain);
		}

		// prioritise additional attachments first, then the usual
		List<MailAttachment> attachmentList = new ArrayList<>();
		if (additionalAttachments != null && additionalAttachments.length > 0) {
			Collections.addAll(attachmentList, additionalAttachments);
		}
		Collections.addAll(attachmentList, getDefinedAttachments(communication));

		// add calendar items
		StringBuilder emailBody = new StringBuilder(emailBodyMain);
		CommunicationCalendarItem calendarItem = null;
		if (Boolean.TRUE.equals(communication.getIncludeCalendar())) {
			calendarItem = generateCalendarAttachments(customer, communication, beans);
			emailBody.append("\n").append(calendarItem.getGoogleCalendarLink());
			emailBody.append("\n").append(calendarItem.getYahooCalendarLink());
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
			if (sendTo == null) {
				throw new ValidationException(new Message(SEND_TO_ATTRIBUTE_NAME, "A recipient is required to name the communication file."));
			}
			String fileName = sendTo.replace("@", "_").replace(".", "_");
			String filePath = FileUtil.constructSafeFilePath(communication.getBasePath(), fileName, ".eml", true, communication.getBatch());

			try (FileOutputStream fos = new FileOutputStream(filePath)) {

				// add attachments

				if (RunMode.ACTION.equals(runMode)) {
					switch (formatType(communication)) {
					case email:
						EXT.getMailService()
								.writeMail(new Mail().addTo(sendToAddresses)
													.addCC(ccToAddresses)
													.addBCC(bcc)
													.from(sendFrom)
													.subject(emailSubject)
													.body(htmlEnclose(emailBody.toString()))
													.attach(attachments),
										fos);
						resultingFilePath = filePath;
						break;
					default:
						break;
					}
				}

				fos.flush();
			} catch (Exception e) {
				if (ResponseMode.SILENT.equals(responseMode)) {
					LOGGER.warn(e.toString(), e);
				} else {
					throw e;
				}
			}
			break;
		default:
			if (RunMode.ACTION.equals(runMode)) {
				switch (formatType(communication)) {
				case email:
					EXT.getMailService()
							.sendMail(new Mail().addTo(sendToAddresses)
											.addCC(ccToAddresses)
											.addBCC(bcc)
											.from(sendFrom)
											.subject(emailSubject)
											.body(emailBody.toString())
											.attach(attachments));

					if (webContext != null) {
						webContext.growl(MessageSeverity.info, SENT_SUCCESSFULLY_MESSAGE);
					}
					break;
				default:
					break;
				}
			}
			break;
		}

		return resultingFilePath;
	}

	/**
	 * Create a list of address Strings which have been validated and resolved via the subscription facility
	 * 
	 * @param addressList
	 * @return
	 */
	@SuppressWarnings("java:S3776") // Complexity OK
	private static @Nonnull List<String> resolveAndValidateEmailAddressList(@Nullable String addressList, @Nonnull ResponseMode responseMode, @Nonnull Document communicationDoc,
			@Nonnull Document subscriptionDoc) throws Exception {

		List<String> resolvedAndValidatedAddresses = new ArrayList<>();

		if (addressList != null) {
			String[] addresses = addressList.split(EMAIL_ADDRESS_DELIMETERS);

			for (int i = 0; i < addresses.length; i++) {
				// handle Subscription redirect for messages of the same format
				final String trimmedAddress = addresses[i].trim();
				String address = CORE.getPersistence().withDocumentPermissionScopes(DocumentPermissionScope.customer, p -> {
					DocumentQuery q = p.newDocumentQuery(AppConstants.ADMIN_MODULE_NAME, AppConstants.SUBSCRIPTION_DOCUMENT_NAME);
					q.getFilter().addEquals(AppConstants.RECEIVER_IDENTIFIER_ATTRIBUTE_NAME, trimmedAddress);
					Subscription subscription = q.beanResult();

					if (subscription != null) {
						// check for declined
						if (Boolean.TRUE.equals(subscription.getDeclined())) {
							StringBuilder msg = new StringBuilder(128);
							msg.append(communicationDoc.getLocalisedSingularAlias()).append(" prevented because the recipient ");
							msg.append(trimmedAddress).append(" has a ").append(subscriptionDoc.getLocalisedSingularAlias());
							msg.append(" set ").append(AppConstants.DECLINED_ATTRIBUTE_NAME);

							// block the communication if explicit mode
							if (ResponseMode.EXPLICIT.equals(responseMode)) {
								throw new DomainException(msg.toString());
							}
						}
						else {
							return subscription.getPreferredReceiverIdentifier();
						}
					}
					return trimmedAddress;
				});

				// validate the resulting email address
				ValidationException ve = new ValidationException();
				TextValidator v = new TextValidator();
				v.setType(ValidatorType.email);
				v.validate(CORE.getUser(), address, "email1", "Email", null, ve);
				if (!ve.getMessages().isEmpty()) {
					if (ResponseMode.SILENT.equals(responseMode)) {
						LOGGER.trace("The resolved email address {} could not be validated.", address);
					} else {
						throw ve;
					}
				}

				// Validate resolved email address
				if (address == null || address.trim().length() < 1) {
					throw new IllegalArgumentException(INVALID_RESOLVED_EMAIL_ADDRESS);
				}

				resolvedAndValidatedAddresses.add(address);
			}
		}

		return resolvedAndValidatedAddresses;

	}

	private static @Nullable String actionCommunicationRequest(@Nonnull ActionType actionType, @Nonnull Communication communication, @Nonnull RunMode runMode, @Nonnull ResponseMode responseMode,
			@Nullable MailAttachment[] additionalAttachments, @Nonnull Bean... specificBeans) throws Exception {
		return actionCommunicationRequest(null, actionType, communication, runMode, responseMode, additionalAttachments, specificBeans);
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
	public static void send(@Nonnull Communication communication, @Nonnull RunMode runMode, @Nonnull ResponseMode responseMode, @Nullable MailAttachment[] additionalAttachments, @Nonnull Bean... specificBeans)
			throws Exception {
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
	public static void send(@Nullable WebContext webContext, @Nonnull Communication communication, @Nonnull RunMode runMode, @Nonnull ResponseMode responseMode, @Nullable MailAttachment[] additionalAttachments,
			@Nonnull Bean... specificBeans) throws Exception {
		actionCommunicationRequest(ActionType.SMTP, communication, runMode, responseMode, additionalAttachments, specificBeans);
		if (RunMode.ACTION.equals(runMode) && ResponseMode.EXPLICIT.equals(responseMode) && webContext != null) {
			webContext.growl(MessageSeverity.info, SENT_SUCCESSFULLY_MESSAGE);
		}
	}

	/**
	 * Generates communication output to file storage rather than sending via SMTP.
	 * 
	 * @param communication Communication definition to execute
	 * @param runMode Whether to execute or dry-run
	 * @param responseMode Exception handling mode
	 * @param additionalAttachments Attachments to append ahead of configured attachments
	 * @param specificBeans Binding context beans
	 * @return Generated output file path when generation occurs, otherwise {@code null}
	 * @throws Exception If generation fails and explicit mode is used
	 */
	public static @Nullable String generate(@Nonnull Communication communication, @Nonnull RunMode runMode, @Nonnull ResponseMode responseMode, @Nullable MailAttachment[] additionalAttachments, @Nonnull Bean... specificBeans)
			throws Exception {
		return actionCommunicationRequest(ActionType.FILE, communication, runMode, responseMode, additionalAttachments, specificBeans);
	}

	/**
	 * Retrieves a persisted system communication with the nominated description
	 * 
	 * @param description Communication description to search for
	 * @return Matching system communication, or {@code null} when not found
	 */
	public static @Nullable Communication getSystemCommunicationByDescription(@Nonnull String description) {
		return CORE.getPersistence().withDocumentPermissionScopes(DocumentPermissionScope.customer, p -> {
			DocumentQuery query = p.newDocumentQuery(AppConstants.ADMIN_MODULE_NAME, AppConstants.COMMUNICATION_DOCUMENT_NAME);
			query.getFilter().addEquals(AppConstants.DESCRIPTION_ATTRIBUTE_NAME, description);
			return query.beanResult();
		});
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
	public static void sendSimpleBeanCommunication(@Nonnull String sendTo, @Nonnull String subject, @Nonnull String body, @Nonnull ResponseMode responseMode, @Nonnull FormatType formatType, @Nonnull Bean... beans) throws Exception {
		User u = CORE.getUser();
		Customer c = u.getCustomer();
		Module m = c.getModule(AppConstants.ADMIN_MODULE_NAME);
		Document d = m.getDocument(c, AppConstants.COMMUNICATION_DOCUMENT_NAME);
		Communication bean = d.newInstance(u);

		bean.setDescription("Simple Bean Communication");
		bean.setSendTo(sendTo);
		bean.setSubject(subject);
		bean.setBody(body);
		bean.setFormatType(formatType);
		bean.setSystemUse(Boolean.TRUE);

		send(bean, RunMode.ACTION, responseMode, null, beans);
	}

	/**
	 * Creates the required system communication if it does not exist
	 * 
	 * @param description Communication description key
	 * @param sendToExpression Recipient expression
	 * @param ccExpression CC expression
	 * @param defaultSubject Default subject template
	 * @param defaultBody Default body template
	 * @return Existing or newly created persisted communication
	 * @throws Exception If initialisation fails
	 */
	public static @Nonnull Communication initialiseSystemCommunication(@Nonnull String description, @Nonnull String sendToExpression, @Nullable String ccExpression, @Nonnull String defaultSubject, @Nonnull String defaultBody)
			throws Exception {

		// create a default communication
		Communication result = getSystemCommunicationByDescription(description);
		if (result == null) {
			// create a basic default system email
			User u = CORE.getUser();
			Customer c = u.getCustomer();
			Module m = c.getModule(AppConstants.ADMIN_MODULE_NAME);
			Document d = m.getDocument(c, AppConstants.COMMUNICATION_DOCUMENT_NAME);
			result = d.newInstance(u);

			result.setDescription(description);
			result.setFormatType(FormatType.email);
			result.setSystemUse(Boolean.TRUE);
			result.setSendFrom(UtilImpl.SMTP_SENDER);
			result.setSendTo(sendToExpression);
			result.setCcTo(ccExpression);
			result.setSubject(defaultSubject);
			result.setBody(defaultBody);

			Persistence pers = CORE.getPersistence();
			result = pers.save(result);
		}

		return result;
	}

	/**
	 * Creates a system communication with default recipient expression when absent.
	 *
	 * @param description Communication description key
	 * @param defaultSubject Default subject template
	 * @param defaultBody Default body template
	 * @return Existing or newly created persisted communication
	 * @throws Exception If initialisation fails
	 */
	public static @Nonnull Communication initialiseSystemCommunication(@Nonnull String description, @Nonnull String defaultSubject, @Nonnull String defaultBody) throws Exception {

		return initialiseSystemCommunication(description, "{contact.email1}", null, defaultSubject, defaultBody);
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
	public static void sendFailSafeSystemCommunication(@Nullable WebContext webContext, @Nonnull String description, @Nonnull String defaultSubject, @Nonnull String defaultBody, @Nonnull ResponseMode responseMode,
			@Nullable MailAttachment[] additionalAttachments, @Nonnull Bean... beans) throws Exception {

		String sendTo = "{contact.email1}"; // user contact email address
		String ccTo = null;

		sendFailSafeSystemCommunication(webContext, description, sendTo, ccTo, defaultSubject, defaultBody, responseMode, additionalAttachments, beans);
	}

	/**
	 * Fail-safe sends a system communication using default recipient expressions.
	 *
	 * @param description Communication description key
	 * @param defaultSubject Default subject template
	 * @param defaultBody Default body template
	 * @param responseMode Exception handling mode
	 * @param additionalAttachments Attachments to append
	 * @param beans Binding context beans
	 * @throws Exception If initialisation or send fails in explicit mode
	 */
	public static void sendFailSafeSystemCommunication(@Nonnull String description, @Nonnull String defaultSubject, @Nonnull String defaultBody, @Nonnull ResponseMode responseMode,
			@Nullable MailAttachment[] additionalAttachments, @Nonnull Bean... beans) throws Exception {
		sendFailSafeSystemCommunication(null, description, defaultSubject, defaultBody, responseMode, additionalAttachments, beans);
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
	@SuppressWarnings("java:S107") // Long parameter list preserves the existing framework/API contract.
	public static void sendFailSafeSystemCommunication(@Nullable WebContext webContext, @Nonnull String description, @Nonnull String sendTo, @Nullable String ccTo, @Nonnull String defaultSubject, @Nonnull String defaultBody,
			@Nonnull ResponseMode responseMode, @Nullable MailAttachment[] additionalAttachments, @Nonnull Bean... beans) throws Exception {
		Communication c = initialiseSystemCommunication(description, sendTo, ccTo, defaultSubject, defaultBody);
		actionCommunicationRequest(webContext, ActionType.SMTP, c, RunMode.ACTION, responseMode, additionalAttachments, beans);
	}

	/**
	 * Fail-safe sends a system communication using explicit recipient expressions.
	 *
	 * @param description Communication description key
	 * @param sendTo Recipient expression
	 * @param ccTo CC expression
	 * @param defaultSubject Default subject template
	 * @param defaultBody Default body template
	 * @param responseMode Exception handling mode
	 * @param additionalAttachments Attachments to append
	 * @param beans Binding context beans
	 * @throws Exception If initialisation or send fails in explicit mode
	 */
	@SuppressWarnings("java:S107") // Long parameter list preserves the existing framework/API contract.
	public static void sendFailSafeSystemCommunication(@Nonnull String description, @Nonnull String sendTo, @Nullable String ccTo, @Nonnull String defaultSubject, @Nonnull String defaultBody, @Nonnull ResponseMode responseMode,
			@Nullable MailAttachment[] additionalAttachments, @Nonnull Bean... beans) throws Exception {
		sendFailSafeSystemCommunication(null, description, sendTo, ccTo, defaultSubject, defaultBody, responseMode, additionalAttachments, beans);
	}

	/**
	 * Kicks off a one-shot-job which is implied by the attributes set in the
	 * communication.
	 * 
	 * Using the tag, format and action type, run the one shot job to either
	 * test, send or generate the communication files.
	 * 
	 * @param bean Communication instance driving the one-shot job request
	 * @return Persisted communication instance with updated results text
	 * @throws Exception If job kickoff cannot be scheduled
	 */
	public static @Nonnull <T extends Communication> T kickOffJob(@Nonnull T bean) throws Exception {

		T communication = bean;

		String results = getResults(communication);

		// save this communication if it has not been saved yet
		if (communication.isNotPersisted()) {
			communication = CORE.getPersistence().save(communication);
		}

		Persistence persistence = CORE.getPersistence();
		User user = persistence.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(AppConstants.ADMIN_MODULE_NAME);
		JobMetaData job = module.getJob(AppConstants.PROCESS_COMMUNICATIONS_FOR_TAG_JOB_NAME);

		EXT.getJobScheduler().runOneShotJob(job, communication, user);

		StringBuilder sb = new StringBuilder(results);
		sb.append("\nThe job has been commenced - check Admin->Jobs for the log.");
		communication.setResults(sb.toString());

		communication.originalValues().remove(AppConstants.RESULTS_ATTRIBUTE_NAME);

		return communication;
	}

	public static @Nonnull String getResults(@Nonnull Communication bean) throws Exception {

		// validate required fields
		if (bean.getModuleName() == null) {
			throw new ValidationException(
					new Message(AppConstants.MODULE_NAME_ATTRIBUTE_NAME, "A module must be selected for results."));
		}
		if (bean.getDocumentName() == null) {
			throw new ValidationException(
					new Message(AppConstants.DOCUMENT_NAME_ATTRIBUTE_NAME, "A document must be selected for results."));
		}
		Tag tag = bean.getTag();
		if (tag == null) {
			throw new ValidationException(new Message(AppConstants.TAG_ATTRIBUTE_NAME, "A tag must be selected for results."));
		}

		long count = tag.countDocument(bean.getModuleName(), bean.getDocumentName());

		StringBuilder results = new StringBuilder();
		results.append(count).append(" communications for ");
		results.append(bean.getDocumentName());

		Communication.ActionType actionType = bean.getActionType();
		if (actionType != null) {
			results.append(" will be ");
			switch (actionType) {
			case saveForBulkSend:
				results.append("created as a batch for download.");
				break;

			case sendImmediately:
				results.append("sent immediately.");
				break;

			case testBindingsAndOutput:
				results.append("tested.");
				break;

			default:
				break;
			}
		}

		return results.toString();
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
	private static @Nonnull CommunicationCalendarItem generateCalendarAttachments(@Nonnull Customer customer, @Nonnull Communication communication, @Nonnull Bean... beans) throws Exception {
		CommunicationCalendarItem result = new CommunicationCalendarItem();

		String google = null;
		String yahoo = null;

		// set Title
		StringBuilder sb = new StringBuilder(512);
		// sb.append(row[1]).append(" - ").append(row[2]);
		String eventTitle = formatCommunicationMessage(customer, communication.getCalendarTitleExpression(), beans);
		String eventDescription = formatCommunicationMessage(customer, communication.getCalendarDescriptionExpression(), beans);

		DateTime startTime = communication.getCalendarStartTime();
		if (startTime == null) {
			throw new ValidationException(new Message(CALENDAR_START_TIME_ATTRIBUTE_NAME, "A calendar start time is required to include a calendar."));
		}
		DateTime endTime = communication.getCalendarEndTime();
		if (endTime == null) {
			throw new ValidationException(new Message(CALENDAR_END_TIME_ATTRIBUTE_NAME, "A calendar end time is required to include a calendar."));
		}
		String start = TimeUtil.formatISODate(startTime, true).replace(".000+00:00", "Z").replace("-", "").replace(":", "");
		String end = TimeUtil.formatISODate(endTime, true).replace(".000+00:00", "Z").replace("-", "").replace(":", "");

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
	 * Compiles the attachments defined in the attachment slots of a communication.
	 * A slot is attached only when it has both a file name and content.
	 *
	 * @param communication	The communication.
	 * @return	The attachments of the filled slots in slot order, which is empty when no slot is filled.
	 * @throws DomainException	if the content of a filled slot cannot be found.
	 */
	private static @Nonnull MailAttachment[] getDefinedAttachments(@Nonnull Communication communication) throws Exception {
		List<MailAttachment> result = new ArrayList<>(3);

		try (ContentManager cm = EXT.newContentManager()) {

			// Attachment 1
			String attachmentFileName1 = communication.getAttachmentFileName1();
			String attachment1 = communication.getAttachment1();
			if (attachmentFileName1 != null && attachment1 != null) {
				result.add(newDefinedAttachment(cm, attachmentFileName1, attachment1));
			}

			// Attachment 2
			String attachmentFileName2 = communication.getAttachmentFileName2();
			String attachment2 = communication.getAttachment2();
			if (attachmentFileName2 != null && attachment2 != null) {
				result.add(newDefinedAttachment(cm, attachmentFileName2, attachment2));
			}

			// Attachment 3
			String attachmentFileName3 = communication.getAttachmentFileName3();
			String attachment3 = communication.getAttachment3();
			if (attachmentFileName3 != null && attachment3 != null) {
				result.add(newDefinedAttachment(cm, attachmentFileName3, attachment3));
			}
		}

		return result.toArray(new MailAttachment[result.size()]);
	}

	/**
	 * Returns the mail attachment for a filled attachment slot of a communication.
	 *
	 * @param cm	The content manager to load the attachment content with.
	 * @param attachmentFileName	The file name to attach the content as.
	 * @param contentId	The content ID of the attachment content.
	 * @return	The mail attachment.
	 * @throws DomainException	if the attachment content cannot be found.
	 */
	private static @Nonnull MailAttachment newDefinedAttachment(@Nonnull ContentManager cm,
																	@Nonnull String attachmentFileName,
																	@Nonnull String contentId)
	throws Exception {
		AttachmentContent content = cm.getAttachment(contentId);
		if (content == null) {
			throw new DomainException("The content for the attachment can't be retrieved - re-attach the content and try again.");
		}
		byte[] fileBytes = content.getContentBytes();
		// A content type that is not a known mime type is attached without a content type
		MimeType mimeType = content.getMimeType();
		return new MailAttachment(attachmentFileName, fileBytes, (mimeType == null) ? null : mimeType.toString());
	}

	/**
	 * Returns the format type of the communication.
	 *
	 * @param communication	The communication.
	 * @return	The format type.
	 * @throws ValidationException	if the communication has no format type.
	 */
	private static @Nonnull FormatType formatType(@Nonnull Communication communication) {
		FormatType result = communication.getFormatType();
		if (result == null) {
			throw new ValidationException(new Message(FORMAT_TYPE_ATTRIBUTE_NAME, "A format type is required for the communication."));
		}
		return result;
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
	public static @Nullable String formatCommunicationMessage(@Nullable Customer customer, @Nullable String expression, @Nullable Bean... beans) {
		String result = expression;
		
		if (result != null) {
			result = result.replace(SPECIAL_LOGOUT_URL, Util.getResetPasswordUrl());
		}
		
		
		// default url binding to first bean
		if (beans != null && beans.length > 0 && result != null) {
			Bean bean = beans[0];
			if (bean != null) {
				result = result.replace(SPECIAL_BEAN_URL, Util.getDocumentUrl(beans[0]));
				result = result.replace(SPECIAL_CONTEXT, Util.getBaseUrl());
			}
			result = Binder.formatMessage(result, beans);
		}
		return result;
	}

	/**
	 * Returns a String with wrapped with <html><body> tags
	 * and replaces \n within a line with <br>
	 * 
	 * @param html
	 * @return
	 */
	private static @Nonnull String htmlEnclose(@Nonnull String html) {
		StringBuilder sb = new StringBuilder();

		// replace all \n with <br> when it is not at the end of a line
		String[] lines = html.split("\n");
		for (String line : lines) {
			sb.append(line);
			if ((! line.isEmpty()) && (! line.endsWith(">"))) {
				sb.append("<br>\n");
			} else {
				sb.append("\n");
			}
		}

		// enclose the html if required
		if (html.indexOf("<html") < 0) {
			sb.insert(0, "<html><body>");
		}
		if (html.indexOf("</html>") < 0) {
			sb.append("</body></html>");
		}

		return sb.toString();
	}
}
