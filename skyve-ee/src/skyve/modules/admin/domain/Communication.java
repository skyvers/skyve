package modules.admin.domain;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import org.skyve.CORE;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Enumeration;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.impl.domain.types.jaxb.DateTimeMapper;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

/**
 * Communication
 * 
 * @depend - - - ActionType
 * @depend - - - FormatType
 * @navhas n tag 0..1 Tag
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
public class Communication extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";
	/** @hidden */
	public static final String DOCUMENT_NAME = "Communication";

	/** @hidden */
	public static final String descriptionPropertyName = "description";
	/** @hidden */
	public static final String tagPropertyName = "tag";
	/** @hidden */
	public static final String moduleNamePropertyName = "moduleName";
	/** @hidden */
	public static final String documentNamePropertyName = "documentName";
	/** @hidden */
	public static final String toBindingPropertyName = "toBinding";
	/** @hidden */
	public static final String sendToPropertyName = "sendTo";
	/** @hidden */
	public static final String ccToPropertyName = "ccTo";
	/** @hidden */
	public static final String sendToOverridePropertyName = "sendToOverride";
	/** @hidden */
	public static final String ccToOverridePropertyName = "ccToOverride";
	/** @hidden */
	public static final String sendFromPropertyName = "sendFrom";
	/** @hidden */
	public static final String monitorBccPropertyName = "monitorBcc";
	/** @hidden */
	public static final String subjectPropertyName = "subject";
	/** @hidden */
	public static final String bodyPropertyName = "body";
	/** @hidden */
	public static final String resultsPropertyName = "results";
	/** @hidden */
	public static final String attachment1PropertyName = "attachment1";
	/** @hidden */
	public static final String attachmentFileName1PropertyName = "attachmentFileName1";
	/** @hidden */
	public static final String attachment2PropertyName = "attachment2";
	/** @hidden */
	public static final String attachmentFileName2PropertyName = "attachmentFileName2";
	/** @hidden */
	public static final String attachment3PropertyName = "attachment3";
	/** @hidden */
	public static final String attachmentFileName3PropertyName = "attachmentFileName3";
	/** @hidden */
	public static final String actionTypePropertyName = "actionType";
	/** @hidden */
	public static final String formatTypePropertyName = "formatType";
	/** @hidden */
	public static final String selectedBatchTimestampFolderNamePropertyName = "selectedBatchTimestampFolderName";
	/** @hidden */
	public static final String refreshBatchesPropertyName = "refreshBatches";
	/** @hidden */
	public static final String systemPropertyName = "system";
	/** @hidden */
	public static final String unsubscribeUrlPropertyName = "unsubscribeUrl";
	/** @hidden */
	public static final String includeCalendarPropertyName = "includeCalendar";
	/** @hidden */
	public static final String calendarTitleExpressionPropertyName = "calendarTitleExpression";
	/** @hidden */
	public static final String calendarStartTimePropertyName = "calendarStartTime";
	/** @hidden */
	public static final String calendarEndTimePropertyName = "calendarEndTime";
	/** @hidden */
	public static final String calendarDescriptionExpressionPropertyName = "calendarDescriptionExpression";
	/** @hidden */
	public static final String mailImagePropertyName = "mailImage";
	/** @hidden */
	public static final String subscriptionsPropertyName = "subscriptions";

	/**
	 * Action
	 **/
	@XmlEnum
	public static enum ActionType implements Enumeration {
		saveForBulkSend("save", "Save for bulk send"),
		sendImmediately("send", "Send Immediately"),
		testBindingsAndOutput("test", "Test bindings and output");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues;

		private ActionType(String code, String description) {
			this.code = code;
			this.description = description;
			this.domainValue = new DomainValue(code, description);
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toDescription() {
			return description;
		}

		@Override
		public DomainValue toDomainValue() {
			return domainValue;
		}

		public static ActionType fromCode(String code) {
			ActionType result = null;

			for (ActionType value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static ActionType fromDescription(String description) {
			ActionType result = null;

			for (ActionType value : values()) {
				if (value.description.equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			if (domainValues == null) {
				ActionType[] values = values();
				domainValues = new ArrayList<>(values.length);
				for (ActionType value : values) {
					domainValues.add(value.domainValue);
				}
			}

			return domainValues;
		}
	}

	/**
	 * Format
	 **/
	@XmlEnum
	public static enum FormatType implements Enumeration {
		email("email", "email");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues;

		private FormatType(String code, String description) {
			this.code = code;
			this.description = description;
			this.domainValue = new DomainValue(code, description);
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toDescription() {
			return description;
		}

		@Override
		public DomainValue toDomainValue() {
			return domainValue;
		}

		public static FormatType fromCode(String code) {
			FormatType result = null;

			for (FormatType value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static FormatType fromDescription(String description) {
			FormatType result = null;

			for (FormatType value : values()) {
				if (value.description.equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			if (domainValues == null) {
				FormatType[] values = values();
				domainValues = new ArrayList<>(values.length);
				for (FormatType value : values) {
					domainValues.add(value.domainValue);
				}
			}

			return domainValues;
		}
	}

	/**
	 * Description
	 **/
	private String description;
	/**
	 * Tag
	 **/
	private Tag tag = null;
	/**
	 * Module
	 * <br/>
	 * Bindings used in the communication address, subject and body will be based on the selected module document.
	 **/
	private String moduleName;
	/**
	 * Document
	 * <br/>
	 * Bindings used in the communication address, subject and body will be based on the selected module document.
	 **/
	private String documentName;
	/**
	 * Send to
	 * <br/>
	 * Provide a binding which contains the email address to send to
	 **/
	private String toBinding;
	/**
	 * To
	 * <br/>
	 * The address to send to.
	 **/
	private String sendTo;
	/**
	 * CC To
	 * <br/>
	 * The address to send to.
	 **/
	private String ccTo;
	/**
	 * To (expression)
	 * <br/>
	 * The address to send to. Bindings are allowed relative to the above module document.
	 **/
	private String sendToOverride;
	/**
	 * CC To (expression)
	 * <br/>
	 * The address to send to. Bindings are allowed relative to the above module document.
	 **/
	private String ccToOverride;
	/**
	 * From (expression)
	 * <br/>
	 * The address to send from. Bindings are allowed relative to the above module document.
	 **/
	private String sendFrom;
	/**
	 * BCC Me
	 * <br/>
	 * Monitor outgoing emails by including me in the Bcc
	 **/
	private Boolean monitorBcc;
	/**
	 * Subject (expression)
	 * <br/>
	 * The subject of the communication. Bindings are allowed relative to the above module document.
	 **/
	private String subject;
	/**
	 * Body (expression)
	 * <br/>
	 * The body of the communication.  
			<p/>
			Bindings are allowed relative to the above module document.
			<p/>
			To include images in the HTML, switch to the Source view, and embed the 64bit encoding from a site like 
			http://www.freeformatter.com/base64-encoder.html
	 **/
	private String body;
	/**
	 * Results
	 **/
	private String results;
	/**
	 * Attachment 1
	 **/
	private String attachment1;
	/**
	 * Attachment File Name
	 * <br/>
	 * The file name for the attachment as it will appear to receivers.
	 **/
	private String attachmentFileName1;
	/**
	 * Attachment 2
	 **/
	private String attachment2;
	/**
	 * Attachment File Name
	 * <br/>
	 * The file name for the attachment as it will appear to receivers.
	 **/
	private String attachmentFileName2;
	/**
	 * Attachment 3
	 **/
	private String attachment3;
	/**
	 * Attachment File Name
	 * <br/>
	 * The file name for the attachment as it will appear to receivers.
	 **/
	private String attachmentFileName3;
	/**
	 * Action
	 **/
	private ActionType actionType;
	/**
	 * Format
	 **/
	private FormatType formatType;
	/**
	 * Selected Batch Timestamp Folder Name
	 **/
	private String selectedBatchTimestampFolderName;
	/**
	 * Refresh Batches
	 **/
	private Boolean refreshBatches = new Boolean(true);
	/**
	 * Used for System communications
	 * <br/>
	 * System communications can not be deleted unless the system flag is cleared first.
	 **/
	private Boolean system;
	/**
	 * UnsubscribeUrl
	 **/
	private String unsubscribeUrl;
	/**
	 * Include Calendar Item
	 * <br/>
	 * <b>Include Calendar Item</b>:
				<br/>
				Includes links for Google, Yahoo and .ics attachment for Outlook and iCal calendar events.
				<br/>
				Check Options page for more.
	 **/
	private Boolean includeCalendar;
	/**
	 * Title (expression)
	 * <br/>
	 * Specify the title for the calendar item as a binding expression relative to the module document.
	 **/
	private String calendarTitleExpression;
	/**
	 * Start Time
	 **/
	private DateTime calendarStartTime;
	/**
	 * End Time
	 **/
	private DateTime calendarEndTime;
	/**
	 * Description (expression)
	 * <br/>
	 * <b>Description (expression)</b>
			<br/>
			Specify a description for the calendar item as a binding expression relative to the module document.
			<br/>
			NOTE: Google and Yahoo calendar links do not support multi-line descriptions.
	 **/
	private String calendarDescriptionExpression;
	/**
	 * Image
	 **/
	private String mailImage;
	/**
	 * Subscriptions
	 **/
	private List<Subscription> subscriptions = new ArrayList<>();

	@Override
	@XmlTransient
	public String getBizModule() {
		return Communication.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return Communication.DOCUMENT_NAME;
	}

	public static Communication newInstance() throws Exception {
		return CORE.getUser().getCustomer().getModule(MODULE_NAME).getDocument(CORE.getUser().getCustomer(), DOCUMENT_NAME).newInstance(CORE.getUser());
	}

	@Override
	@XmlTransient
	public String getBizKey() {
		try {
			return org.skyve.util.Binder.formatMessage(org.skyve.CORE.getUser().getCustomer(),
														"{description}",
														this);
		}
		catch (Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof Communication) && 
					this.getBizId().equals(((Communication) o).getBizId()));
	}

	/**
	 * {@link #description} accessor.
	 * @return	The value.
	 **/
	public String getDescription() {
		return description;
	}

	/**
	 * {@link #description} mutator.
	 * @param description	The new value.
	 **/
	@XmlElement
	public void setDescription(String description) {
		preset(descriptionPropertyName, description);
		this.description = description;
	}

	/**
	 * {@link #tag} accessor.
	 * @return	The value.
	 **/
	public Tag getTag() {
		return tag;
	}

	/**
	 * {@link #tag} mutator.
	 * @param tag	The new value.
	 **/
	@XmlElement
	public void setTag(Tag tag) {
		preset(tagPropertyName, tag);
		this.tag = tag;
	}

	/**
	 * {@link #moduleName} accessor.
	 * @return	The value.
	 **/
	public String getModuleName() {
		return moduleName;
	}

	/**
	 * {@link #moduleName} mutator.
	 * @param moduleName	The new value.
	 **/
	@XmlElement
	public void setModuleName(String moduleName) {
		preset(moduleNamePropertyName, moduleName);
		this.moduleName = moduleName;
	}

	/**
	 * {@link #documentName} accessor.
	 * @return	The value.
	 **/
	public String getDocumentName() {
		return documentName;
	}

	/**
	 * {@link #documentName} mutator.
	 * @param documentName	The new value.
	 **/
	@XmlElement
	public void setDocumentName(String documentName) {
		preset(documentNamePropertyName, documentName);
		this.documentName = documentName;
	}

	/**
	 * {@link #toBinding} accessor.
	 * @return	The value.
	 **/
	public String getToBinding() {
		return toBinding;
	}

	/**
	 * {@link #toBinding} mutator.
	 * @param toBinding	The new value.
	 **/
	@XmlElement
	public void setToBinding(String toBinding) {
		preset(toBindingPropertyName, toBinding);
		this.toBinding = toBinding;
	}

	/**
	 * {@link #sendTo} accessor.
	 * @return	The value.
	 **/
	public String getSendTo() {
		return sendTo;
	}

	/**
	 * {@link #sendTo} mutator.
	 * @param sendTo	The new value.
	 **/
	@XmlElement
	public void setSendTo(String sendTo) {
		preset(sendToPropertyName, sendTo);
		this.sendTo = sendTo;
	}

	/**
	 * {@link #ccTo} accessor.
	 * @return	The value.
	 **/
	public String getCcTo() {
		return ccTo;
	}

	/**
	 * {@link #ccTo} mutator.
	 * @param ccTo	The new value.
	 **/
	@XmlElement
	public void setCcTo(String ccTo) {
		preset(ccToPropertyName, ccTo);
		this.ccTo = ccTo;
	}

	/**
	 * {@link #sendToOverride} accessor.
	 * @return	The value.
	 **/
	public String getSendToOverride() {
		return sendToOverride;
	}

	/**
	 * {@link #sendToOverride} mutator.
	 * @param sendToOverride	The new value.
	 **/
	@XmlElement
	public void setSendToOverride(String sendToOverride) {
		this.sendToOverride = sendToOverride;
	}

	/**
	 * {@link #ccToOverride} accessor.
	 * @return	The value.
	 **/
	public String getCcToOverride() {
		return ccToOverride;
	}

	/**
	 * {@link #ccToOverride} mutator.
	 * @param ccToOverride	The new value.
	 **/
	@XmlElement
	public void setCcToOverride(String ccToOverride) {
		this.ccToOverride = ccToOverride;
	}

	/**
	 * {@link #sendFrom} accessor.
	 * @return	The value.
	 **/
	public String getSendFrom() {
		return sendFrom;
	}

	/**
	 * {@link #sendFrom} mutator.
	 * @param sendFrom	The new value.
	 **/
	@XmlElement
	public void setSendFrom(String sendFrom) {
		preset(sendFromPropertyName, sendFrom);
		this.sendFrom = sendFrom;
	}

	/**
	 * {@link #monitorBcc} accessor.
	 * @return	The value.
	 **/
	public Boolean getMonitorBcc() {
		return monitorBcc;
	}

	/**
	 * {@link #monitorBcc} mutator.
	 * @param monitorBcc	The new value.
	 **/
	@XmlElement
	public void setMonitorBcc(Boolean monitorBcc) {
		preset(monitorBccPropertyName, monitorBcc);
		this.monitorBcc = monitorBcc;
	}

	/**
	 * {@link #subject} accessor.
	 * @return	The value.
	 **/
	public String getSubject() {
		return subject;
	}

	/**
	 * {@link #subject} mutator.
	 * @param subject	The new value.
	 **/
	@XmlElement
	public void setSubject(String subject) {
		preset(subjectPropertyName, subject);
		this.subject = subject;
	}

	/**
	 * {@link #body} accessor.
	 * @return	The value.
	 **/
	public String getBody() {
		return body;
	}

	/**
	 * {@link #body} mutator.
	 * @param body	The new value.
	 **/
	@XmlElement
	public void setBody(String body) {
		preset(bodyPropertyName, body);
		this.body = body;
	}

	/**
	 * {@link #results} accessor.
	 * @return	The value.
	 **/
	public String getResults() {
		return results;
	}

	/**
	 * {@link #results} mutator.
	 * @param results	The new value.
	 **/
	@XmlElement
	public void setResults(String results) {
		preset(resultsPropertyName, results);
		this.results = results;
	}

	/**
	 * {@link #attachment1} accessor.
	 * @return	The value.
	 **/
	public String getAttachment1() {
		return attachment1;
	}

	/**
	 * {@link #attachment1} mutator.
	 * @param attachment1	The new value.
	 **/
	@XmlElement
	public void setAttachment1(String attachment1) {
		preset(attachment1PropertyName, attachment1);
		this.attachment1 = attachment1;
	}

	/**
	 * {@link #attachmentFileName1} accessor.
	 * @return	The value.
	 **/
	public String getAttachmentFileName1() {
		return attachmentFileName1;
	}

	/**
	 * {@link #attachmentFileName1} mutator.
	 * @param attachmentFileName1	The new value.
	 **/
	@XmlElement
	public void setAttachmentFileName1(String attachmentFileName1) {
		preset(attachmentFileName1PropertyName, attachmentFileName1);
		this.attachmentFileName1 = attachmentFileName1;
	}

	/**
	 * {@link #attachment2} accessor.
	 * @return	The value.
	 **/
	public String getAttachment2() {
		return attachment2;
	}

	/**
	 * {@link #attachment2} mutator.
	 * @param attachment2	The new value.
	 **/
	@XmlElement
	public void setAttachment2(String attachment2) {
		preset(attachment2PropertyName, attachment2);
		this.attachment2 = attachment2;
	}

	/**
	 * {@link #attachmentFileName2} accessor.
	 * @return	The value.
	 **/
	public String getAttachmentFileName2() {
		return attachmentFileName2;
	}

	/**
	 * {@link #attachmentFileName2} mutator.
	 * @param attachmentFileName2	The new value.
	 **/
	@XmlElement
	public void setAttachmentFileName2(String attachmentFileName2) {
		preset(attachmentFileName2PropertyName, attachmentFileName2);
		this.attachmentFileName2 = attachmentFileName2;
	}

	/**
	 * {@link #attachment3} accessor.
	 * @return	The value.
	 **/
	public String getAttachment3() {
		return attachment3;
	}

	/**
	 * {@link #attachment3} mutator.
	 * @param attachment3	The new value.
	 **/
	@XmlElement
	public void setAttachment3(String attachment3) {
		preset(attachment3PropertyName, attachment3);
		this.attachment3 = attachment3;
	}

	/**
	 * {@link #attachmentFileName3} accessor.
	 * @return	The value.
	 **/
	public String getAttachmentFileName3() {
		return attachmentFileName3;
	}

	/**
	 * {@link #attachmentFileName3} mutator.
	 * @param attachmentFileName3	The new value.
	 **/
	@XmlElement
	public void setAttachmentFileName3(String attachmentFileName3) {
		preset(attachmentFileName3PropertyName, attachmentFileName3);
		this.attachmentFileName3 = attachmentFileName3;
	}

	/**
	 * {@link #actionType} accessor.
	 * @return	The value.
	 **/
	public ActionType getActionType() {
		return actionType;
	}

	/**
	 * {@link #actionType} mutator.
	 * @param actionType	The new value.
	 **/
	@XmlElement
	public void setActionType(ActionType actionType) {
		this.actionType = actionType;
	}

	/**
	 * {@link #formatType} accessor.
	 * @return	The value.
	 **/
	public FormatType getFormatType() {
		return formatType;
	}

	/**
	 * {@link #formatType} mutator.
	 * @param formatType	The new value.
	 **/
	@XmlElement
	public void setFormatType(FormatType formatType) {
		preset(formatTypePropertyName, formatType);
		this.formatType = formatType;
	}

	/**
	 * {@link #selectedBatchTimestampFolderName} accessor.
	 * @return	The value.
	 **/
	public String getSelectedBatchTimestampFolderName() {
		return selectedBatchTimestampFolderName;
	}

	/**
	 * {@link #selectedBatchTimestampFolderName} mutator.
	 * @param selectedBatchTimestampFolderName	The new value.
	 **/
	@XmlElement
	public void setSelectedBatchTimestampFolderName(String selectedBatchTimestampFolderName) {
		this.selectedBatchTimestampFolderName = selectedBatchTimestampFolderName;
	}

	/**
	 * {@link #refreshBatches} accessor.
	 * @return	The value.
	 **/
	public Boolean getRefreshBatches() {
		return refreshBatches;
	}

	/**
	 * {@link #refreshBatches} mutator.
	 * @param refreshBatches	The new value.
	 **/
	@XmlElement
	public void setRefreshBatches(Boolean refreshBatches) {
		this.refreshBatches = refreshBatches;
	}

	/**
	 * {@link #system} accessor.
	 * @return	The value.
	 **/
	public Boolean getSystem() {
		return system;
	}

	/**
	 * {@link #system} mutator.
	 * @param system	The new value.
	 **/
	@XmlElement
	public void setSystem(Boolean system) {
		preset(systemPropertyName, system);
		this.system = system;
	}

	/**
	 * {@link #unsubscribeUrl} accessor.
	 * @return	The value.
	 **/
	public String getUnsubscribeUrl() {
		return unsubscribeUrl;
	}

	/**
	 * {@link #unsubscribeUrl} mutator.
	 * @param unsubscribeUrl	The new value.
	 **/
	@XmlElement
	public void setUnsubscribeUrl(String unsubscribeUrl) {
		this.unsubscribeUrl = unsubscribeUrl;
	}

	/**
	 * {@link #includeCalendar} accessor.
	 * @return	The value.
	 **/
	public Boolean getIncludeCalendar() {
		return includeCalendar;
	}

	/**
	 * {@link #includeCalendar} mutator.
	 * @param includeCalendar	The new value.
	 **/
	@XmlElement
	public void setIncludeCalendar(Boolean includeCalendar) {
		preset(includeCalendarPropertyName, includeCalendar);
		this.includeCalendar = includeCalendar;
	}

	/**
	 * {@link #calendarTitleExpression} accessor.
	 * @return	The value.
	 **/
	public String getCalendarTitleExpression() {
		return calendarTitleExpression;
	}

	/**
	 * {@link #calendarTitleExpression} mutator.
	 * @param calendarTitleExpression	The new value.
	 **/
	@XmlElement
	public void setCalendarTitleExpression(String calendarTitleExpression) {
		preset(calendarTitleExpressionPropertyName, calendarTitleExpression);
		this.calendarTitleExpression = calendarTitleExpression;
	}

	/**
	 * {@link #calendarStartTime} accessor.
	 * @return	The value.
	 **/
	public DateTime getCalendarStartTime() {
		return calendarStartTime;
	}

	/**
	 * {@link #calendarStartTime} mutator.
	 * @param calendarStartTime	The new value.
	 **/
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
	@XmlElement
	public void setCalendarStartTime(DateTime calendarStartTime) {
		preset(calendarStartTimePropertyName, calendarStartTime);
		this.calendarStartTime = calendarStartTime;
	}

	/**
	 * {@link #calendarEndTime} accessor.
	 * @return	The value.
	 **/
	public DateTime getCalendarEndTime() {
		return calendarEndTime;
	}

	/**
	 * {@link #calendarEndTime} mutator.
	 * @param calendarEndTime	The new value.
	 **/
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
	@XmlElement
	public void setCalendarEndTime(DateTime calendarEndTime) {
		preset(calendarEndTimePropertyName, calendarEndTime);
		this.calendarEndTime = calendarEndTime;
	}

	/**
	 * {@link #calendarDescriptionExpression} accessor.
	 * @return	The value.
	 **/
	public String getCalendarDescriptionExpression() {
		return calendarDescriptionExpression;
	}

	/**
	 * {@link #calendarDescriptionExpression} mutator.
	 * @param calendarDescriptionExpression	The new value.
	 **/
	@XmlElement
	public void setCalendarDescriptionExpression(String calendarDescriptionExpression) {
		preset(calendarDescriptionExpressionPropertyName, calendarDescriptionExpression);
		this.calendarDescriptionExpression = calendarDescriptionExpression;
	}

	/**
	 * {@link #mailImage} accessor.
	 * @return	The value.
	 **/
	public String getMailImage() {
		return mailImage;
	}

	/**
	 * {@link #mailImage} mutator.
	 * @param mailImage	The new value.
	 **/
	@XmlElement
	public void setMailImage(String mailImage) {
		preset(mailImagePropertyName, mailImage);
		this.mailImage = mailImage;
	}

	/**
	 * {@link #subscriptions} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<Subscription> getSubscriptions() {
		return subscriptions;
	}

	/**
	 * {@link #subscriptions} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public Subscription getSubscriptionsElementById(String bizId) {
		return getElementById(subscriptions, bizId);
	}

	/**
	 * Batch Selected
	 * @return	The condition

	 */
	@XmlTransient
	public boolean isBatchSelected() {
		return (selectedBatchTimestampFolderName != null);
	}

	/**	 * {@link #isBatchSelected} negation.

	 * @return	The negated condition

	 */
	public boolean isNotBatchSelected() {
		return (! isBatchSelected());
	}

	/**
	 * Refresh Batches
	 * @return	The condition

	 */
	@XmlTransient
	public boolean isBatchesRefreshRequired() {
		return (Boolean.TRUE.equals(refreshBatches));
	}

	/**	 * {@link #isBatchesRefreshRequired} negation.

	 * @return	The negated condition

	 */
	public boolean isNotBatchesRefreshRequired() {
		return (! isBatchesRefreshRequired());
	}

	/**
	 * Email type format
	 * @return	The condition

	 */
	@XmlTransient
	public boolean isEmailType() {
		return (FormatType.email.equals(this.getFormatType()));
	}

	/**	 * {@link #isEmailType} negation.

	 * @return	The negated condition

	 */
	public boolean isNotEmailType() {
		return (! isEmailType());
	}

	/**
	 * Includes Calendar
	 * @return	The condition

	 */
	@XmlTransient
	public boolean isIncludesCalendar() {
		return (Boolean.TRUE.equals(getIncludeCalendar()));
	}

	/**	 * {@link #isIncludesCalendar} negation.

	 * @return	The negated condition

	 */
	public boolean isNotIncludesCalendar() {
		return (! isIncludesCalendar());
	}

	/**
	 * Locked
	 * @return	The condition

	 */
	@XmlTransient
	public boolean isLocked() {
		return (isPersisted() && Boolean.TRUE.equals(getSystem()));
	}

	/**	 * {@link #isLocked} negation.

	 * @return	The negated condition

	 */
	public boolean isNotLocked() {
		return (! isLocked());
	}

	/**
	 * Save for Bulk Send
	 * @return	The condition

	 */
	@XmlTransient
	public boolean isSaveAction() {
		return (ActionType.saveForBulkSend.equals(this.getActionType()));
	}

	/**	 * {@link #isSaveAction} negation.

	 * @return	The negated condition

	 */
	public boolean isNotSaveAction() {
		return (! isSaveAction());
	}
}
