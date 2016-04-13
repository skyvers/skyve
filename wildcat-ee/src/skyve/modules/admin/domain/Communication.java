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
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.wildcat.domain.AbstractPersistentBean;
import org.skyve.wildcat.domain.types.jaxb.DateTimeMapper;

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
	public static final String sendToOverridePropertyName = "sendToOverride";
	/** @hidden */
	public static final String sendFromPropertyName = "sendFrom";
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
		private static List<DomainValue> domainValues;

		private ActionType(String code, String description) {
			this.code = code;
			this.description = description;
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toDescription() {
			return description;
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
					domainValues.add(new DomainValue(value.code, value.description));
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
		private static List<DomainValue> domainValues;

		private FormatType(String code, String description) {
			this.code = code;
			this.description = description;
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toDescription() {
			return description;
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
					domainValues.add(new DomainValue(value.code, value.description));
				}
			}

			return domainValues;
		}
	}

	private String description;
	private Tag tag = null;
	/**
	 * Bindings used in the communication address, subject and body will be based on the selected module document.
	 **/
	private String moduleName;
	/**
	 * Bindings used in the communication address, subject and body will be based on the selected module document.
	 **/
	private String documentName;
	/**
	 * Provide a binding which contains the email address to send to
	 **/
	private String toBinding;
	/**
	 * The address to send to. Bindings are allowed relative to the above module document.
	 **/
	private String sendTo;
	/**
	 * The address to send to. Bindings are allowed relative to the above module document.
	 **/
	private String sendToOverride;
	/**
	 * The address to send from.
	 **/
	private String sendFrom;
	/**
	 * The subject of the communication. Bindings are allowed relative to the above module document.
	 **/
	private String subject;
	/**
	 * The body of the communication.  
			<p/>
			Bindings are allowed relative to the above module document.
			<p/>
			To include images in the HTML, switch to the Source view, and embed the 64bit encoding from a site like 
			http://www.freeformatter.com/base64-encoder.html
	 **/
	private String body;
	private String results;
	private String attachment1;
	/**
	 * The file name for the attachment as it will appear to receivers.
	 **/
	private String attachmentFileName1;
	private String attachment2;
	/**
	 * The file name for the attachment as it will appear to receivers.
	 **/
	private String attachmentFileName2;
	private String attachment3;
	/**
	 * The file name for the attachment as it will appear to receivers.
	 **/
	private String attachmentFileName3;
	private ActionType actionType;
	private FormatType formatType;
	private String selectedBatchTimestampFolderName;
	private Boolean refreshBatches = new Boolean(true);
	/**
	 * System communications can not be deleted unless the system flag is cleared first.
	 **/
	private Boolean system;
	private String unsubscribeUrl;
	/**
	 * <b>Include Calendar Item</b>:
				<br/>
				Includes links for Google, Yahoo and .ics attachment for Outlook and iCal calendar events.
				<br/>
				Check Options page for more.
	 **/
	private Boolean includeCalendar;
	/**
	 * Specify the title for the calendar item as a binding expression relative to the module document.
	 **/
	private String calendarTitleExpression;
	private DateTime calendarStartTime;
	private DateTime calendarEndTime;
	/**
	 * <b>Description (expression)</b>
			<br/>
			Specify a description for the calendar item as a binding expression relative to the module document.
			<br/>
			NOTE: Google and Yahoo calendar links do not support multi-line descriptions.
	 **/
	private String calendarDescriptionExpression;
	private String mailImage;
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
	 **/
	public String getDescription() {
		return description;
	}

	/**
	 * {@link #description} mutator.
	 * 
	 * @param description	The new value to set.
	 **/
	@XmlElement
	public void setDescription(String description) {
		preset(descriptionPropertyName, description);
		this.description = description;
	}

	/**
	 * {@link #tag} accessor.
	 **/
	public Tag getTag() {
		return tag;
	}

	/**
	 * {@link #tag} mutator.
	 * 
	 * @param tag	The new value to set.
	 **/
	@XmlElement
	public void setTag(Tag tag) {
		preset(tagPropertyName, tag);
		this.tag = tag;
	}

	/**
	 * {@link #moduleName} accessor.
	 **/
	public String getModuleName() {
		return moduleName;
	}

	/**
	 * {@link #moduleName} mutator.
	 * 
	 * @param moduleName	The new value to set.
	 **/
	@XmlElement
	public void setModuleName(String moduleName) {
		preset(moduleNamePropertyName, moduleName);
		this.moduleName = moduleName;
	}

	/**
	 * {@link #documentName} accessor.
	 **/
	public String getDocumentName() {
		return documentName;
	}

	/**
	 * {@link #documentName} mutator.
	 * 
	 * @param documentName	The new value to set.
	 **/
	@XmlElement
	public void setDocumentName(String documentName) {
		preset(documentNamePropertyName, documentName);
		this.documentName = documentName;
	}

	/**
	 * {@link #toBinding} accessor.
	 **/
	public String getToBinding() {
		return toBinding;
	}

	/**
	 * {@link #toBinding} mutator.
	 * 
	 * @param toBinding	The new value to set.
	 **/
	@XmlElement
	public void setToBinding(String toBinding) {
		preset(toBindingPropertyName, toBinding);
		this.toBinding = toBinding;
	}

	/**
	 * {@link #sendTo} accessor.
	 **/
	public String getSendTo() {
		return sendTo;
	}

	/**
	 * {@link #sendTo} mutator.
	 * 
	 * @param sendTo	The new value to set.
	 **/
	@XmlElement
	public void setSendTo(String sendTo) {
		preset(sendToPropertyName, sendTo);
		this.sendTo = sendTo;
	}

	/**
	 * {@link #sendToOverride} accessor.
	 **/
	public String getSendToOverride() {
		return sendToOverride;
	}

	/**
	 * {@link #sendToOverride} mutator.
	 * 
	 * @param sendToOverride	The new value to set.
	 **/
	@XmlElement
	public void setSendToOverride(String sendToOverride) {
		this.sendToOverride = sendToOverride;
	}

	/**
	 * {@link #sendFrom} accessor.
	 **/
	public String getSendFrom() {
		return sendFrom;
	}

	/**
	 * {@link #sendFrom} mutator.
	 * 
	 * @param sendFrom	The new value to set.
	 **/
	@XmlElement
	public void setSendFrom(String sendFrom) {
		preset(sendFromPropertyName, sendFrom);
		this.sendFrom = sendFrom;
	}

	/**
	 * {@link #subject} accessor.
	 **/
	public String getSubject() {
		return subject;
	}

	/**
	 * {@link #subject} mutator.
	 * 
	 * @param subject	The new value to set.
	 **/
	@XmlElement
	public void setSubject(String subject) {
		preset(subjectPropertyName, subject);
		this.subject = subject;
	}

	/**
	 * {@link #body} accessor.
	 **/
	public String getBody() {
		return body;
	}

	/**
	 * {@link #body} mutator.
	 * 
	 * @param body	The new value to set.
	 **/
	@XmlElement
	public void setBody(String body) {
		preset(bodyPropertyName, body);
		this.body = body;
	}

	/**
	 * {@link #results} accessor.
	 **/
	public String getResults() {
		return results;
	}

	/**
	 * {@link #results} mutator.
	 * 
	 * @param results	The new value to set.
	 **/
	@XmlElement
	public void setResults(String results) {
		preset(resultsPropertyName, results);
		this.results = results;
	}

	/**
	 * {@link #attachment1} accessor.
	 **/
	public String getAttachment1() {
		return attachment1;
	}

	/**
	 * {@link #attachment1} mutator.
	 * 
	 * @param attachment1	The new value to set.
	 **/
	@XmlElement
	public void setAttachment1(String attachment1) {
		preset(attachment1PropertyName, attachment1);
		this.attachment1 = attachment1;
	}

	/**
	 * {@link #attachmentFileName1} accessor.
	 **/
	public String getAttachmentFileName1() {
		return attachmentFileName1;
	}

	/**
	 * {@link #attachmentFileName1} mutator.
	 * 
	 * @param attachmentFileName1	The new value to set.
	 **/
	@XmlElement
	public void setAttachmentFileName1(String attachmentFileName1) {
		preset(attachmentFileName1PropertyName, attachmentFileName1);
		this.attachmentFileName1 = attachmentFileName1;
	}

	/**
	 * {@link #attachment2} accessor.
	 **/
	public String getAttachment2() {
		return attachment2;
	}

	/**
	 * {@link #attachment2} mutator.
	 * 
	 * @param attachment2	The new value to set.
	 **/
	@XmlElement
	public void setAttachment2(String attachment2) {
		preset(attachment2PropertyName, attachment2);
		this.attachment2 = attachment2;
	}

	/**
	 * {@link #attachmentFileName2} accessor.
	 **/
	public String getAttachmentFileName2() {
		return attachmentFileName2;
	}

	/**
	 * {@link #attachmentFileName2} mutator.
	 * 
	 * @param attachmentFileName2	The new value to set.
	 **/
	@XmlElement
	public void setAttachmentFileName2(String attachmentFileName2) {
		preset(attachmentFileName2PropertyName, attachmentFileName2);
		this.attachmentFileName2 = attachmentFileName2;
	}

	/**
	 * {@link #attachment3} accessor.
	 **/
	public String getAttachment3() {
		return attachment3;
	}

	/**
	 * {@link #attachment3} mutator.
	 * 
	 * @param attachment3	The new value to set.
	 **/
	@XmlElement
	public void setAttachment3(String attachment3) {
		preset(attachment3PropertyName, attachment3);
		this.attachment3 = attachment3;
	}

	/**
	 * {@link #attachmentFileName3} accessor.
	 **/
	public String getAttachmentFileName3() {
		return attachmentFileName3;
	}

	/**
	 * {@link #attachmentFileName3} mutator.
	 * 
	 * @param attachmentFileName3	The new value to set.
	 **/
	@XmlElement
	public void setAttachmentFileName3(String attachmentFileName3) {
		preset(attachmentFileName3PropertyName, attachmentFileName3);
		this.attachmentFileName3 = attachmentFileName3;
	}

	/**
	 * {@link #actionType} accessor.
	 **/
	public ActionType getActionType() {
		return actionType;
	}

	/**
	 * {@link #actionType} mutator.
	 * 
	 * @param actionType	The new value to set.
	 **/
	@XmlElement
	public void setActionType(ActionType actionType) {
		this.actionType = actionType;
	}

	/**
	 * {@link #formatType} accessor.
	 **/
	public FormatType getFormatType() {
		return formatType;
	}

	/**
	 * {@link #formatType} mutator.
	 * 
	 * @param formatType	The new value to set.
	 **/
	@XmlElement
	public void setFormatType(FormatType formatType) {
		preset(formatTypePropertyName, formatType);
		this.formatType = formatType;
	}

	/**
	 * {@link #selectedBatchTimestampFolderName} accessor.
	 **/
	public String getSelectedBatchTimestampFolderName() {
		return selectedBatchTimestampFolderName;
	}

	/**
	 * {@link #selectedBatchTimestampFolderName} mutator.
	 * 
	 * @param selectedBatchTimestampFolderName	The new value to set.
	 **/
	@XmlElement
	public void setSelectedBatchTimestampFolderName(String selectedBatchTimestampFolderName) {
		this.selectedBatchTimestampFolderName = selectedBatchTimestampFolderName;
	}

	/**
	 * {@link #refreshBatches} accessor.
	 **/
	public Boolean getRefreshBatches() {
		return refreshBatches;
	}

	/**
	 * {@link #refreshBatches} mutator.
	 * 
	 * @param refreshBatches	The new value to set.
	 **/
	@XmlElement
	public void setRefreshBatches(Boolean refreshBatches) {
		this.refreshBatches = refreshBatches;
	}

	/**
	 * {@link #system} accessor.
	 **/
	public Boolean getSystem() {
		return system;
	}

	/**
	 * {@link #system} mutator.
	 * 
	 * @param system	The new value to set.
	 **/
	@XmlElement
	public void setSystem(Boolean system) {
		preset(systemPropertyName, system);
		this.system = system;
	}

	/**
	 * {@link #unsubscribeUrl} accessor.
	 **/
	public String getUnsubscribeUrl() {
		return unsubscribeUrl;
	}

	/**
	 * {@link #unsubscribeUrl} mutator.
	 * 
	 * @param unsubscribeUrl	The new value to set.
	 **/
	@XmlElement
	public void setUnsubscribeUrl(String unsubscribeUrl) {
		this.unsubscribeUrl = unsubscribeUrl;
	}

	/**
	 * {@link #includeCalendar} accessor.
	 **/
	public Boolean getIncludeCalendar() {
		return includeCalendar;
	}

	/**
	 * {@link #includeCalendar} mutator.
	 * 
	 * @param includeCalendar	The new value to set.
	 **/
	@XmlElement
	public void setIncludeCalendar(Boolean includeCalendar) {
		preset(includeCalendarPropertyName, includeCalendar);
		this.includeCalendar = includeCalendar;
	}

	/**
	 * {@link #calendarTitleExpression} accessor.
	 **/
	public String getCalendarTitleExpression() {
		return calendarTitleExpression;
	}

	/**
	 * {@link #calendarTitleExpression} mutator.
	 * 
	 * @param calendarTitleExpression	The new value to set.
	 **/
	@XmlElement
	public void setCalendarTitleExpression(String calendarTitleExpression) {
		preset(calendarTitleExpressionPropertyName, calendarTitleExpression);
		this.calendarTitleExpression = calendarTitleExpression;
	}

	/**
	 * {@link #calendarStartTime} accessor.
	 **/
	public DateTime getCalendarStartTime() {
		return calendarStartTime;
	}

	/**
	 * {@link #calendarStartTime} mutator.
	 * 
	 * @param calendarStartTime	The new value to set.
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
	 **/
	public DateTime getCalendarEndTime() {
		return calendarEndTime;
	}

	/**
	 * {@link #calendarEndTime} mutator.
	 * 
	 * @param calendarEndTime	The new value to set.
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
	 **/
	public String getCalendarDescriptionExpression() {
		return calendarDescriptionExpression;
	}

	/**
	 * {@link #calendarDescriptionExpression} mutator.
	 * 
	 * @param calendarDescriptionExpression	The new value to set.
	 **/
	@XmlElement
	public void setCalendarDescriptionExpression(String calendarDescriptionExpression) {
		preset(calendarDescriptionExpressionPropertyName, calendarDescriptionExpression);
		this.calendarDescriptionExpression = calendarDescriptionExpression;
	}

	/**
	 * {@link #mailImage} accessor.
	 **/
	public String getMailImage() {
		return mailImage;
	}

	/**
	 * {@link #mailImage} mutator.
	 * 
	 * @param mailImage	The new value to set.
	 **/
	@XmlElement
	public void setMailImage(String mailImage) {
		preset(mailImagePropertyName, mailImage);
		this.mailImage = mailImage;
	}

	/**
	 * {@link #subscriptions} accessor.
	 **/
	@XmlElement
	public List<Subscription> getSubscriptions() {
		return subscriptions;
	}

	/**
	 * {@link #subscriptions} accessor.
	 * 
	 * @param bizId	The bizId of the element in the list.
	 **/
	public Subscription getSubscriptionsElementById(String bizId) {
		return getElementById(subscriptions, bizId);
	}

	/**
	 * Batch Selected
	 */
	@XmlTransient
	public boolean isBatchSelected() {
		return (selectedBatchTimestampFolderName != null);
	}

	public boolean isNotBatchSelected() {
		return (! isBatchSelected());
	}

	/**
	 * Refresh Batches
	 */
	@XmlTransient
	public boolean isBatchesRefreshRequired() {
		return (Boolean.TRUE.equals(refreshBatches));
	}

	public boolean isNotBatchesRefreshRequired() {
		return (! isBatchesRefreshRequired());
	}

	/**
	 * Email type format
	 */
	@XmlTransient
	public boolean isEmailType() {
		return (FormatType.email.equals(this.getFormatType()));
	}

	public boolean isNotEmailType() {
		return (! isEmailType());
	}

	/**
	 * Includes Calendar
	 */
	@XmlTransient
	public boolean isIncludesCalendar() {
		return (Boolean.TRUE.equals(getIncludeCalendar()));
	}

	public boolean isNotIncludesCalendar() {
		return (! isIncludesCalendar());
	}

	/**
	 * Locked
	 */
	@XmlTransient
	public boolean isLocked() {
		return (isPersisted() && Boolean.TRUE.equals(getSystem()));
	}

	public boolean isNotLocked() {
		return (! isLocked());
	}

	/**
	 * Save for Bulk Send
	 */
	@XmlTransient
	public boolean isSaveAction() {
		return (ActionType.saveForBulkSend.equals(this.getActionType()));
	}

	public boolean isNotSaveAction() {
		return (! isSaveAction());
	}
}
