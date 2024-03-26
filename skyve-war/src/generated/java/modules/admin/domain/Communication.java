package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlSchemaType;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import modules.admin.Communication.CommunicationExtension;
import modules.admin.Tag.TagExtension;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.DateTime;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.impl.domain.types.jaxb.DateTimeMapper;

/**
 * Communication
 * 
 * @depend - - - ActionType
 * @depend - - - FormatType
 * @navhas n template 0..1 CommunicationTemplate
 * @navhas n tag 0..1 Tag
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public abstract class Communication extends AbstractPersistentBean implements org.skyve.domain.app.admin.Communication {
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
	public static final String moduleNamePropertyName = "moduleName";

	/** @hidden */
	public static final String documentNamePropertyName = "documentName";

	/** @hidden */
	public static final String tagPropertyName = "tag";

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
	public static final String unTagSuccessfulPropertyName = "unTagSuccessful";

	/** @hidden */
	public static final String notificationPropertyName = "notification";

	/** @hidden */
	public static final String systemUsePropertyName = "systemUse";

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
	public static final String templatePropertyName = "template";

	/** @hidden */
	public static final String basePathPropertyName = "basePath";

	/** @hidden */
	public static final String batchPropertyName = "batch";

	/**
	 * Description
	 **/
	private String description;

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
	 * Tag
	 * <br/>
	 * <p>Tag is transient:
			<ul><li>as good practice so that the user is forced to reconsider which
			tag is used for the bulk communication each time, and</li>
			<li>using a tag in the communication should not block the tag being removed
			by normal user actions through the list functions</li></ul>
			</p>
	 **/
	private TagExtension tag = null;

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
	 * Monitor outgoing emails by including yourself in the Bcc
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
	 * 
		The body of the communication.  
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
	private Boolean refreshBatches = Boolean.valueOf(true);

	/**
	 * Untag successful documents
	 **/
	private Boolean unTagSuccessful;

	/**
	 * Notify when job is complete
	 **/
	private Boolean notification;

	/**
	 * Used for System communications
	 * <br/>
	 * System communications can not be deleted unless the system flag is cleared first.
	 **/
	private Boolean systemUse;

	/**
	 * admin.commmunication.unsubscribe.displayName
	 **/
	private String unsubscribeUrl;

	/**
	 * Include Calendar Item
	 * <br/>
	 * 
	<b>Include Calendar Item</b>:
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
	 * 	<b>Description (expression)</b>
	<br/>
	Specify a description for the calendar item as a binding expression relative to the module document.
	<br/>
	NOTE: Google and Yahoo calendar links do not support multi-line 	descriptions.
	 **/
	private String calendarDescriptionExpression;

	/**
	 * Image
	 **/
	private String mailImage;

	/**
	 * Communication Template
	 * <br/>
	 * <em>Optional</em> Select a communication template to use for this communication.
	 **/
	private CommunicationTemplate template = null;

	/**
	 * File Path to batches for this communication
	 **/
	private String basePath;

	/**
	 * Batch
	 * <br/>
	 * The batch identifier for a current bulk creation for this communication (in the format yyyyMMddHHmmss)
	 **/
	private String batch;

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

	public static CommunicationExtension newInstance() {
		try {
			return CORE.getUser().getCustomer().getModule(MODULE_NAME).getDocument(CORE.getUser().getCustomer(), DOCUMENT_NAME).newInstance(CORE.getUser());
		}
		catch (RuntimeException e) {
			throw e;
		}
		catch (Exception e) {
			throw new DomainException(e);
		}
	}

	@Override
	@XmlTransient
	public String getBizKey() {
		try {
			return org.skyve.util.Binder.formatMessage("{description}", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
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
	 * {@link #tag} accessor.
	 * @return	The value.
	 **/
	public TagExtension getTag() {
		return tag;
	}

	/**
	 * {@link #tag} mutator.
	 * @param tag	The new value.
	 **/
	@XmlElement
	public void setTag(TagExtension tag) {
		if (this.tag != tag) {
			this.tag = tag;
		}
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
	 * {@link #unTagSuccessful} accessor.
	 * @return	The value.
	 **/
	public Boolean getUnTagSuccessful() {
		return unTagSuccessful;
	}

	/**
	 * {@link #unTagSuccessful} mutator.
	 * @param unTagSuccessful	The new value.
	 **/
	@XmlElement
	public void setUnTagSuccessful(Boolean unTagSuccessful) {
		this.unTagSuccessful = unTagSuccessful;
	}

	/**
	 * {@link #notification} accessor.
	 * @return	The value.
	 **/
	public Boolean getNotification() {
		return notification;
	}

	/**
	 * {@link #notification} mutator.
	 * @param notification	The new value.
	 **/
	@XmlElement
	public void setNotification(Boolean notification) {
		this.notification = notification;
	}

	/**
	 * {@link #systemUse} accessor.
	 * @return	The value.
	 **/
	public Boolean getSystemUse() {
		return systemUse;
	}

	/**
	 * {@link #systemUse} mutator.
	 * @param systemUse	The new value.
	 **/
	@XmlElement
	public void setSystemUse(Boolean systemUse) {
		preset(systemUsePropertyName, systemUse);
		this.systemUse = systemUse;
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
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
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
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(DateTimeMapper.class)
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
		this.mailImage = mailImage;
	}

	/**
	 * {@link #template} accessor.
	 * @return	The value.
	 **/
	public CommunicationTemplate getTemplate() {
		return template;
	}

	/**
	 * {@link #template} mutator.
	 * @param template	The new value.
	 **/
	@XmlElement
	public void setTemplate(CommunicationTemplate template) {
		if (this.template != template) {
			preset(templatePropertyName, template);
			this.template = template;
		}
	}

	/**
	 * {@link #basePath} accessor.
	 * @return	The value.
	 **/
	public String getBasePath() {
		return basePath;
	}

	/**
	 * {@link #basePath} mutator.
	 * @param basePath	The new value.
	 **/
	@XmlElement
	public void setBasePath(String basePath) {
		this.basePath = basePath;
	}

	/**
	 * {@link #batch} accessor.
	 * @return	The value.
	 **/
	public String getBatch() {
		return batch;
	}

	/**
	 * {@link #batch} mutator.
	 * @param batch	The new value.
	 **/
	@XmlElement
	public void setBatch(String batch) {
		this.batch = batch;
	}

	/**
	 * Batch Selected
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isBatchSelected() {
		return (selectedBatchTimestampFolderName != null);
	}

	/**
	 * {@link #isBatchSelected} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotBatchSelected() {
		return (! isBatchSelected());
	}

	/**
	 * Refresh Batches
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isBatchesRefreshRequired() {
		return (Boolean.TRUE.equals(refreshBatches));
	}

	/**
	 * {@link #isBatchesRefreshRequired} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotBatchesRefreshRequired() {
		return (! isBatchesRefreshRequired());
	}

	/**
	 * emailConfigured
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isEmailConfigured() {
		return (modules.admin.Configuration.ConfigurationExtension.validSMTPHost());
	}

	/**
	 * {@link #isEmailConfigured} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotEmailConfigured() {
		return (! isEmailConfigured());
	}

	/**
	 * Email type format
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isEmailType() {
		return (FormatType.email.equals(this.getFormatType()));
	}

	/**
	 * {@link #isEmailType} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotEmailType() {
		return (! isEmailType());
	}

	/**
	 * Includes Calendar
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isIncludesCalendar() {
		return (Boolean.TRUE.equals(getIncludeCalendar()));
	}

	/**
	 * {@link #isIncludesCalendar} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotIncludesCalendar() {
		return (! isIncludesCalendar());
	}

	/**
	 * Locked
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isLocked() {
		return (isPersisted() && Boolean.TRUE.equals(getSystemUse()));
	}

	/**
	 * {@link #isLocked} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotLocked() {
		return (! isLocked());
	}

	/**
	 * Save for Bulk Send
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isSaveAction() {
		return (ActionType.saveForBulkSend.equals(this.getActionType()));
	}

	/**
	 * {@link #isSaveAction} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotSaveAction() {
		return (! isSaveAction());
	}

	/**
	 * Whether to show the list of batches
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isShowBatches() {
		return (description!=null);
	}

	/**
	 * {@link #isShowBatches} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotShowBatches() {
		return (! isShowBatches());
	}
}
