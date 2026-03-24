package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlSchemaType;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.impl.domain.types.jaxb.TimestampMapper;
import org.skyve.util.ExpressionEvaluator;

/**
 * Mail Log
 * <br/>
 * The Mail Log records outbound mail dispatch requests and relay outcomes across all configured mail providers.
 * 
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class MailLog extends AbstractPersistentBean implements org.skyve.domain.app.admin.MailLog {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "MailLog";

	/** @hidden */
	public static final String timestampPropertyName = "timestamp";

	/** @hidden */
	public static final String toRecipientsPropertyName = "toRecipients";

	/** @hidden */
	public static final String ccRecipientsPropertyName = "ccRecipients";

	/** @hidden */
	public static final String bccRecipientsPropertyName = "bccRecipients";

	/** @hidden */
	public static final String subjectPropertyName = "subject";

	/** @hidden */
	public static final String bodyExcerptPropertyName = "bodyExcerpt";

	/** @hidden */
	public static final String attachmentFileNamesPropertyName = "attachmentFileNames";

	/** @hidden */
	public static final String dispatchStatusPropertyName = "dispatchStatus";

	/** @hidden */
	public static final String providerPropertyName = "provider";

	/** @hidden */
	public static final String providerMessageIdPropertyName = "providerMessageId";

	/** @hidden */
	public static final String relayStatusPropertyName = "relayStatus";

	/** @hidden */
	public static final String relayDetailPropertyName = "relayDetail";

	/** @hidden */
	public static final String errorDetailPropertyName = "errorDetail";

	/** @hidden */
	public static final String isBulkPropertyName = "isBulk";

	/** @hidden */
	public static final String mailCountPropertyName = "mailCount";

	/** @hidden */
	public static final String recipientCountPropertyName = "recipientCount";

	/** @hidden */
	public static final String hasMultipleSubjectsPropertyName = "hasMultipleSubjects";

	/** @hidden */
	public static final String subjectVariantCountPropertyName = "subjectVariantCount";

	/** @hidden */
	public static final String hasMultipleBodiesPropertyName = "hasMultipleBodies";

	/** @hidden */
	public static final String bodyVariantCountPropertyName = "bodyVariantCount";

	/**
	 * Timestamp
	 **/
	private Timestamp timestamp = (Timestamp) ExpressionEvaluator.evaluate("{TIMESTAMP}", this);

	/**
	 * To Recipients
	 **/
	private String toRecipients;

	/**
	 * CC Recipients
	 **/
	private String ccRecipients;

	/**
	 * BCC Recipients
	 **/
	private String bccRecipients;

	/**
	 * Subject
	 **/
	private String subject;

	/**
	 * Body Excerpt
	 **/
	private String bodyExcerpt;

	/**
	 * Attachment Filenames
	 **/
	private String attachmentFileNames;

	/**
	 * Dispatch Status
	 **/
	private String dispatchStatus;

	/**
	 * Provider
	 **/
	private String provider;

	/**
	 * Provider Message ID
	 **/
	private String providerMessageId;

	/**
	 * Relay Status
	 **/
	private String relayStatus;

	/**
	 * Relay Detail
	 **/
	private String relayDetail;

	/**
	 * Error Detail
	 **/
	private String errorDetail;

	/**
	 * Bulk
	 **/
	private Boolean isBulk = Boolean.valueOf(false);

	/**
	 * Mail Count
	 **/
	private Long mailCount = Long.valueOf(0);

	/**
	 * Recipient Count
	 **/
	private Long recipientCount = Long.valueOf(0);

	/**
	 * Multiple Subjects
	 **/
	private Boolean hasMultipleSubjects = Boolean.valueOf(false);

	/**
	 * Subject Variant Count
	 **/
	private Long subjectVariantCount = Long.valueOf(0);

	/**
	 * Multiple Bodies
	 **/
	private Boolean hasMultipleBodies = Boolean.valueOf(false);

	/**
	 * Body Variant Count
	 **/
	private Long bodyVariantCount = Long.valueOf(0);

	@Override
	@XmlTransient
	public String getBizModule() {
		return MailLog.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return MailLog.DOCUMENT_NAME;
	}

	public static MailLog newInstance() {
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
			return org.skyve.util.Binder.formatMessage("Mail Log - {timestamp}", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	/**
	 * {@link #timestamp} accessor.
	 * @return	The value.
	 **/
	public Timestamp getTimestamp() {
		return timestamp;
	}

	/**
	 * {@link #timestamp} mutator.
	 * @param timestamp	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	public void setTimestamp(Timestamp timestamp) {
		preset(timestampPropertyName, timestamp);
		this.timestamp = timestamp;
	}

	/**
	 * {@link #toRecipients} accessor.
	 * @return	The value.
	 **/
	public String getToRecipients() {
		return toRecipients;
	}

	/**
	 * {@link #toRecipients} mutator.
	 * @param toRecipients	The new value.
	 **/
	@XmlElement
	public void setToRecipients(String toRecipients) {
		preset(toRecipientsPropertyName, toRecipients);
		this.toRecipients = toRecipients;
	}

	/**
	 * {@link #ccRecipients} accessor.
	 * @return	The value.
	 **/
	public String getCcRecipients() {
		return ccRecipients;
	}

	/**
	 * {@link #ccRecipients} mutator.
	 * @param ccRecipients	The new value.
	 **/
	@XmlElement
	public void setCcRecipients(String ccRecipients) {
		preset(ccRecipientsPropertyName, ccRecipients);
		this.ccRecipients = ccRecipients;
	}

	/**
	 * {@link #bccRecipients} accessor.
	 * @return	The value.
	 **/
	public String getBccRecipients() {
		return bccRecipients;
	}

	/**
	 * {@link #bccRecipients} mutator.
	 * @param bccRecipients	The new value.
	 **/
	@XmlElement
	public void setBccRecipients(String bccRecipients) {
		preset(bccRecipientsPropertyName, bccRecipients);
		this.bccRecipients = bccRecipients;
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
	 * {@link #bodyExcerpt} accessor.
	 * @return	The value.
	 **/
	public String getBodyExcerpt() {
		return bodyExcerpt;
	}

	/**
	 * {@link #bodyExcerpt} mutator.
	 * @param bodyExcerpt	The new value.
	 **/
	@XmlElement
	public void setBodyExcerpt(String bodyExcerpt) {
		preset(bodyExcerptPropertyName, bodyExcerpt);
		this.bodyExcerpt = bodyExcerpt;
	}

	/**
	 * {@link #attachmentFileNames} accessor.
	 * @return	The value.
	 **/
	public String getAttachmentFileNames() {
		return attachmentFileNames;
	}

	/**
	 * {@link #attachmentFileNames} mutator.
	 * @param attachmentFileNames	The new value.
	 **/
	@XmlElement
	public void setAttachmentFileNames(String attachmentFileNames) {
		preset(attachmentFileNamesPropertyName, attachmentFileNames);
		this.attachmentFileNames = attachmentFileNames;
	}

	/**
	 * {@link #dispatchStatus} accessor.
	 * @return	The value.
	 **/
	public String getDispatchStatus() {
		return dispatchStatus;
	}

	/**
	 * {@link #dispatchStatus} mutator.
	 * @param dispatchStatus	The new value.
	 **/
	@XmlElement
	public void setDispatchStatus(String dispatchStatus) {
		preset(dispatchStatusPropertyName, dispatchStatus);
		this.dispatchStatus = dispatchStatus;
	}

	/**
	 * {@link #provider} accessor.
	 * @return	The value.
	 **/
	public String getProvider() {
		return provider;
	}

	/**
	 * {@link #provider} mutator.
	 * @param provider	The new value.
	 **/
	@XmlElement
	public void setProvider(String provider) {
		preset(providerPropertyName, provider);
		this.provider = provider;
	}

	/**
	 * {@link #providerMessageId} accessor.
	 * @return	The value.
	 **/
	public String getProviderMessageId() {
		return providerMessageId;
	}

	/**
	 * {@link #providerMessageId} mutator.
	 * @param providerMessageId	The new value.
	 **/
	@XmlElement
	public void setProviderMessageId(String providerMessageId) {
		preset(providerMessageIdPropertyName, providerMessageId);
		this.providerMessageId = providerMessageId;
	}

	/**
	 * {@link #relayStatus} accessor.
	 * @return	The value.
	 **/
	public String getRelayStatus() {
		return relayStatus;
	}

	/**
	 * {@link #relayStatus} mutator.
	 * @param relayStatus	The new value.
	 **/
	@XmlElement
	public void setRelayStatus(String relayStatus) {
		preset(relayStatusPropertyName, relayStatus);
		this.relayStatus = relayStatus;
	}

	/**
	 * {@link #relayDetail} accessor.
	 * @return	The value.
	 **/
	public String getRelayDetail() {
		return relayDetail;
	}

	/**
	 * {@link #relayDetail} mutator.
	 * @param relayDetail	The new value.
	 **/
	@XmlElement
	public void setRelayDetail(String relayDetail) {
		preset(relayDetailPropertyName, relayDetail);
		this.relayDetail = relayDetail;
	}

	/**
	 * {@link #errorDetail} accessor.
	 * @return	The value.
	 **/
	public String getErrorDetail() {
		return errorDetail;
	}

	/**
	 * {@link #errorDetail} mutator.
	 * @param errorDetail	The new value.
	 **/
	@XmlElement
	public void setErrorDetail(String errorDetail) {
		preset(errorDetailPropertyName, errorDetail);
		this.errorDetail = errorDetail;
	}

	/**
	 * {@link #isBulk} accessor.
	 * @return	The value.
	 **/
	public Boolean getIsBulk() {
		return isBulk;
	}

	/**
	 * {@link #isBulk} mutator.
	 * @param isBulk	The new value.
	 **/
	@XmlElement
	public void setIsBulk(Boolean isBulk) {
		preset(isBulkPropertyName, isBulk);
		this.isBulk = isBulk;
	}

	/**
	 * {@link #mailCount} accessor.
	 * @return	The value.
	 **/
	public Long getMailCount() {
		return mailCount;
	}

	/**
	 * {@link #mailCount} mutator.
	 * @param mailCount	The new value.
	 **/
	@XmlElement
	public void setMailCount(Long mailCount) {
		preset(mailCountPropertyName, mailCount);
		this.mailCount = mailCount;
	}

	/**
	 * {@link #recipientCount} accessor.
	 * @return	The value.
	 **/
	public Long getRecipientCount() {
		return recipientCount;
	}

	/**
	 * {@link #recipientCount} mutator.
	 * @param recipientCount	The new value.
	 **/
	@XmlElement
	public void setRecipientCount(Long recipientCount) {
		preset(recipientCountPropertyName, recipientCount);
		this.recipientCount = recipientCount;
	}

	/**
	 * {@link #hasMultipleSubjects} accessor.
	 * @return	The value.
	 **/
	public Boolean getHasMultipleSubjects() {
		return hasMultipleSubjects;
	}

	/**
	 * {@link #hasMultipleSubjects} mutator.
	 * @param hasMultipleSubjects	The new value.
	 **/
	@XmlElement
	public void setHasMultipleSubjects(Boolean hasMultipleSubjects) {
		preset(hasMultipleSubjectsPropertyName, hasMultipleSubjects);
		this.hasMultipleSubjects = hasMultipleSubjects;
	}

	/**
	 * {@link #subjectVariantCount} accessor.
	 * @return	The value.
	 **/
	public Long getSubjectVariantCount() {
		return subjectVariantCount;
	}

	/**
	 * {@link #subjectVariantCount} mutator.
	 * @param subjectVariantCount	The new value.
	 **/
	@XmlElement
	public void setSubjectVariantCount(Long subjectVariantCount) {
		preset(subjectVariantCountPropertyName, subjectVariantCount);
		this.subjectVariantCount = subjectVariantCount;
	}

	/**
	 * {@link #hasMultipleBodies} accessor.
	 * @return	The value.
	 **/
	public Boolean getHasMultipleBodies() {
		return hasMultipleBodies;
	}

	/**
	 * {@link #hasMultipleBodies} mutator.
	 * @param hasMultipleBodies	The new value.
	 **/
	@XmlElement
	public void setHasMultipleBodies(Boolean hasMultipleBodies) {
		preset(hasMultipleBodiesPropertyName, hasMultipleBodies);
		this.hasMultipleBodies = hasMultipleBodies;
	}

	/**
	 * {@link #bodyVariantCount} accessor.
	 * @return	The value.
	 **/
	public Long getBodyVariantCount() {
		return bodyVariantCount;
	}

	/**
	 * {@link #bodyVariantCount} mutator.
	 * @param bodyVariantCount	The new value.
	 **/
	@XmlElement
	public void setBodyVariantCount(Long bodyVariantCount) {
		preset(bodyVariantCountPropertyName, bodyVariantCount);
		this.bodyVariantCount = bodyVariantCount;
	}
}
