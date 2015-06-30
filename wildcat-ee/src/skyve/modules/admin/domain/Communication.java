package modules.admin.domain;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import org.skyve.wildcat.domain.AbstractPersistentBean;

/**
 * Communication
 * 
 * @navhas n tag 1 Tag
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
	public static final String emailToPropertyName = "emailTo";
	/** @hidden */
	public static final String emailSubjectPropertyName = "emailSubject";
	/** @hidden */
	public static final String emailBodyPropertyName = "emailBody";
	/** @hidden */
	public static final String resultsPropertyName = "results";
	/** @hidden */
	public static final String attachmentPropertyName = "attachment";
	/** @hidden */
	public static final String filePathPropertyName = "filePath";

	private String description;
	private Tag tag = null;
	/**
	 * Bindings used in the email address, subject and body will be based on the selected module document.
	 **/
	private String moduleName;
	/**
	 * Bindings used in the email address, subject and body will be based on the selected module document.
	 **/
	private String documentName;
	/**
	 * Provide a binding which contains the email address to send to
	 **/
	private String toBinding;
	/**
	 * The email address to send to.  Bindings are allowed relative to the above module document.
	 **/
	private String emailTo;
	/**
	 * The subject of the email.  Bindings are allowed relative to the above module document.
	 **/
	private String emailSubject;
	/**
	 * The body of the email.  
			<p/>
			Bindings are allowed relative to the above module document.
			<p/>
			To include images in the HTML, switch to the Source view, and embed the 64bit encoding from a site like 
			http://www.freeformatter.com/base64-encoder.html
	 **/
	private String emailBody;
	private String results;
	private String attachment;
	private String filePath;

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
	 * {@link #emailTo} accessor.
	 **/
	public String getEmailTo() {
		return emailTo;
	}

	/**
	 * {@link #emailTo} mutator.
	 * 
	 * @param emailTo	The new value to set.
	 **/
	@XmlElement
	public void setEmailTo(String emailTo) {
		preset(emailToPropertyName, emailTo);
		this.emailTo = emailTo;
	}

	/**
	 * {@link #emailSubject} accessor.
	 **/
	public String getEmailSubject() {
		return emailSubject;
	}

	/**
	 * {@link #emailSubject} mutator.
	 * 
	 * @param emailSubject	The new value to set.
	 **/
	@XmlElement
	public void setEmailSubject(String emailSubject) {
		preset(emailSubjectPropertyName, emailSubject);
		this.emailSubject = emailSubject;
	}

	/**
	 * {@link #emailBody} accessor.
	 **/
	public String getEmailBody() {
		return emailBody;
	}

	/**
	 * {@link #emailBody} mutator.
	 * 
	 * @param emailBody	The new value to set.
	 **/
	@XmlElement
	public void setEmailBody(String emailBody) {
		preset(emailBodyPropertyName, emailBody);
		this.emailBody = emailBody;
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
	 * {@link #attachment} accessor.
	 **/
	public String getAttachment() {
		return attachment;
	}

	/**
	 * {@link #attachment} mutator.
	 * 
	 * @param attachment	The new value to set.
	 **/
	@XmlElement
	public void setAttachment(String attachment) {
		preset(attachmentPropertyName, attachment);
		this.attachment = attachment;
	}

	/**
	 * {@link #filePath} accessor.
	 **/
	public String getFilePath() {
		return filePath;
	}

	/**
	 * {@link #filePath} mutator.
	 * 
	 * @param filePath	The new value to set.
	 **/
	@XmlElement
	public void setFilePath(String filePath) {
		preset(filePathPropertyName, filePath);
		this.filePath = filePath;
	}
}
