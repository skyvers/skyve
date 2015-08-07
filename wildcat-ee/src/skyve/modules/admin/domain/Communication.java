package modules.admin.domain;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.domain.types.Enumeration;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.wildcat.domain.AbstractPersistentBean;

/**
 * Communication
 * 
 * @depend - - - ActionType
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
	public static final String subjectPropertyName = "subject";
	/** @hidden */
	public static final String bodyPropertyName = "body";
	/** @hidden */
	public static final String resultsPropertyName = "results";
	/** @hidden */
	public static final String attachmentPropertyName = "attachment";
	/** @hidden */
	public static final String actionTypePropertyName = "actionType";
	/** @hidden */
	public static final String filePathPropertyName = "filePath";

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
	private String attachment;
	private ActionType actionType;
	/**
	 * The path (local to the server) where bulk files will be created.
	 **/
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
		preset(actionTypePropertyName, actionType);
		this.actionType = actionType;
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

	@XmlTransient
	public boolean isSaveAction() {
		return (ActionType.saveForBulkSend.equals(this.getActionType()));
	}

	public boolean isNotSaveAction() {
		return (! isSaveAction());
	}
}
