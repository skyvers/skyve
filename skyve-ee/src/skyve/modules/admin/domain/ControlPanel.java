package modules.admin.domain;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.impl.domain.AbstractTransientBean;

/**
 * Control Panel
 * 
 * @stereotype "transient"
 */
@XmlType
@XmlRootElement
public class ControlPanel extends AbstractTransientBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";
	/** @hidden */
	public static final String DOCUMENT_NAME = "ControlPanel";

	/** @hidden */
	public static final String designModuleDocumentNamePropertyName = "designModuleDocumentName";
	/** @hidden */
	public static final String queryPropertyName = "query";
	/** @hidden */
	public static final String customerNameToSwapToPropertyName = "customerNameToSwapTo";
	/** @hidden */
	public static final String emailFromPropertyName = "emailFrom";
	/** @hidden */
	public static final String emailToPropertyName = "emailTo";
	/** @hidden */
	public static final String emailSubjectPropertyName = "emailSubject";
	/** @hidden */
	public static final String emailContentPropertyName = "emailContent";
	/** @hidden */
	public static final String resultsPropertyName = "results";

	/**
	 * Module.Document Name
	 **/
	private String designModuleDocumentName;
	/**
	 * BizQL
	 **/
	private String query;
	/**
	 * Customer Name To Swap To
	 **/
	private String customerNameToSwapTo;
	/**
	 * Email From
	 **/
	private String emailFrom;
	/**
	 * Email To
	 **/
	private String emailTo;
	/**
	 * Email Subject
	 **/
	private String emailSubject;
	/**
	 * Email
	 **/
	private String emailContent;
	/**
	 * Results
	 **/
	private String results;

	@Override
	@XmlTransient
	public String getBizModule() {
		return ControlPanel.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return ControlPanel.DOCUMENT_NAME;
	}

	public static ControlPanel newInstance() throws Exception {
		return CORE.getUser().getCustomer().getModule(MODULE_NAME).getDocument(CORE.getUser().getCustomer(), DOCUMENT_NAME).newInstance(CORE.getUser());
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof ControlPanel) && 
					this.getBizId().equals(((ControlPanel) o).getBizId()));
	}

	/**
	 * {@link #designModuleDocumentName} accessor.
	 * @return	The value.
	 **/
	public String getDesignModuleDocumentName() {
		return designModuleDocumentName;
	}

	/**
	 * {@link #designModuleDocumentName} mutator.
	 * @param designModuleDocumentName	The new value.
	 **/
	@XmlElement
	public void setDesignModuleDocumentName(String designModuleDocumentName) {
		preset(designModuleDocumentNamePropertyName, designModuleDocumentName);
		this.designModuleDocumentName = designModuleDocumentName;
	}

	/**
	 * {@link #query} accessor.
	 * @return	The value.
	 **/
	public String getQuery() {
		return query;
	}

	/**
	 * {@link #query} mutator.
	 * @param query	The new value.
	 **/
	@XmlElement
	public void setQuery(String query) {
		preset(queryPropertyName, query);
		this.query = query;
	}

	/**
	 * {@link #customerNameToSwapTo} accessor.
	 * @return	The value.
	 **/
	public String getCustomerNameToSwapTo() {
		return customerNameToSwapTo;
	}

	/**
	 * {@link #customerNameToSwapTo} mutator.
	 * @param customerNameToSwapTo	The new value.
	 **/
	@XmlElement
	public void setCustomerNameToSwapTo(String customerNameToSwapTo) {
		preset(customerNameToSwapToPropertyName, customerNameToSwapTo);
		this.customerNameToSwapTo = customerNameToSwapTo;
	}

	/**
	 * {@link #emailFrom} accessor.
	 * @return	The value.
	 **/
	public String getEmailFrom() {
		return emailFrom;
	}

	/**
	 * {@link #emailFrom} mutator.
	 * @param emailFrom	The new value.
	 **/
	@XmlElement
	public void setEmailFrom(String emailFrom) {
		preset(emailFromPropertyName, emailFrom);
		this.emailFrom = emailFrom;
	}

	/**
	 * {@link #emailTo} accessor.
	 * @return	The value.
	 **/
	public String getEmailTo() {
		return emailTo;
	}

	/**
	 * {@link #emailTo} mutator.
	 * @param emailTo	The new value.
	 **/
	@XmlElement
	public void setEmailTo(String emailTo) {
		preset(emailToPropertyName, emailTo);
		this.emailTo = emailTo;
	}

	/**
	 * {@link #emailSubject} accessor.
	 * @return	The value.
	 **/
	public String getEmailSubject() {
		return emailSubject;
	}

	/**
	 * {@link #emailSubject} mutator.
	 * @param emailSubject	The new value.
	 **/
	@XmlElement
	public void setEmailSubject(String emailSubject) {
		preset(emailSubjectPropertyName, emailSubject);
		this.emailSubject = emailSubject;
	}

	/**
	 * {@link #emailContent} accessor.
	 * @return	The value.
	 **/
	public String getEmailContent() {
		return emailContent;
	}

	/**
	 * {@link #emailContent} mutator.
	 * @param emailContent	The new value.
	 **/
	@XmlElement
	public void setEmailContent(String emailContent) {
		preset(emailContentPropertyName, emailContent);
		this.emailContent = emailContent;
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
}
