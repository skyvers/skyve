package modules.admin.domain;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractTransientBean;

/**
 * Control Panel
 * 
 * @navhas n emailToContact 0..1 Contact
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
	public static final String xmlTracePropertyName = "xmlTrace";
	/** @hidden */
	public static final String httpTracePropertyName = "httpTrace";
	/** @hidden */
	public static final String queryTracePropertyName = "queryTrace";
	/** @hidden */
	public static final String commandTracePropertyName = "commandTrace";
	/** @hidden */
	public static final String facesTracePropertyName = "facesTrace";
	/** @hidden */
	public static final String contentTracePropertyName = "contentTrace";
	/** @hidden */
	public static final String securityTracePropertyName = "securityTrace";
	/** @hidden */
	public static final String bizletTracePropertyName = "bizletTrace";
	/** @hidden */
	public static final String dirtyTracePropertyName = "dirtyTrace";
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
	public static final String emailToContactPropertyName = "emailToContact";
	/** @hidden */
	public static final String emailSubjectPropertyName = "emailSubject";
	/** @hidden */
	public static final String emailContentPropertyName = "emailContent";
	/** @hidden */
	public static final String resultsPropertyName = "results";
	/** @hidden */
	public static final String tabIndexPropertyName = "tabIndex";

	/**
	 * XML
	 * <br/>
	 * Log XML metadata parse operations
	 **/
	private Boolean xmlTrace;
	/**
	 * HTTP
	 * <br/>
	 * Log request information including headers, parameters, cache state and timings.
	 **/
	private Boolean httpTrace;
	/**
	 * Query
	 * <br/>
	 * Log BizQL, Document Queries, Metadata Queries generated and executed during processing.
	 **/
	private Boolean queryTrace;
	/**
	 * Command
	 * <br/>
	 * Log command information such as filter criteria and paging row counts.
	 **/
	private Boolean commandTrace;
	/**
	 * Faces
	 * <br/>
	 * Log the faces phases and the xhtml generated.
	 **/
	private Boolean facesTrace;
	/**
	 * Content
	 * <br/>
	 * Log information on content fetched and stored.
	 **/
	private Boolean contentTrace;
	/**
	 * Security
	 * <br/>
	 * Log information on security denials.
	 **/
	private Boolean securityTrace;
	/**
	 * Bizlet
	 * <br/>
	 * Log every bizlet callback made (verbose).
	 **/
	private Boolean bizletTrace;
	/**
	 * Dirty
	 * <br/>
	 * Log the dirty state of domain objects (verbose).
	 **/
	private Boolean dirtyTrace;
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
	 * Email To Contact
	 **/
	private Contact emailToContact = null;
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
	/**
	 * TabIndex
	 * <br/>
	 * The index of the tab in the edit view.
			 	This is set to the results tab when there is results to display.
	 **/
	private Integer tabIndex;

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

	public static ControlPanel newInstance() {
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
	public boolean equals(Object o) {
		return ((o instanceof ControlPanel) && 
					this.getBizId().equals(((ControlPanel) o).getBizId()));
	}

	/**
	 * {@link #xmlTrace} accessor.
	 * @return	The value.
	 **/
	public Boolean getXmlTrace() {
		return xmlTrace;
	}

	/**
	 * {@link #xmlTrace} mutator.
	 * @param xmlTrace	The new value.
	 **/
	@XmlElement
	public void setXmlTrace(Boolean xmlTrace) {
		preset(xmlTracePropertyName, xmlTrace);
		this.xmlTrace = xmlTrace;
	}

	/**
	 * {@link #httpTrace} accessor.
	 * @return	The value.
	 **/
	public Boolean getHttpTrace() {
		return httpTrace;
	}

	/**
	 * {@link #httpTrace} mutator.
	 * @param httpTrace	The new value.
	 **/
	@XmlElement
	public void setHttpTrace(Boolean httpTrace) {
		preset(httpTracePropertyName, httpTrace);
		this.httpTrace = httpTrace;
	}

	/**
	 * {@link #queryTrace} accessor.
	 * @return	The value.
	 **/
	public Boolean getQueryTrace() {
		return queryTrace;
	}

	/**
	 * {@link #queryTrace} mutator.
	 * @param queryTrace	The new value.
	 **/
	@XmlElement
	public void setQueryTrace(Boolean queryTrace) {
		preset(queryTracePropertyName, queryTrace);
		this.queryTrace = queryTrace;
	}

	/**
	 * {@link #commandTrace} accessor.
	 * @return	The value.
	 **/
	public Boolean getCommandTrace() {
		return commandTrace;
	}

	/**
	 * {@link #commandTrace} mutator.
	 * @param commandTrace	The new value.
	 **/
	@XmlElement
	public void setCommandTrace(Boolean commandTrace) {
		preset(commandTracePropertyName, commandTrace);
		this.commandTrace = commandTrace;
	}

	/**
	 * {@link #facesTrace} accessor.
	 * @return	The value.
	 **/
	public Boolean getFacesTrace() {
		return facesTrace;
	}

	/**
	 * {@link #facesTrace} mutator.
	 * @param facesTrace	The new value.
	 **/
	@XmlElement
	public void setFacesTrace(Boolean facesTrace) {
		preset(facesTracePropertyName, facesTrace);
		this.facesTrace = facesTrace;
	}

	/**
	 * {@link #contentTrace} accessor.
	 * @return	The value.
	 **/
	public Boolean getContentTrace() {
		return contentTrace;
	}

	/**
	 * {@link #contentTrace} mutator.
	 * @param contentTrace	The new value.
	 **/
	@XmlElement
	public void setContentTrace(Boolean contentTrace) {
		preset(contentTracePropertyName, contentTrace);
		this.contentTrace = contentTrace;
	}

	/**
	 * {@link #securityTrace} accessor.
	 * @return	The value.
	 **/
	public Boolean getSecurityTrace() {
		return securityTrace;
	}

	/**
	 * {@link #securityTrace} mutator.
	 * @param securityTrace	The new value.
	 **/
	@XmlElement
	public void setSecurityTrace(Boolean securityTrace) {
		preset(securityTracePropertyName, securityTrace);
		this.securityTrace = securityTrace;
	}

	/**
	 * {@link #bizletTrace} accessor.
	 * @return	The value.
	 **/
	public Boolean getBizletTrace() {
		return bizletTrace;
	}

	/**
	 * {@link #bizletTrace} mutator.
	 * @param bizletTrace	The new value.
	 **/
	@XmlElement
	public void setBizletTrace(Boolean bizletTrace) {
		preset(bizletTracePropertyName, bizletTrace);
		this.bizletTrace = bizletTrace;
	}

	/**
	 * {@link #dirtyTrace} accessor.
	 * @return	The value.
	 **/
	public Boolean getDirtyTrace() {
		return dirtyTrace;
	}

	/**
	 * {@link #dirtyTrace} mutator.
	 * @param dirtyTrace	The new value.
	 **/
	@XmlElement
	public void setDirtyTrace(Boolean dirtyTrace) {
		preset(dirtyTracePropertyName, dirtyTrace);
		this.dirtyTrace = dirtyTrace;
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
	 * {@link #emailToContact} accessor.
	 * @return	The value.
	 **/
	public Contact getEmailToContact() {
		return emailToContact;
	}

	/**
	 * {@link #emailToContact} mutator.
	 * @param emailToContact	The new value.
	 **/
	@XmlElement
	public void setEmailToContact(Contact emailToContact) {
		preset(emailToContactPropertyName, emailToContact);
		this.emailToContact = emailToContact;
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

	/**
	 * {@link #tabIndex} accessor.
	 * @return	The value.
	 **/
	public Integer getTabIndex() {
		return tabIndex;
	}

	/**
	 * {@link #tabIndex} mutator.
	 * @param tabIndex	The new value.
	 **/
	@XmlElement
	public void setTabIndex(Integer tabIndex) {
		preset(tabIndexPropertyName, tabIndex);
		this.tabIndex = tabIndex;
	}
}
