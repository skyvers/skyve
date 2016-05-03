package modules.admin.domain;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import org.skyve.CORE;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.domain.AbstractTransientBean;
import org.skyve.impl.domain.types.jaxb.TimestampMapper;

/**
 * Content
 * 
 * @stereotype "transient"
 */
@XmlType
@XmlRootElement
public class Content extends AbstractTransientBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";
	/** @hidden */
	public static final String DOCUMENT_NAME = "Content";

	/** @hidden */
	public static final String contentIdPropertyName = "contentId";
	/** @hidden */
	public static final String customerNamePropertyName = "customerName";
	/** @hidden */
	public static final String moduleNamePropertyName = "moduleName";
	/** @hidden */
	public static final String documentNamePropertyName = "documentName";
	/** @hidden */
	public static final String contentBizIdPropertyName = "contentBizId";
	/** @hidden */
	public static final String attributeNamePropertyName = "attributeName";
	/** @hidden */
	public static final String lastModifiedPropertyName = "lastModified";
	/** @hidden */
	public static final String contentPropertyName = "content";

	private String contentId;
	private String customerName;
	private String moduleName;
	private String documentName;
	private String contentBizId;
	private String attributeName;
	private Timestamp lastModified;
	private String content;

	@Override
	@XmlTransient
	public String getBizModule() {
		return Content.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return Content.DOCUMENT_NAME;
	}

	public static Content newInstance() throws Exception {
		return CORE.getUser().getCustomer().getModule(MODULE_NAME).getDocument(CORE.getUser().getCustomer(), DOCUMENT_NAME).newInstance(CORE.getUser());
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof Content) && 
					this.getBizId().equals(((Content) o).getBizId()));
	}

	/**
	 * {@link #contentId} accessor.
	 **/
	public String getContentId() {
		return contentId;
	}

	/**
	 * {@link #contentId} mutator.
	 * 
	 * @param contentId	The new value to set.
	 **/
	@XmlElement
	public void setContentId(String contentId) {
		preset(contentIdPropertyName, contentId);
		this.contentId = contentId;
	}

	/**
	 * {@link #customerName} accessor.
	 **/
	public String getCustomerName() {
		return customerName;
	}

	/**
	 * {@link #customerName} mutator.
	 * 
	 * @param customerName	The new value to set.
	 **/
	@XmlElement
	public void setCustomerName(String customerName) {
		preset(customerNamePropertyName, customerName);
		this.customerName = customerName;
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
	 * {@link #contentBizId} accessor.
	 **/
	public String getContentBizId() {
		return contentBizId;
	}

	/**
	 * {@link #contentBizId} mutator.
	 * 
	 * @param contentBizId	The new value to set.
	 **/
	@XmlElement
	public void setContentBizId(String contentBizId) {
		preset(contentBizIdPropertyName, contentBizId);
		this.contentBizId = contentBizId;
	}

	/**
	 * {@link #attributeName} accessor.
	 **/
	public String getAttributeName() {
		return attributeName;
	}

	/**
	 * {@link #attributeName} mutator.
	 * 
	 * @param attributeName	The new value to set.
	 **/
	@XmlElement
	public void setAttributeName(String attributeName) {
		preset(attributeNamePropertyName, attributeName);
		this.attributeName = attributeName;
	}

	/**
	 * {@link #lastModified} accessor.
	 **/
	public Timestamp getLastModified() {
		return lastModified;
	}

	/**
	 * {@link #lastModified} mutator.
	 * 
	 * @param lastModified	The new value to set.
	 **/
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	@XmlElement
	public void setLastModified(Timestamp lastModified) {
		preset(lastModifiedPropertyName, lastModified);
		this.lastModified = lastModified;
	}

	/**
	 * {@link #content} accessor.
	 **/
	public String getContent() {
		return content;
	}

	/**
	 * {@link #content} mutator.
	 * 
	 * @param content	The new value to set.
	 **/
	@XmlElement
	public void setContent(String content) {
		preset(contentPropertyName, content);
		this.content = content;
	}
}
