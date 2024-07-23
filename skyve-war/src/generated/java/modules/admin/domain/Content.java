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
import org.skyve.impl.domain.AbstractTransientBean;
import org.skyve.impl.domain.types.jaxb.TimestampMapper;

/**
 * Content
 * 
 * @stereotype "transient"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
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

	/**
	 * Content Id
	 **/
	private String contentId;

	/**
	 * Customer Name
	 **/
	private String customerName;

	/**
	 * Module Name
	 **/
	private String moduleName;

	/**
	 * Document Name
	 **/
	private String documentName;

	/**
	 * Content Biz Id
	 **/
	private String contentBizId;

	/**
	 * Attribute Name
	 **/
	private String attributeName;

	/**
	 * Last Modified
	 **/
	private Timestamp lastModified;

	/**
	 * Content
	 **/
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

	public static Content newInstance() {
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
		return toString();

	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof Content) && 
					this.getBizId().equals(((Content) o).getBizId()));
	}

	/**
	 * {@link #contentId} accessor.
	 * @return	The value.
	 **/
	public String getContentId() {
		return contentId;
	}

	/**
	 * {@link #contentId} mutator.
	 * @param contentId	The new value.
	 **/
	@XmlElement
	public void setContentId(String contentId) {
		preset(contentIdPropertyName, contentId);
		this.contentId = contentId;
	}

	/**
	 * {@link #customerName} accessor.
	 * @return	The value.
	 **/
	public String getCustomerName() {
		return customerName;
	}

	/**
	 * {@link #customerName} mutator.
	 * @param customerName	The new value.
	 **/
	@XmlElement
	public void setCustomerName(String customerName) {
		preset(customerNamePropertyName, customerName);
		this.customerName = customerName;
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
	 * {@link #contentBizId} accessor.
	 * @return	The value.
	 **/
	public String getContentBizId() {
		return contentBizId;
	}

	/**
	 * {@link #contentBizId} mutator.
	 * @param contentBizId	The new value.
	 **/
	@XmlElement
	public void setContentBizId(String contentBizId) {
		preset(contentBizIdPropertyName, contentBizId);
		this.contentBizId = contentBizId;
	}

	/**
	 * {@link #attributeName} accessor.
	 * @return	The value.
	 **/
	public String getAttributeName() {
		return attributeName;
	}

	/**
	 * {@link #attributeName} mutator.
	 * @param attributeName	The new value.
	 **/
	@XmlElement
	public void setAttributeName(String attributeName) {
		preset(attributeNamePropertyName, attributeName);
		this.attributeName = attributeName;
	}

	/**
	 * {@link #lastModified} accessor.
	 * @return	The value.
	 **/
	public Timestamp getLastModified() {
		return lastModified;
	}

	/**
	 * {@link #lastModified} mutator.
	 * @param lastModified	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	public void setLastModified(Timestamp lastModified) {
		preset(lastModifiedPropertyName, lastModified);
		this.lastModified = lastModified;
	}

	/**
	 * {@link #content} accessor.
	 * @return	The value.
	 **/
	public String getContent() {
		return content;
	}

	/**
	 * {@link #content} mutator.
	 * @param content	The new value.
	 **/
	@XmlElement
	public void setContent(String content) {
		preset(contentPropertyName, content);
		this.content = content;
	}
}
