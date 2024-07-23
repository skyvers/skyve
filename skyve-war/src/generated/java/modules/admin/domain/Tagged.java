package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import modules.admin.Tag.TagExtension;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractPersistentBean;

/**
 * Tagged
 * 
 * @navhas n tag 1 Tag
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class Tagged extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "Tagged";

	/** @hidden */
	public static final String taggedModulePropertyName = "taggedModule";

	/** @hidden */
	public static final String taggedDocumentPropertyName = "taggedDocument";

	/** @hidden */
	public static final String taggedBizIdPropertyName = "taggedBizId";

	/** @hidden */
	public static final String tagPropertyName = "tag";

	/**
	 * Tagged Module
	 **/
	private String taggedModule;

	/**
	 * Tagged Document
	 **/
	private String taggedDocument;

	/**
	 * Tagged BizId
	 **/
	private String taggedBizId;

	/**
	 * Tag
	 * <br/>
	 * The tag
	 **/
	private TagExtension tag = null;

	@Override
	@XmlTransient
	public String getBizModule() {
		return Tagged.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return Tagged.DOCUMENT_NAME;
	}

	public static Tagged newInstance() {
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
			return org.skyve.util.Binder.formatMessage("Tagged Item", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof Tagged) && 
					this.getBizId().equals(((Tagged) o).getBizId()));
	}

	/**
	 * {@link #taggedModule} accessor.
	 * @return	The value.
	 **/
	public String getTaggedModule() {
		return taggedModule;
	}

	/**
	 * {@link #taggedModule} mutator.
	 * @param taggedModule	The new value.
	 **/
	@XmlElement
	public void setTaggedModule(String taggedModule) {
		preset(taggedModulePropertyName, taggedModule);
		this.taggedModule = taggedModule;
	}

	/**
	 * {@link #taggedDocument} accessor.
	 * @return	The value.
	 **/
	public String getTaggedDocument() {
		return taggedDocument;
	}

	/**
	 * {@link #taggedDocument} mutator.
	 * @param taggedDocument	The new value.
	 **/
	@XmlElement
	public void setTaggedDocument(String taggedDocument) {
		preset(taggedDocumentPropertyName, taggedDocument);
		this.taggedDocument = taggedDocument;
	}

	/**
	 * {@link #taggedBizId} accessor.
	 * @return	The value.
	 **/
	public String getTaggedBizId() {
		return taggedBizId;
	}

	/**
	 * {@link #taggedBizId} mutator.
	 * @param taggedBizId	The new value.
	 **/
	@XmlElement
	public void setTaggedBizId(String taggedBizId) {
		preset(taggedBizIdPropertyName, taggedBizId);
		this.taggedBizId = taggedBizId;
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
			preset(tagPropertyName, tag);
			this.tag = tag;
		}
	}
}
