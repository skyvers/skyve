package modules.admin.domain;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractPersistentBean;

/**
 * Dynamic Relation
 * 
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
public class DynamicRelation extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "DynamicRelation";

	/** @hidden */
	public static final String ownerIdPropertyName = "ownerId";

	/** @hidden */
	public static final String relatedIdPropertyName = "relatedId";

	/** @hidden */
	public static final String attributeNamePropertyName = "attributeName";

	/**
	 * Owner ID
	 **/
	private String ownerId;

	/**
	 * Related ID
	 **/
	private String relatedId;

	/**
	 * Attribute Name
	 **/
	private String attributeName;

	@Override
	@XmlTransient
	public String getBizModule() {
		return DynamicRelation.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return DynamicRelation.DOCUMENT_NAME;
	}

	public static DynamicRelation newInstance() {
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
			return org.skyve.util.Binder.formatMessage("{ownerId}.{relatedId}", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof DynamicRelation) && 
					this.getBizId().equals(((DynamicRelation) o).getBizId()));
	}

	/**
	 * {@link #ownerId} accessor.
	 * @return	The value.
	 **/
	public String getOwnerId() {
		return ownerId;
	}

	/**
	 * {@link #ownerId} mutator.
	 * @param ownerId	The new value.
	 **/
	@XmlElement
	public void setOwnerId(String ownerId) {
		preset(ownerIdPropertyName, ownerId);
		this.ownerId = ownerId;
	}

	/**
	 * {@link #relatedId} accessor.
	 * @return	The value.
	 **/
	public String getRelatedId() {
		return relatedId;
	}

	/**
	 * {@link #relatedId} mutator.
	 * @param relatedId	The new value.
	 **/
	@XmlElement
	public void setRelatedId(String relatedId) {
		preset(relatedIdPropertyName, relatedId);
		this.relatedId = relatedId;
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
}
