package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractPersistentBean;

/**
 * Dynamic Relation
 * <br/>
 * Represents a relation - ie a database joining table or foereign key (association or collection).
			There can be multiple rows with the same attribute name for multiple cardinality relations.
			The Dynamic Relations are upserted with the same bizId as its static counterpart when there is a mix.
			So this is manipulated outside of Hibernate O/R mapping.
 * 
 * @stereotype "persistent child"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public abstract class DynamicRelation extends AbstractPersistentBean implements ChildBean<DynamicEntity> {
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
	public static final String relatedModuleNamePropertyName = "relatedModuleName";

	/** @hidden */
	public static final String relatedDocumentNamePropertyName = "relatedDocumentName";

	/** @hidden */
	public static final String relatedIdPropertyName = "relatedId";

	/** @hidden */
	public static final String attributeNamePropertyName = "attributeName";

	/** @hidden */
	public static final String ordinalPropertyName = "ordinal";

	/**
	 * Module Name
	 * <br/>
	 * Populated with the related document's module name if the dynamic relation is to a static document.
	 **/
	private String relatedModuleName;

	/**
	 * Document Name
	 * <br/>
	 * Populated with the related document's name if the dynamic relation is to a static document.
	 **/
	private String relatedDocumentName;

	/**
	 * Related ID
	 * <br/>
	 * The ID for the related dynamic entity (admin.DynamicEntity) OR a real entity.
	 * <br/>
	 * This is an ID because it may be related to a static entity.
					It also makes schema evolution easier as deletes will remove everything related.
	 **/
	private String relatedId;

	/**
	 * Attribute Name
	 * <br/>
	 * The attribute name of this relation.
	 **/
	private String attributeName;

	/**
	 * Ordinal
	 * <br/>
	 * Used for ordered collections (akin to bizOrdinal)
	 **/
	private Integer ordinal;

	private DynamicEntity parent;

	private Integer bizOrdinal;

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
			return org.skyve.util.Binder.formatMessage("{parent.bizModule}.{parent.bizDocument}.{attributeName}#{parent.bizId}->{relatedId}", this);
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
	 * {@link #relatedModuleName} accessor.
	 * @return	The value.
	 **/
	public String getRelatedModuleName() {
		return relatedModuleName;
	}

	/**
	 * {@link #relatedModuleName} mutator.
	 * @param relatedModuleName	The new value.
	 **/
	@XmlElement
	public void setRelatedModuleName(String relatedModuleName) {
		preset(relatedModuleNamePropertyName, relatedModuleName);
		this.relatedModuleName = relatedModuleName;
	}

	/**
	 * {@link #relatedDocumentName} accessor.
	 * @return	The value.
	 **/
	public String getRelatedDocumentName() {
		return relatedDocumentName;
	}

	/**
	 * {@link #relatedDocumentName} mutator.
	 * @param relatedDocumentName	The new value.
	 **/
	@XmlElement
	public void setRelatedDocumentName(String relatedDocumentName) {
		preset(relatedDocumentNamePropertyName, relatedDocumentName);
		this.relatedDocumentName = relatedDocumentName;
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

	/**
	 * {@link #ordinal} accessor.
	 * @return	The value.
	 **/
	public Integer getOrdinal() {
		return ordinal;
	}

	/**
	 * {@link #ordinal} mutator.
	 * @param ordinal	The new value.
	 **/
	@XmlElement
	public void setOrdinal(Integer ordinal) {
		preset(ordinalPropertyName, ordinal);
		this.ordinal = ordinal;
	}

	@Override
	public DynamicEntity getParent() {
		return parent;
	}

	@Override
	@XmlElement
	public void setParent(DynamicEntity parent) {
		if (this.parent != parent) {
			preset(ChildBean.PARENT_NAME, parent);
			this.parent = parent;
		}
	}

	@Override
	public Integer getBizOrdinal() {
		return bizOrdinal;
	}

	@Override
	@XmlElement
	public void setBizOrdinal(Integer bizOrdinal) {
		preset(Bean.ORDINAL_NAME, bizOrdinal);
		this.bizOrdinal =  bizOrdinal;
	}
}
