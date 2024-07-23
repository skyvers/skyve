package org.skyve.impl.metadata.model.document;

import java.util.ArrayList;
import java.util.List;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.SerializableMetaData;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.UniqueConstraint;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlElementWrapper;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;

@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name = "collection")
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE,
			name = "collection",
			propOrder = {"type", "ordered", "minCardinality", "maxCardinality", "ordering", "uniqueConstraints", "ownerDatabaseIndex", "elementDatabaseIndex", "cacheName"})
public class CollectionImpl extends ReferenceImpl implements Collection {
	private static final long serialVersionUID = 835190692384615766L;

	@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name = "ordering", propOrder = {"sort", "by"})
	public static final class OrderingImpl implements Ordering, SerializableMetaData {
		private static final long serialVersionUID = 5816585894418535717L;

		private String by;
		private SortDirection sort;

		public OrderingImpl() {
			// nothing to see here
		}
		
		public OrderingImpl(String by, SortDirection sort) {
			this.by = UtilImpl.processStringValue(by);
			this.sort = sort;
		}
		
		@Override
		public String getBy() {
			return by;
		}

		@XmlAttribute(required = true)
		public void setBy(String by) {
			this.by = UtilImpl.processStringValue(by);
		}

		@Override
		public SortDirection getSort() {
			return sort;
		}

		@XmlAttribute(required = true)
		public void setSort(SortDirection sort) {
			this.sort = sort;
		}
	}

	private CollectionType type;

	/**
	 * Indicates that the collection will be **lastly** orderd by the bizOrdinal attribute.
	 * Also allows grids to do DnD reordering and persists the bizOrdinal attribute in the ORM xml.
	 */
	private Boolean ordered;
	private Integer minCardinality;
	private Integer maxCardinality;
	private Boolean ownerDatabaseIndex;
	private Boolean elementDatabaseIndex;
	private String cacheName;
	private List<org.skyve.metadata.model.document.Collection.Ordering> ordering = new ArrayList<>();
	private List<UniqueConstraint> uniqueConstraints = new ArrayList<>();

	/**
	 * Populated by Document.convert(), this indicates that the order by statement contains at least 1 compound binding.
	 * This means that the framework must sort this collection once it is loaded, not in the SQL statement.
	 */
	private boolean complexOrdering = false;
	
	public CollectionImpl() {
		setAttributeType(AttributeType.collection);
	}

	@Override
	public Boolean getOrdered() {
		return ordered;
	}

	@XmlAttribute(required = false)
	public void setOrdered(Boolean ordered) {
		this.ordered = ordered;
	}

	@Override
	public CollectionType getType() {
		return type;
	}

	@XmlAttribute(required = true)
	public void setType(CollectionType type) {
		this.type = type;
	}

	@Override
	public Boolean getOwnerDatabaseIndex() {
		return ownerDatabaseIndex;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setOwnerDatabaseIndex(Boolean ownerDatabaseIndex) {
		this.ownerDatabaseIndex = ownerDatabaseIndex;
	}

	@Override
	public Boolean getElementDatabaseIndex() {
		return elementDatabaseIndex;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setElementDatabaseIndex(Boolean elementDatabaseIndex) {
		this.elementDatabaseIndex = elementDatabaseIndex;
	}

	@Override
	public String getCacheName() {
		return cacheName;
	}

	@XmlElement(name = "cache", namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setCacheName(String cacheName) {
		this.cacheName = cacheName;
	}

	@Override
	public Integer getMaxCardinality() {
		return maxCardinality;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setMaxCardinality(Integer maxCardinality) {
		this.maxCardinality = maxCardinality;
	}

	@Override
	public Integer getMinCardinality() {
		return minCardinality;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE, required = true)
	public void setMinCardinality(Integer minCardinality) {
		this.minCardinality = minCardinality;
	}

	@Override
	@XmlElementWrapper(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name = "ordering")
	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name = "order", type = OrderingImpl.class, required = false)
	public List<org.skyve.metadata.model.document.Collection.Ordering> getOrdering() {
		return ordering;
	}

	@Override
	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE, 
					name = "unique",
					type = UniqueConstraintImpl.class,
					required = false)
	public List<UniqueConstraint> getUniqueConstraints() {
		return uniqueConstraints;
	}

	@Override
	public boolean isRequired() {
		return ((minCardinality != null) && (minCardinality.intValue() > 0));
	}

	public boolean isComplexOrdering() {
		return complexOrdering;
	}

	@XmlTransient
	public void setComplexOrdering(boolean complexOrdering) {
		this.complexOrdering = complexOrdering;
	}
}
