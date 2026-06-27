package org.skyve.impl.metadata;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.Ordering;
import org.skyve.metadata.SortDirection;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Stores a document ordering definition for JAXB-backed metadata.
 *
 * <p>Instances carry the sort field and direction used by repository and query metadata
 * to serialise ordering rules.
 */
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name = "ordering", propOrder = {"sort", "by"})
public class OrderingImpl implements Ordering {
	private static final long serialVersionUID = 5816585894418535717L;

	@XmlAttribute(required = true)
	private String by;
	@XmlAttribute(required = true)
	private SortDirection sort;

	/**
	 * Creates an empty ordering definition for JAXB.
	 */
	public OrderingImpl() {
		// nothing to see here
	}
	
	/**
	 * Creates an ordering definition from a field name and direction.
	 *
	 * @param by the field or binding to order by
	 * @param sort the direction to apply
	 */
	public OrderingImpl(String by, SortDirection sort) {
		this.by = UtilImpl.processStringValue(by);
		this.sort = sort;
	}
	
	/**
	 * Returns the ordered field name.
	 *
	 * @return the field or binding name
	 */
	@Override
	public String getBy() {
		return by;
	}

	/**
	 * Sets the ordered field name.
	 *
	 * @param by the field or binding name
	 */
	public void setBy(String by) {
		this.by = UtilImpl.processStringValue(by);
	}

	/**
	 * Returns the sort direction.
	 *
	 * @return the configured sort direction
	 */
	@Override
	public SortDirection getSort() {
		return sort;
	}

	/**
	 * Sets the sort direction.
	 *
	 * @param sort the direction to apply
	 */
	public void setSort(SortDirection sort) {
		this.sort = sort;
	}
}
