package org.skyve.impl.metadata;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.Ordering;
import org.skyve.metadata.SortDirection;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name = "ordering", propOrder = {"sort", "by"})
public class OrderingImpl implements Ordering {
	private static final long serialVersionUID = 5816585894418535717L;

	@XmlAttribute(required = true)
	private String by;
	@XmlAttribute(required = true)
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

	public void setBy(String by) {
		this.by = UtilImpl.processStringValue(by);
	}

	@Override
	public SortDirection getSort() {
		return sort;
	}

	public void setSort(SortDirection sort) {
		this.sort = sort;
	}
}
