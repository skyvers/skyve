package org.skyve.impl.metadata.view.model.chart;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.SerializableMetaData;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.view.model.chart.OrderBy;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlRootElement(name = "order", namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
public class ChartBuilderOrderMetaData implements SerializableMetaData {
	private static final long serialVersionUID = -1811520217428427701L;
	
	private OrderBy by;
	private SortDirection sort;

	public OrderBy getBy() {
		return by;
	}
	@XmlAttribute(required = true)
	public void setBy(OrderBy by) {
		this.by = by;
	}
	
	public SortDirection getSort() {
		return sort;
	}
	@XmlAttribute(required = true)
	public void setSort(SortDirection sort) {
		this.sort = sort;
	}
}
