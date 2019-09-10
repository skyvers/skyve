package org.skyve.impl.metadata.view.model.chart;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.model.chart.NumericRangeBucket;

@XmlRootElement(name = "range", namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
public class NumericRangeMetaData extends NumericRangeBucket {
	private static final long serialVersionUID = -7407592073904520692L;

	private int range;

	public int getRange() {
		return range;
	}
	@XmlValue
	public void setRange(int range) {
		this.range = range;
	}
}
