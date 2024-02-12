package org.skyve.impl.metadata.view.model.chart;

import java.util.ArrayList;
import java.util.List;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.model.chart.NumericRangeBucket;

import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlRootElement(name = "numericRangeBucket", namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
public class NumericRangeBucketMetaData extends NumericRangeBucket {
	private static final long serialVersionUID = -7407592073904520692L;

	@XmlElement(name="range", namespace = XMLMetaData.VIEW_NAMESPACE)
	private List<NumericRangeMetaData> rangeList = new ArrayList<>();

	public NumericRangeBucketMetaData() {
		super();
	}

	public List<NumericRangeMetaData> getRanges() {
		return rangeList;
	}
	
	void convert() {
		int length = rangeList.size();
		ranges = new int[length];
		for (int i = 0; i < length; i++) {
			ranges[i] = rangeList.get(i).getRange();
		}
	}
}
