package org.skyve.impl.metadata.view.model.chart;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.model.chart.NumericMultipleBucket;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB descriptor for numeric-multiple bucketing.
 *
 * <p>Groups numeric values into buckets whose boundaries are a fixed multiple
 * of the configured unit size.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 */
@XmlRootElement(name = "numericMultipleBucket", namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
public class NumericMultipleBucketMetaData extends NumericMultipleBucket {
	private static final long serialVersionUID = 3136709693846883511L;

	public NumericMultipleBucketMetaData() {
		super(0);
	}

	public int getMultiple() {
		return multiple;
	}

	@XmlAttribute(required = true)
	public void setMultiple(int multiple) {
		this.multiple = multiple;
	}
}
