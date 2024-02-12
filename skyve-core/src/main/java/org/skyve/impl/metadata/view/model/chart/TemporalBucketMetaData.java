package org.skyve.impl.metadata.view.model.chart;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.model.chart.TemporalBucket;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlRootElement(name = "temporalBucket", namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
public class TemporalBucketMetaData extends TemporalBucket {
	private static final long serialVersionUID = -8756158527886460010L;

	public TemporalBucketMetaData() {
		super(TemporalBucketType.day);
	}
	
	public TemporalBucketType getType() {
		return type;
	}
	
	@XmlAttribute(required = true)
	public void setType(TemporalBucketType type) {
		this.type = type;
	}
}
