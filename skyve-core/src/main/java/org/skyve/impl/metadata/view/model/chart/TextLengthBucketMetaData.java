package org.skyve.impl.metadata.view.model.chart;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.model.chart.TextLengthBucket;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlRootElement(name = "textLengthBucket", namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
public class TextLengthBucketMetaData extends TextLengthBucket {
	private static final long serialVersionUID = 1471391824983371452L;

	public TextLengthBucketMetaData() {
		super();
	}
}
