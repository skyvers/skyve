package org.skyve.impl.metadata.view.model.chart;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.model.chart.TextLengthBucket;

@XmlRootElement(name = "textLengthBucket", namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
public class TextLengthBucketMetaData extends TextLengthBucket {
	private static final long serialVersionUID = 1471391824983371452L;

	public TextLengthBucketMetaData() {
		super();
	}
}
