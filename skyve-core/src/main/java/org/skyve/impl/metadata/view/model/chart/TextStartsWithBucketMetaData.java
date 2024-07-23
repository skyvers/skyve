package org.skyve.impl.metadata.view.model.chart;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.model.chart.TextStartsWithBucket;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlRootElement(name = "textStartsWithBucket", namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
public class TextStartsWithBucketMetaData extends TextStartsWithBucket {
	private static final long serialVersionUID = -307976219288558204L;

	public TextStartsWithBucketMetaData() {
		super(0, false);
	}

	public int getLength() {
		return length;
	}

	@XmlAttribute(required = true)
	public void setLength(int length) {
		this.length = length;
	}
	
	public boolean isCaseSensitive() {
		return caseSensitive;
	}
	
	@XmlAttribute(required = true)
	public void setCaseSensitive(boolean caseSensitive) {
		this.caseSensitive = caseSensitive;
	}
}
