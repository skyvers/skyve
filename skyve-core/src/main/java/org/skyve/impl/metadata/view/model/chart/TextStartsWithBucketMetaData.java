package org.skyve.impl.metadata.view.model.chart;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.model.chart.TextStartsWithBucket;

@XmlRootElement(name = "textStartsWithBucket", namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
public class TextStartsWithBucketMetaData extends TextStartsWithBucket {
	private static final long serialVersionUID = -307976219288558204L;

	public TextStartsWithBucketMetaData() {
		super(0, false);
	}

	public int getLenth() {
		return length;
	}

	@XmlAttribute(required = true)
	public void setLength(int length) {
		this.length = length;
	}
	
	public boolean getCaseSensitive() {
		return caseSensitive;
	}
	
	@XmlAttribute(required = true)
	public void setCaseSensitive(boolean caseSensitive) {
		this.caseSensitive = caseSensitive;
	}
}
