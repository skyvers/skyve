package org.skyve.impl.metadata.view.model.chart;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlRootElement(name = "top", namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
public class ChartBuilderTopMetaData extends ChartBuilderOrderMetaData {
	private static final long serialVersionUID = 5416228121746943426L;

	private int top;
	private boolean includeOthers;

	public int getTop() {
		return top;
	}
	@XmlAttribute(required = true)
	public void setTop(int top) {
		this.top = top;
	}

	public boolean isIncludeOthers() {
		return includeOthers;
	}
	@XmlAttribute(required = true)
	public void setIncludeOthers(boolean includeOthers) {
		this.includeOthers = includeOthers;
	}
}
