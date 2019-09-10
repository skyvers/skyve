package org.skyve.impl.metadata.view.model.chart;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;

@XmlRootElement(name = "top", namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
public class ChartBuilderTopMetaData extends ChartBuilderOrderMetaData {
	private static final long serialVersionUID = 5416228121746943426L;

	private int top;

	public int getTop() {
		return top;
	}
	@XmlAttribute(required = true)
	public void setTop(int top) {
		this.top = top;
	}
}
