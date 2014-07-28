package org.skyve.wildcat.metadata.view;

import javax.xml.bind.annotation.XmlType;

import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.VIEW_NAMESPACE)
public enum HorizontalAlignment {
	left("left"), centre("center"), right("right");
	
	private String alignmentString;
	
	private HorizontalAlignment(String alignmentString) {
		this.alignmentString = alignmentString;
	}
	
	public String toAlignmentString() {
		return this.alignmentString;
	}
}
