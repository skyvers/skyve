package org.skyve.impl.metadata.view;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
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
