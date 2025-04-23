package org.skyve.impl.metadata.view;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
public enum HorizontalAlignment {
	left("left", "start"), centre("center", "center"), right("right", "end");
	
	private String textAlignmentString;
	private String flexAlignmentString;
	
	private HorizontalAlignment(String textAlignmentString, String flexAlignmentString) {
		this.textAlignmentString = textAlignmentString;
		this.flexAlignmentString = flexAlignmentString;
	}
	
	public String toTextAlignmentString() {
		return this.textAlignmentString;
	}

	public String toFlexAlignmentString() {
		return this.flexAlignmentString;
	}
}
