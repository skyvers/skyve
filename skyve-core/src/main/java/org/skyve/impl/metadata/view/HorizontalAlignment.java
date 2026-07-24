package org.skyve.impl.metadata.view;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated enumeration of horizontal text/content alignment values
 * for view widgets.
 */
@XmlType(namespace = XMLMetaData.COMMON_NAMESPACE)
@SuppressWarnings("java:S115") // Suppress "Constant names should comply with a naming convention" as these are not constants but enum values
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
