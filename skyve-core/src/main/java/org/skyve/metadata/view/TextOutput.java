package org.skyve.metadata.view;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlType;

public interface TextOutput {
	Boolean getEscape();

	@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
	public static enum Sanitisation {
		// Note the values are ordered from least restrictive to most restrictive so we can compare the ordinals
		
		// No sanitisation applied
		none,
		// Formatting tags, structural tags, links and images and CSS too
		relaxed,
		// Formatting tags, structural tags, links and images
		simple,
		// Formatting tags only
		basic,
		// Only Text - no HTML
		text
	}
	
	Sanitisation getSanitise();
}
