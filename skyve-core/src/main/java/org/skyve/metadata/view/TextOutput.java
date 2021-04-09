package org.skyve.metadata.view;

import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;

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
		basic,
		// Formatting tags only
		simple,
		// Only Text - no HTML
		text
	}
	
	Sanitisation getSanitise();
}
