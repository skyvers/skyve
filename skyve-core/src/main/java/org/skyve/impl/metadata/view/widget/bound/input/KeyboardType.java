package org.skyve.impl.metadata.view.widget.bound.input;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlType;

/**
 * Represents different keyboard modes on mobile devices.
 * These are not automatically assigned by Skyve view generation mainly because the numeric/decimal 
 * keyboards do not include a '-' button on their keyboard on some mobile devices (iOS) 
 * and there is no way to know whether the input should accept negative numbers in Skyve.
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
public enum KeyboardType {
	numeric, // useful for positive whole numbers (as '-' is not always on the keyboard)
	tel, // useful for telephone numbers
	decimal, // usefu for positive decimal numbers (as '-' is not always on the keyboard)
	email, // useful for entering emails
	url, // useful for entering urls
	search // normal keyboard but with 'search' key
}
