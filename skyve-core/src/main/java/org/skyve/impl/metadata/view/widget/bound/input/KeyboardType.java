package org.skyve.impl.metadata.view.widget.bound.input;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlType;

/**
 * Enumerates soft-keyboard hints for mobile text-entry widgets.
 * 
 * Represents different keyboard modes on mobile devices.
 * These are not automatically assigned by Skyve view generation mainly because the numeric/decimal 
 * keyboards do not include a '-' button on their keyboard on some mobile devices (iOS) 
 * and there is no way to know whether the input should accept negative numbers in Skyve.
 *
 * <p>Skyve does not infer this automatically because some numeric layouts omit a
 * minus key on specific platforms, so the required input semantics cannot be
 * derived safely.
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@SuppressWarnings("java:S115") // Suppress "Constant names should comply with a naming convention" as these are not constants but enum values
public enum KeyboardType {
	numeric, // useful for positive whole numbers (as '-' is not always on the keyboard)
	tel, // useful for telephone numbers
	decimal, // usefu for positive decimal numbers (as '-' is not always on the keyboard)
	email, // useful for entering emails
	url, // useful for entering urls
	search // normal keyboard but with 'search' key
}
