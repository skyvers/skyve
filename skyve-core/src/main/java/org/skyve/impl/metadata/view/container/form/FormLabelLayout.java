package org.skyve.impl.metadata.view.container.form;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlType;

/**
 * Enumerates label placement strategies for form layouts.
 * Represents a Side or Top layout.
 *
 * <p>{@link #side} renders labels beside field content, while {@link #top}
 * renders labels above field content.
 */
@XmlType(namespace = XMLMetaData.COMMON_NAMESPACE)
@SuppressWarnings("java:S115") // Suppress "Constant names should comply with a naming convention" as these are not constants but enum values
public enum FormLabelLayout {
	side,
	top
}
