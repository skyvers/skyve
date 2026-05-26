package org.skyve.impl.metadata.view.container.form;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlType;

/**
 * Represents a Side or Top layout.
 */
@XmlType(namespace = XMLMetaData.COMMON_NAMESPACE)
@SuppressWarnings("java:S115") // Suppress "Constant names should comply with a naming convention" as these are not constants but enum values
public enum FormLabelLayout {
	side,
	top
}
