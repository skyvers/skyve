package org.skyve.impl.metadata.view.container.form;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlType;

/**
 * Represents a Side or Top layout.
 */
@XmlType(namespace = XMLMetaData.COMMON_NAMESPACE)
public enum FormLabelLayout {
	side,
	top
}
