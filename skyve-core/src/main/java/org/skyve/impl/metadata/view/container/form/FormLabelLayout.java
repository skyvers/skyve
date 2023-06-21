package org.skyve.impl.metadata.view.container.form;

import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;

/**
 * Represents a Side or Top layout.
 */
@XmlType(namespace = XMLMetaData.COMMON_NAMESPACE)
public enum FormLabelLayout {
	side,
	top
}
