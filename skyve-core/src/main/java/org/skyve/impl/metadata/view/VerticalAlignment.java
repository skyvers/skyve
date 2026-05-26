package org.skyve.impl.metadata.view;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated enumeration of vertical content alignment values for view
 * widgets.
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
public enum VerticalAlignment {
	middle, top, bottom
}
