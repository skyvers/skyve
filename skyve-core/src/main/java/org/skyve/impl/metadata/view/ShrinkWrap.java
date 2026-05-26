package org.skyve.impl.metadata.view;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated enumeration of shrink-wrap layout modes.
 *
 * <p>Determines whether a widget auto-sizes to its content horizontally,
 * vertically, or in both directions.
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
public enum ShrinkWrap {
	width, height, both
}
