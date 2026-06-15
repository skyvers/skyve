package org.skyve.impl.metadata.view.widget.bound.input;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated enumeration of geometry capture modes for geometry input
 * widgets.
 *
 * <p>Controls which geometry shapes users may draw: point, line, polygon,
 * or configured combinations.
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@SuppressWarnings("java:S115") // Suppress "Constant names should comply with a naming convention" as these are not constants but enum values
public enum GeometryInputType {
	point,
	line,
	polygon,
	pointAndLine,
	pointAndPolygon,
	lineAndPolygon
}
