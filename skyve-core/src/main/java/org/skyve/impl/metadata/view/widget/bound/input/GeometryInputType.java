package org.skyve.impl.metadata.view.widget.bound.input;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
public enum GeometryInputType {
	point,
	line,
	polygon,
	pointAndLine,
	pointAndPolygon,
	lineAndPolygon
}
