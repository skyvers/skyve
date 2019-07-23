package org.skyve.impl.metadata.view.widget.bound.input;

import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
public enum GeometryInputType {
	point,
	line,
	polygon,
	pointAndLine,
	pointAndPolygon,
	lineAndPolygon
}
