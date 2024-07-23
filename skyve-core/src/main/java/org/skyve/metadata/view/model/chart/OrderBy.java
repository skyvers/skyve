package org.skyve.metadata.view.model.chart;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
public enum OrderBy {
	category, value
}
