package org.skyve.metadata.view.model.chart;

import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
public enum OrderBy {
	category, value
}
