package org.skyve.impl.metadata.repository;

import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;

/**
 * Represents a Side or Top layout.
 */
@XmlType(namespace = XMLMetaData.COMMON_NAMESPACE)
public enum ViewLayout {
	side,
	top
}
