package org.skyve.impl.metadata.repository;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;

import org.skyve.impl.util.XMLMetaData;

/**
 * Represents a key/value map entry in JAXB.
 */
@XmlType(namespace = XMLMetaData.COMMON_NAMESPACE)
public final class PropertyMapEntryType {
	@XmlAttribute
	public String key;
	@XmlValue
	public String value;
}
