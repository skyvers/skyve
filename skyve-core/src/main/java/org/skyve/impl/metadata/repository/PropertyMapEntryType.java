package org.skyve.impl.metadata.repository;

import java.io.Serializable;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.XmlValue;

/**
 * Represents a key/value map entry in JAXB.
 */
@XmlType(namespace = XMLMetaData.COMMON_NAMESPACE)
public final class PropertyMapEntryType implements Serializable {
	private static final long serialVersionUID = -5310288850322753579L;

	@XmlAttribute
	public String key;
	@XmlValue
	public String value;
}
