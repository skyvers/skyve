package org.skyve.impl.metadata.repository;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;

/**
 * Represents a Map<String, String> in JAXB.
 */
@XmlType(namespace = XMLMetaData.COMMON_NAMESPACE)
public final class PropertyMapType implements Serializable {
	private static final long serialVersionUID = 770909412399339666L;

	@XmlElement(namespace = XMLMetaData.COMMON_NAMESPACE, name="property")
	public List<PropertyMapEntryType> properties = new ArrayList<>();
}
