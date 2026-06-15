package org.skyve.impl.metadata.repository.document;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Represents a Map<String, String> in JAXB.
 *
 * <p>Represents the serialised form of a {@code Map<String, String>} where each
 * {@code <class>} entry maps a logical dynamic key to a fully-qualified class name.
 */
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public final class DynamicClassMapType implements Serializable {
	private static final long serialVersionUID = 7108031158729086800L;

	/**
	 * Ordered JAXB entries representing dynamic key-to-class mappings.
	 */
	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name="class")
	public List<DynamicClassMapEntryType> classes = new ArrayList<>();
}
