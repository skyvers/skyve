package org.skyve.impl.metadata.repository.document;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Represents a Map<String, String> in JAXB.
 */
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public final class DynamicClassMapType implements Serializable {
	private static final long serialVersionUID = 7108031158729086800L;

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name="class")
	public List<DynamicClassMapEntryType> classes = new ArrayList<>();
}
