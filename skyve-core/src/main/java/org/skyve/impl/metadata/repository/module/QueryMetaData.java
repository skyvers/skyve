package org.skyve.impl.metadata.repository.module;

import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.metadata.repository.NamedMetaData;
import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;

import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * Abstract JAXB base for all named query descriptor types in a module.
 *
 * <p>Carries the query name, description, documentation, and decorator properties.
 * Concrete subtypes add a query body ({@link QueryDefinitionMetaData}) or a
 * cross-module reference ({@link QueryReferenceMetaData}).
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see QueryDefinitionMetaData
 * @see QueryReferenceMetaData
 */
@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE, 
			propOrder = {"properties"})
public abstract class QueryMetaData extends NamedMetaData implements DecoratedMetaData {
	private static final long serialVersionUID = -1776371200320514678L;

	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	@Override
	public Map<String, String> getProperties() {
		return properties;
	}
}
