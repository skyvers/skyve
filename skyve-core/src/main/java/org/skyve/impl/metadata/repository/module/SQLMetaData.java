package org.skyve.impl.metadata.repository.module;

import org.skyve.impl.domain.types.jaxb.CDATAAdapter;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * JAXB-annotated descriptor for a {@code <sql>} query element in a module.
 *
 * <p>Holds the raw SQL query string body.  The repository bootstrap converts it
 * to a runtime {@link org.skyve.metadata.module.query.SQLDefinition} backed by
 * {@link org.skyve.impl.metadata.module.query.SQLDefinitionImpl}.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see QueryDefinitionMetaData
 * @see SQLReferenceMetaData
 */
@XmlRootElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "sql")
@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE, name = "sql", propOrder = {"query"})
public class SQLMetaData extends QueryDefinitionMetaData {
	private static final long serialVersionUID = 2092696254537507474L;

	private String query;

	public String getQuery() {
		return query;
	}

	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE, required = true)
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setQuery(String query) {
		this.query = UtilImpl.processStringValue(query);
	}
}
