package org.skyve.impl.metadata.repository.module;

import org.skyve.impl.domain.types.jaxb.CDATAAdapter;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * JAXB-annotated descriptor for a {@code <bizQL>} query element in a module.
 *
 * <p>Holds the BizQL (Skyve object-query language) query string body.  The
 * repository bootstrap converts it to a runtime
 * {@link org.skyve.metadata.module.query.BizQLDefinition} backed by
 * {@link org.skyve.impl.metadata.module.query.BizQLDefinitionImpl}.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see QueryDefinitionMetaData
 * @see BizQLReferenceMetaData
 */
@XmlRootElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "bizQL")
@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE, name = "bizQL", propOrder = {"query"})
public class BizQLMetaData extends QueryDefinitionMetaData {
	private static final long serialVersionUID = -3313909514104053162L;

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
