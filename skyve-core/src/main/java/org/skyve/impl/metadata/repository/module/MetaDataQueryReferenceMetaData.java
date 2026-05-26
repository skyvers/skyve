package org.skyve.impl.metadata.repository.module;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated descriptor for a {@code <queryImport>} element in a module,
 * importing a metadata query defined in another module.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see QueryReferenceMetaData
 * @see MetaDataQueryMetaData
 */
@XmlRootElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "queryImport")
@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE, name = "queryImport")
public class MetaDataQueryReferenceMetaData extends QueryReferenceMetaData {
	private static final long serialVersionUID = 5027371768973533290L;
}
