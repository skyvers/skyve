package org.skyve.impl.metadata.repository.module;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated descriptor for a {@code <bizQLImport>} element in a module,
 * importing a BizQL query defined in another module.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see QueryReferenceMetaData
 * @see BizQLMetaData
 */
@XmlRootElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "bizQLImport")
@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE, name = "bizQLImport")
public class BizQLReferenceMetaData extends QueryReferenceMetaData {
	private static final long serialVersionUID = -4142739690810542267L;
}
