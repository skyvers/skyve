package org.skyve.impl.metadata.repository.module;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlRootElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "bizQLImport")
@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE, name = "bizQLImport")
public class BizQLReferenceMetaData extends QueryReferenceMetaData {
	private static final long serialVersionUID = -4142739690810542267L;
}
