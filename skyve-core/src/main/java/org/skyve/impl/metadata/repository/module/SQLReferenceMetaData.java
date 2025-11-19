package org.skyve.impl.metadata.repository.module;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlRootElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "sqlImport")
@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE, name = "sqlImport")
public class SQLReferenceMetaData extends QueryReferenceMetaData {
	private static final long serialVersionUID = -3741912587308448423L;
}
