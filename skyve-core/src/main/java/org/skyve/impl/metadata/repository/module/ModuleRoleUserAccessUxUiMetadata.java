package org.skyve.impl.metadata.repository.module;

import org.skyve.impl.metadata.repository.NamedMetaData;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE)
public class ModuleRoleUserAccessUxUiMetadata extends NamedMetaData {
	private static final long serialVersionUID = 6147710006677424683L;
}
