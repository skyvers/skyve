package org.skyve.impl.metadata.repository.module;

import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.metadata.repository.NamedMetaData;
import org.skyve.impl.util.XMLMetaData;

@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE)
public class UserAccessUxUiMetadata extends NamedMetaData {
	private static final long serialVersionUID = 6147710006677424683L;
}
