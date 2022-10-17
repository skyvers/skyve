package org.skyve.impl.metadata.repository.view.access;

import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.metadata.repository.NamedMetaData;
import org.skyve.impl.util.XMLMetaData;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
public class ViewUserAccessUxUiMetadata extends NamedMetaData {
	private static final long serialVersionUID = -3523148277675838881L;
}
