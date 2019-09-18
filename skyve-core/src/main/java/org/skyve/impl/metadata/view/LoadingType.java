package org.skyve.impl.metadata.view;

import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
public enum LoadingType {
	eager, lazy
}
