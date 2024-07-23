package org.skyve.impl.metadata.view;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
public enum ShrinkWrap {
	width, height, both
}
