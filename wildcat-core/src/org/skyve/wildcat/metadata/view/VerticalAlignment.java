package org.skyve.wildcat.metadata.view;

import javax.xml.bind.annotation.XmlType;

import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.VIEW_NAMESPACE)
public enum VerticalAlignment {
	middle, top, bottom
}
