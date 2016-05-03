package org.skyve.impl.metadata.view;

import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLUtil;

@XmlType(namespace = XMLUtil.VIEW_NAMESPACE)
public enum DownloadAreaType {
	resources, content;
}
