package org.skyve.wildcat.metadata.view.widget;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.metadata.MetaData;
import org.skyve.wildcat.util.XMLUtil;

@XmlRootElement(namespace = XMLUtil.VIEW_NAMESPACE)
@XmlType(namespace = XMLUtil.VIEW_NAMESPACE)
public class Spacer implements MetaData {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -3535525299887373582L;
}
