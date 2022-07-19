package org.skyve.impl.metadata.repository.module;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

@XmlType(name = "link", namespace = XMLMetaData.MODULE_NAMESPACE)
@XmlRootElement(name = "link", namespace = XMLMetaData.MODULE_NAMESPACE)
public class LinkItemMetaData extends ItemMetaData {
	private static final long serialVersionUID = 4058261818028726112L;

	private String href;

	public String getHref() {
		return href;
	}
	@XmlAttribute(name = "href", required = true)
	public void setHref(String href) {
		this.href = UtilImpl.processStringValue(href);
	}
}
