package org.skyve.impl.metadata.repository.customer;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

@XmlType(namespace = XMLMetaData.CUSTOMER_NAMESPACE)
public class HTMLResources implements org.skyve.metadata.customer.HTMLResources {
	private String cssRelativeFileName;
	
	@Override
	public String getCssRelativeFileName() {
		return cssRelativeFileName;
	}

	@XmlAttribute(name = "css")
	public void setCssRelativeFileName(String cssRelativeFileName) {
		this.cssRelativeFileName = UtilImpl.processStringValue(cssRelativeFileName);
	}
}
