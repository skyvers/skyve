package org.skyve.wildcat.metadata.repository.customer;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.CUSTOMER_NAMESPACE)
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
