package org.skyve.impl.metadata.repository.customer;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.customer.HTMLResources;

@XmlType(namespace = XMLMetaData.CUSTOMER_NAMESPACE)
public class HTMLResourcesMetaData implements HTMLResources {
	private static final long serialVersionUID = 3982423990945642514L;

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
