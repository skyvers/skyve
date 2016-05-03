package org.skyve.impl.metadata.repository.customer;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLUtil;

@XmlType(namespace = XMLUtil.CUSTOMER_NAMESPACE)
public class UIResources implements org.skyve.metadata.customer.UIResources {
	private String logoRelativeFileName;
	
	@Override
	public String getLogoRelativeFileName() {
		return logoRelativeFileName;
	}

	@XmlAttribute(name = "logo")
	public void setLogoRelativeFileName(String logoRelativeFileName) {
		this.logoRelativeFileName = UtilImpl.processStringValue(logoRelativeFileName);
	}
}
