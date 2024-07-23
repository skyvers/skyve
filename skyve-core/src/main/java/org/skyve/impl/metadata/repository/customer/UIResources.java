package org.skyve.impl.metadata.repository.customer;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.CUSTOMER_NAMESPACE)
public class UIResources implements org.skyve.metadata.customer.UIResources {
	private static final long serialVersionUID = 7253552655682697331L;

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
