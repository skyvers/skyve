package org.skyve.wildcat.metadata.repository.customer;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

import org.skyve.wildcat.metadata.repository.NamedMetaData;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.CUSTOMER_NAMESPACE)
public class Service extends NamedMetaData implements org.skyve.metadata.customer.Service {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 5474635172629521175L;

	/**
	 * The fully qualified classname of the service implementation.
	 */
	private String className;

	@Override
	public String getClassName() {
		return className;
	}

	@XmlAttribute(required = true)
	public void setClassName(String className) {
		this.className = UtilImpl.processStringValue(className);
	}
}
