package org.skyve.impl.metadata.repository.customer;

import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.metadata.repository.NamedMetaData;
import org.skyve.impl.util.XMLMetaData;

@XmlType(namespace = XMLMetaData.CUSTOMER_NAMESPACE, name = "module")
public class CustomerModuleMetaData extends NamedMetaData {
	private static final long serialVersionUID = -4251806306391755042L;

	// no extra properties or methods
}
