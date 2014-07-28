package org.skyve.wildcat.metadata.repository.customer;

import javax.xml.bind.annotation.XmlType;

import org.skyve.wildcat.metadata.repository.NamedMetaData;
import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.CUSTOMER_NAMESPACE, name = "module")
public class CustomerModuleMetaData extends NamedMetaData {
	// no extra properties or methods
}
