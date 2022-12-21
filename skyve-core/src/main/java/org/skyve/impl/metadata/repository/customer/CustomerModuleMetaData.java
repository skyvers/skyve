package org.skyve.impl.metadata.repository.customer;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.metadata.repository.NamedMetaData;
import org.skyve.impl.metadata.repository.ViewLayout;
import org.skyve.impl.util.XMLMetaData;

@XmlType(namespace = XMLMetaData.CUSTOMER_NAMESPACE, name = "module")
public class CustomerModuleMetaData extends NamedMetaData {
	private static final long serialVersionUID = -4251806306391755042L;

	private ViewLayout viewLayout;

	public ViewLayout getViewLayout() {
		return viewLayout;
	}

	@XmlAttribute
	public void setViewLayout(ViewLayout viewLayout) {
		this.viewLayout = viewLayout;
	}
}
