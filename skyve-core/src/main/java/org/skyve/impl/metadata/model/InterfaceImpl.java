package org.skyve.impl.metadata.model;

import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.model.document.Interface;

@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public class InterfaceImpl implements Interface {
	private static final long serialVersionUID = 5670142647312527597L;

	private String name;

	public void setInterfaceName(String name) {
		this.name = name;
	}

	@XmlValue
	@Override
	public String getInterfaceName() {
		return name;
	}
}
