package org.skyve.impl.metadata.repository.module;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.SerializableMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE)
public class ApplicableTo implements SerializableMetaData {
	private static final long serialVersionUID = 8204068663179740572L;

	private String uxui;

	public String getUxUi() {
		return uxui;
	}

	@XmlAttribute(name = "name", required = true)
	public void setUxUi(String uxui) {
		this.uxui = UtilImpl.processStringValue(uxui);
	}
}
