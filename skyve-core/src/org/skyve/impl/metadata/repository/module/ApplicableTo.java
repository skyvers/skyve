package org.skyve.impl.metadata.repository.module;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLUtil;

@XmlType(namespace = XMLUtil.MODULE_NAMESPACE)
public class ApplicableTo {
	private String uxui;

	public String getUxUi() {
		return uxui;
	}

	@XmlAttribute(name = "name", required = true)
	public void setUxUi(String uxui) {
		this.uxui = UtilImpl.processStringValue(uxui);
	}
}
