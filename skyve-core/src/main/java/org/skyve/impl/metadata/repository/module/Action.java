package org.skyve.impl.metadata.repository.module;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.metadata.repository.NamedMetaData;
import org.skyve.impl.util.XMLMetaData;

@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE)
public class Action extends NamedMetaData {
	/**
	 * 
	 */
	private static final long serialVersionUID = 4928567931483027768L;
	private List<ApplicableTo> uxuis = new ArrayList<>();

	@XmlElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "uxui", required = false)
	public List<ApplicableTo> getUxuis() {
		return uxuis;
	}
}
