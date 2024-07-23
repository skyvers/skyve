package org.skyve.impl.metadata.view.container;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
public enum Collapsible {
	open("open"), closed("closed");
	
	private String collapsibleString;
	
	private Collapsible(String collapsibleString) {
		this.collapsibleString = collapsibleString;
	}
	
	public String toCollapsibleString() {
		return this.collapsibleString;
	}
}
