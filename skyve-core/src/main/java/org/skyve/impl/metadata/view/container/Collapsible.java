package org.skyve.impl.metadata.view.container;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated enumeration of collapsibility modes for panel-type containers.
 *
 * <p>Controls whether a container can be collapsed by the user, and if so,
 * whether it starts expanded or collapsed.
 */
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
