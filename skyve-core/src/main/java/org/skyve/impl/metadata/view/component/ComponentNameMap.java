package org.skyve.impl.metadata.view.component;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaData;

@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE, propOrder = {"fromComponent", "mappedTo"})
public class ComponentNameMap implements MetaData {
	private static final long serialVersionUID = -1586157078585007880L;

	private String fromComponent;
	private String mappedTo;

	public String getFromComponent() {
		return fromComponent;
	}

	@XmlAttribute(required = true)
	public void setFromComponent(String fromComponent) {
		this.fromComponent = fromComponent;
	}

	public String getMappedTo() {
		return mappedTo;
	}

	@XmlAttribute(required = true)
	public void setMappedTo(String mappedTo) {
		this.mappedTo = mappedTo;
	}
}
