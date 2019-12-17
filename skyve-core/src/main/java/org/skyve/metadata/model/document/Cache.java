package org.skyve.metadata.model.document;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;

@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name = "cache")
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE,
			name = "cache",
			propOrder = {"name", "type"})
public class Cache {
	@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public static enum CacheType {
		readOnly,
		readWrite
	}

	private String name;
	private CacheType type;

	public String getName() {
		return name;
	}
	
	@XmlAttribute(required = true)
	public void setName(String name) {
		this.name = name;
	}

	public CacheType getType() {
		return type;
	}

	@XmlAttribute(required = true)
	public void setType(CacheType type) {
		this.type = type;
	}
}
