package org.skyve.impl.metadata.repository.module;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;

@XmlType(namespace = XMLMetaData.MODULE_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.MODULE_NAMESPACE, name = "singular")
public class SingularUserAccessMetaData extends UserAccessMetaData {
	private static final long serialVersionUID = 2888202790675893096L;

	private String documentName;

	public String getDocumentName() {
		return documentName;
	}

	@XmlAttribute(name = "document", required = true)
	public void setDocumentName(String documentName) {
		this.documentName = documentName;
	}
}
