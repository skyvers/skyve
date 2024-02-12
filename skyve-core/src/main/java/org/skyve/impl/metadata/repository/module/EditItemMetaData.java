package org.skyve.impl.metadata.repository.module;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlType(name = "edit", namespace = XMLMetaData.MODULE_NAMESPACE)
@XmlRootElement(name = "edit", namespace = XMLMetaData.MODULE_NAMESPACE)
public class EditItemMetaData extends ItemMetaData {
	private static final long serialVersionUID = -810769802744111708L;

	private String documentName;

	public String getDocumentName() {
		return documentName;
	}
	@XmlAttribute(name = "document", required = true)
	public void setDocumentName(String documentName) {
		this.documentName = UtilImpl.processStringValue(documentName);
	}
}
