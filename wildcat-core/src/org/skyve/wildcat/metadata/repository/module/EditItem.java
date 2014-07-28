package org.skyve.wildcat.metadata.repository.module;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;

@XmlType(name = "edit", namespace = XMLUtil.MODULE_NAMESPACE)
@XmlRootElement(name = "edit", namespace = XMLUtil.MODULE_NAMESPACE)
public class EditItem extends Item {
	private String documentName;

	public String getDocumentName() {
		return documentName;
	}
	@XmlAttribute(name = "document", required = true)
	public void setDocumentName(String documentName) {
		this.documentName = UtilImpl.processStringValue(documentName);
	}
}
