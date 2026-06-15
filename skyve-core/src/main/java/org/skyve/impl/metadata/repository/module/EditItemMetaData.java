package org.skyve.impl.metadata.repository.module;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated descriptor for an {@code <edit>} menu item in a module menu.
 *
 * <p>Navigates directly to the edit view for a named document, creating a new
 * instance or targeting an existing one.  Extends {@link ItemMetaData}.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see ItemMetaData
 */
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
