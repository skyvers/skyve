package org.skyve.impl.metadata.repository.document;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.SerializableMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.XmlValue;

@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public class ParentDocument implements SerializableMetaData {
	private static final long serialVersionUID = -4900367761036169747L;

	private Boolean databaseIndex;
	private String parentDocumentName;

	@XmlAttribute
	public Boolean getDatabaseIndex() {
		return databaseIndex;
	}
	public void setDatabaseIndex(Boolean databaseIndex) {
		this.databaseIndex = databaseIndex;
	}
	
	@XmlValue
	public String getParentDocumentName() {
		return parentDocumentName;
	}
	public void setParentDocumentName(String parentDocumentName) {
		this.parentDocumentName = UtilImpl.processStringValue(parentDocumentName);
	}
}
