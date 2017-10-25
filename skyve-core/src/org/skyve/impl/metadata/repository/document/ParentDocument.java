package org.skyve.impl.metadata.repository.document;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaData;

@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public class ParentDocument implements MetaData {
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
