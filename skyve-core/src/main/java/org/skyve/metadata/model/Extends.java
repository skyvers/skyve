package org.skyve.metadata.model;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.SerializableMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public class Extends implements SerializableMetaData {
	private static final long serialVersionUID = -7377979289426429434L;
	
	private String documentName;
 	public String getDocumentName() {
        return documentName;
    }

    @XmlAttribute(name="document", required = true)
    public void setDocumentName(String documentName) {
        this.documentName = UtilImpl.processStringValue(documentName);
    }
}
