package org.skyve.metadata.model;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.SerializableMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Records the parent document name in a Skyve document inheritance hierarchy.
 *
 * <p>When a document XML declares {@code <extends document="ParentDoc" />}, the
 * metadata loader creates an {@code Extends} instance carrying the parent document name.
 * The domain generator uses this to set up Java inheritance and the ORM inheritance
 * strategy declared in {@link Persistent}.
 *
 * @see Persistent.ExtensionStrategy
 */
@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public class Extends implements SerializableMetaData {
	private static final long serialVersionUID = -7377979289426429434L;
	
	private String documentName;
 	/**
 	 * Returns the documentName.
 	 * @return the result
 	 */
 	public String getDocumentName() {
        return documentName;
    }

    /**
     * Sets the documentName.
     * @param documentName the documentName
     */
    @XmlAttribute(name="document", required = true)
    public void setDocumentName(String documentName) {
        this.documentName = UtilImpl.processStringValue(documentName);
    }
}
