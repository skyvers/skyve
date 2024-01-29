package org.skyve.impl.metadata.repository.document;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.SerializableMetaData;

import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.XmlValue;

@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public class FieldReference implements SerializableMetaData {
	private static final long serialVersionUID = 4832758292142080515L;

	private String ref;

	public String getRef() {
		return ref;
	}

	@XmlValue
	public void setRef(String ref) {
		this.ref = UtilImpl.processStringValue(ref);
	}
}
