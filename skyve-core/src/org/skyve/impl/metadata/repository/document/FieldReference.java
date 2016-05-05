package org.skyve.impl.metadata.repository.document;

import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public class FieldReference {
	private String ref;

	public String getRef() {
		return ref;
	}

	@XmlValue
	public void setRef(String ref) {
		this.ref = UtilImpl.processStringValue(ref);
	}
}
