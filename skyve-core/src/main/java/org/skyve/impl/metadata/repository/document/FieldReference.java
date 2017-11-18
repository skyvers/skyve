package org.skyve.impl.metadata.repository.document;

import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaData;

@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
public class FieldReference implements MetaData {
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
