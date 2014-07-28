package org.skyve.wildcat.metadata.repository.document;

import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;

import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.DOCUMENT_NAMESPACE)
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
