package org.skyve.impl.metadata.view.reference;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * This reference can specify a href to the internet 
 * or a binding to an attribute with a href as a value.
 * 
 * @author mike
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
public class ExternalReference implements Reference {
	private static final long serialVersionUID = -5628226053506310381L;
	
	private String href;

	public String getHref() {
		return href;
	}

	@XmlAttribute(required = true)
	public void setHref(String href) {
		this.href = UtilImpl.processStringValue(href);
	}
}
