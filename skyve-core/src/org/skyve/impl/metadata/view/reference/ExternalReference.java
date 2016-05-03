package org.skyve.impl.metadata.view.reference;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLUtil;

/**
 * This reference can specify a href to the internet 
 * or a binding to an attribute with a href as a value.
 * 
 * @author mike
 */
@XmlType(namespace = XMLUtil.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.VIEW_NAMESPACE)
public class ExternalReference implements Reference {
	/**
	 * For Serialization
	 */
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
