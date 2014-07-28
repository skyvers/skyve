package org.skyve.wildcat.metadata.view.reference;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.wildcat.util.XMLUtil;

@XmlType(namespace = XMLUtil.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.VIEW_NAMESPACE)
public class ImplicitActionReference implements Reference {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 2492612188970314035L;

	private ImplicitActionName implicitActionName;

	public ImplicitActionName getImplicitActionName() {
		return implicitActionName;
	}

	@XmlAttribute(required = true)
	public void setImplicitActionName(ImplicitActionName implicitActionName) {
		this.implicitActionName = implicitActionName;
	}
}
