package org.skyve.impl.metadata.view.reference;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.controller.ImplicitActionName;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
/**
 * JAXB-annotated reference to an implicit framework action.
 *
 * <p>Implicit actions represent built-in view operations whose implementation
 * is resolved by framework conventions rather than explicit action metadata.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 */
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
public class ImplicitActionReference implements Reference {
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
