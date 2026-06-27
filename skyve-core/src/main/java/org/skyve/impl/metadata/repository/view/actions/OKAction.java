package org.skyve.impl.metadata.repository.view.actions;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.controller.ImplicitActionName;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated descriptor for an {@code <ok>} action button in a view.
 *
 * <p>An OK action saves the current bean and returns to the parent list/view.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see ValidatableAction
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "ok")
public class OKAction extends ValidatableAction {
	private static final long serialVersionUID = -6931288731473105900L;

	public OKAction() {
		implicitName = ImplicitActionName.OK;
	}
}
