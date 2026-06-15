package org.skyve.impl.metadata.repository.view.actions;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.controller.ImplicitActionName;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated descriptor for a {@code <zoomOut>} action button in a
 * sub-document view.
 *
 * <p>A zoom-out action saves the child bean and returns to the parent document's
 * edit view.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see ValidatableAction
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "zoomOut")
public class ZoomOutAction extends ValidatableAction {
	private static final long serialVersionUID = 1725163676173730339L;

	public ZoomOutAction() {
		implicitName = ImplicitActionName.ZoomOut;
	}
}
