package org.skyve.impl.metadata.repository.view.actions;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.controller.ImplicitActionName;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated descriptor for a {@code <cancel>} action button in a view.
 *
 * <p>A cancel action discards unsaved changes and returns to the parent
 * list or the previous view without persisting.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see PositionableAction
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "cancel")
public class CancelAction extends PositionableAction {
	private static final long serialVersionUID = -1531760495701515987L;

	public CancelAction() {
		implicitName = ImplicitActionName.Cancel;
	}
}
