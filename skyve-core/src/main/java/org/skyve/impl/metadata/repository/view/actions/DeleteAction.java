package org.skyve.impl.metadata.repository.view.actions;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.controller.ImplicitActionName;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated descriptor for a {@code <delete>} action button in a view.
 *
 * <p>A delete action removes the current bean from persistence and returns to
 * the parent list.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see ValidatableAction
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "delete")
public class DeleteAction extends ValidatableAction {
	private static final long serialVersionUID = 1284934610722030822L;

	public DeleteAction() {
		implicitName = ImplicitActionName.Delete;
	}
}
