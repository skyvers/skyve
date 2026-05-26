package org.skyve.impl.metadata.repository.view.actions;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.controller.ImplicitActionName;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated descriptor for a {@code <print>} action button in a view.
 *
 * <p>A print action triggers the browser print dialog for the current view.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see ValidatableAction
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "print")
public class PrintAction extends ValidatableAction {
	private static final long serialVersionUID = -9047041543026042235L;

	public PrintAction() {
		implicitName = ImplicitActionName.Print;
	}
}
