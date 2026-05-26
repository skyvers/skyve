package org.skyve.impl.metadata.repository.view.actions;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.controller.ImplicitActionName;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated descriptor for a {@code <defaults>} action button in a view.
 *
 * <p>A defaults action resets all editable fields on the current form to their
 * metadata-declared default values without persisting.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see ActionMetaData
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "defaults")
public final class DefaultsAction extends ActionMetaData {
	private static final long serialVersionUID = 7370890963580552750L;

	public DefaultsAction() {
		implicitName = ImplicitActionName.DEFAULTS;
	}
}
