package org.skyve.impl.metadata.repository.view.actions;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated descriptor for a {@code <action>} custom action button in a view.
 *
 * <p>A custom action invokes the server-side implementation class identified by
 * {@link ClassAction#getClassName()}.  This is the primary extension point for
 * user-defined action logic.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see ClassAction
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "action")
public class CustomAction extends ClassAction {
	private static final long serialVersionUID = 2552404774127789764L;

	// nothing to see here
}
