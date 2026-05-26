package org.skyve.impl.metadata.repository.view.actions;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.controller.ImplicitActionName;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated descriptor for an {@code <export>} action button in a view.
 *
 * <p>An export action invokes the server-side export class identified by
 * {@link ClassAction#getClassName()} to stream the current data set as a
 * downloadable file (CSV, Excel, etc.).
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see ClassAction
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "export")
public class BizExportAction extends ClassAction {
	private static final long serialVersionUID = -8910845366016496257L;

	public BizExportAction() {
		implicitName = ImplicitActionName.BizExport;
	}
}
