package org.skyve.impl.metadata.repository.view.actions;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.controller.ImplicitActionName;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated descriptor for a {@code <download>} action button in a view.
 *
 * <p>A download action invokes the server-side download class identified by
 * {@link ClassAction#getClassName()} to produce a binary response (PDF,
 * spreadsheet, etc.) for the browser to save.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see ClassAction
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "download")
public class DownloadAction extends ClassAction {
	private static final long serialVersionUID = -3193664397813167826L;

	public DownloadAction() {
		implicitName = ImplicitActionName.Download;
	}
}
