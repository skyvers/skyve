package org.skyve.impl.metadata.repository.view.actions;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.controller.ImplicitActionName;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated descriptor for an {@code <upload>} action button in a view.
 *
 * <p>An upload action invokes the server-side upload class identified by
 * {@link ClassAction#getClassName()} after the user selects a file, streaming
 * the upload to the handler for processing.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see ClassAction
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "upload")
public class UploadAction extends ClassAction {
	private static final long serialVersionUID = -116132937073829890L;

	public UploadAction() {
		implicitName = ImplicitActionName.Upload;
	}
}
