package org.skyve.impl.metadata.repository.view.actions;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.controller.ImplicitActionName;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "upload")
public class UploadAction extends ClassAction {
	private static final long serialVersionUID = -116132937073829890L;

	public UploadAction() {
		implicitName = ImplicitActionName.Upload;
	}
}
