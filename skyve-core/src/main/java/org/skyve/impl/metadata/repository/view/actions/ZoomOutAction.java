package org.skyve.impl.metadata.repository.view.actions;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.controller.ImplicitActionName;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "zoomOut")
public class ZoomOutAction extends ValidatableAction {
	private static final long serialVersionUID = 1725163676173730339L;

	public ZoomOutAction() {
		implicitName = ImplicitActionName.ZoomOut;
	}
}
