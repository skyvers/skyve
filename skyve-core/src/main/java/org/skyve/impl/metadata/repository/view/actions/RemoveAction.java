package org.skyve.impl.metadata.repository.view.actions;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.controller.ImplicitActionName;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "remove")
public class RemoveAction extends PositionableAction {
	private static final long serialVersionUID = -5142675544537387801L;

	public RemoveAction() {
		implicitName = ImplicitActionName.Remove;
	}
}
