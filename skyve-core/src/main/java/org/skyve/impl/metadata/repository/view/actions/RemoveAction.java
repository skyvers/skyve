package org.skyve.impl.metadata.repository.view.actions;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.controller.ImplicitActionName;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "remove")
public class RemoveAction extends PositionableAction {
	private static final long serialVersionUID = -5142675544537387801L;

	public RemoveAction() {
		implicitName = ImplicitActionName.Remove;
	}
}
