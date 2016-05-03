package org.skyve.impl.metadata.repository.view.actions;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLUtil;
import org.skyve.metadata.controller.ImplicitActionName;

@XmlType(namespace = XMLUtil.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLUtil.VIEW_NAMESPACE, name = "remove")
public class RemoveAction extends PositionableAction {
	public RemoveAction() {
		implicitName = ImplicitActionName.Remove;
	}
}
