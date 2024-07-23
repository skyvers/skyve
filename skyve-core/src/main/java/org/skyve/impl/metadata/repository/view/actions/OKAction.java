package org.skyve.impl.metadata.repository.view.actions;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.controller.ImplicitActionName;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "ok")
public class OKAction extends ValidatableAction {
	private static final long serialVersionUID = -6931288731473105900L;

	public OKAction() {
		implicitName = ImplicitActionName.OK;
	}
}
