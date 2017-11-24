package org.skyve.impl.metadata.repository.view.actions;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.controller.ImplicitActionName;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "save")
public class SaveAction extends ValidatableAction {
	private static final long serialVersionUID = -3342187550945463414L;

	public SaveAction() {
		implicitName = ImplicitActionName.Save;
	}
}
