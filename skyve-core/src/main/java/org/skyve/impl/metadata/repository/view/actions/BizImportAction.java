package org.skyve.impl.metadata.repository.view.actions;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.controller.ImplicitActionName;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "import")
public class BizImportAction extends ClassAction {
	private static final long serialVersionUID = 3329981529910164039L;

	public BizImportAction() {
		implicitName = ImplicitActionName.BizImport;
	}
}
