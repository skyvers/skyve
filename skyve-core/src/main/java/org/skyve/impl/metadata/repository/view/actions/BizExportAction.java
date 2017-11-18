package org.skyve.impl.metadata.repository.view.actions;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.controller.ImplicitActionName;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "export")
public class BizExportAction extends ClassAction {
	private static final long serialVersionUID = -8910845366016496257L;

	public BizExportAction() {
		implicitName = ImplicitActionName.BizExport;
	}
}
