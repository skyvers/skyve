package org.skyve.impl.metadata.repository.view.actions;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.controller.ImplicitActionName;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "defaults")
public final class DefaultsAction extends ActionMetaData {
	private static final long serialVersionUID = 7370890963580552750L;

	public DefaultsAction() {
		implicitName = ImplicitActionName.DEFAULTS;
	}
}
