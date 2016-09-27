package org.skyve.impl.metadata.view.event;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.impl.metadata.view.event.EventAction;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "rerender")
public class RerenderEventAction implements EventAction {
	private Boolean clientValidation;

	public Boolean getClientValidation() {
		return clientValidation;
	}
	
	@XmlAttribute(required = false)
	public void setClientValidation(Boolean clientValidation) {
		this.clientValidation = clientValidation;
	}
}
