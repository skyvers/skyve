package org.skyve.impl.metadata.repository.view.actions;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.util.XMLMetaData;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
public class ValidatableAction extends PositionableAction {
	private static final long serialVersionUID = 5326651589131252644L;

	private Boolean clientValidation;

	public Boolean getClientValidation() {
		return clientValidation;
	}
	
	@XmlAttribute(required = false)
	public void setClientValidation(Boolean clientValidation) {
		this.clientValidation = clientValidation;
	}

	@Override
	public ActionImpl toMetaDataAction() {
		ActionImpl result = super.toMetaDataAction();
		result.setClientValidation(clientValidation);
		return result;
	}
}
