package org.skyve.impl.metadata.repository.view.actions;

import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated base for action descriptors that can bypass client-side
 * validation before execution.
 *
 * <p>Adds a {@code clientValidation} flag to {@link PositionableAction};
 * when {@code false} the framework skips required-field and constraint checks
 * on the client before invoking the action.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see PositionableAction
 */
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
