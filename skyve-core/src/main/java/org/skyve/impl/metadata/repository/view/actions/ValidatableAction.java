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

	/**
	 * Returns whether client-side validation should run before this action executes.
	 *
	 * @return {@code true} to perform client-side validation, {@code false} to bypass it,
	 *         or {@code null} to use framework defaults
	 */
	public Boolean getClientValidation() {
		return clientValidation;
	}
	
	/**
	 * Sets whether client-side validation should run before this action executes.
	 *
	 * @param clientValidation {@code true} to enforce client-side validation,
	 *        {@code false} to skip it, or {@code null} to use framework defaults
	 */
	@XmlAttribute(required = false)
	public void setClientValidation(Boolean clientValidation) {
		this.clientValidation = clientValidation;
	}

	/**
	 * Converts this descriptor to runtime metadata including validation behaviour.
	 *
	 * @return runtime action metadata with client-validation policy applied
	 */
	@Override
	public ActionImpl toMetaDataAction() {
		ActionImpl result = super.toMetaDataAction();
		result.setClientValidation(clientValidation);
		return result;
	}
}
