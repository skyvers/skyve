package org.skyve.metadata.sail.language.step.interaction.actions;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.language.Step;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Base class for SAIL action steps that carries an optional post-action success-test toggle.
 *
 * <p>By default ({@code testSuccess} is {@code null} or {@code true}) the executor
 * checks that no validation errors are present after the action runs. Set
 * {@code testSuccess} to {@code false} to suppress the implicit success check, which
 * is useful when an action is expected to produce validation errors that a subsequent
 * {@link org.skyve.metadata.sail.language.step.TestFailure} step will assert.
 *
 * @see Action
 * @see Save
 * @see Delete
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public abstract class AbstractAction implements Step {
	// Defaults to on - ie null/true and false
	private Boolean testSuccess;
	
	/**
	 * Returns the testSuccess.
	 * @return the result
	 */
	public Boolean getTestSuccess() {
		return testSuccess;
	}
	
	/**
	 * Sets the testSuccess.
	 * @param testSuccess the testSuccess
	 */
	@XmlAttribute(name = "testSuccess")
	public void setTestSuccess(Boolean testSuccess) {
		this.testSuccess = testSuccess;
	}
}
