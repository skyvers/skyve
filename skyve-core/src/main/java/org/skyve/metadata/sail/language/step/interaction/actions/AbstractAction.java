package org.skyve.metadata.sail.language.step.interaction.actions;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.language.Step;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Extends Step and adds the ability to turn success testing on and off.
 * @author mike
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public abstract class AbstractAction implements Step {
	// Defaults to on - ie null/true and false
	private Boolean testSuccess;
	
	public Boolean getTestSuccess() {
		return testSuccess;
	}
	
	@XmlAttribute(name = "testSuccess")
	public void setTestSuccess(Boolean testSuccess) {
		this.testSuccess = testSuccess;
	}
}
