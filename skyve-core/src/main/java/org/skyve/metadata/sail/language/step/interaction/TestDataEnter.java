package org.skyve.metadata.sail.language.step.interaction;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.ExecutionOptions;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Populates the view with a randomly generated test instance using a specified fixture.
 * 
 * @author mike
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class TestDataEnter implements Step {

	// The name of a fixture defined using Data Factory mechanism
	private String fixture;

	public String getFixture() {
		return fixture;
	}

	@XmlAttribute(name = "fixture")
	public void setFixture(String fixture) {
		this.fixture = UtilImpl.processStringValue(fixture);
	}

	@Override
	public void execute(Executor executor, ExecutionOptions options) {
		executor.executeTestDataEnter(this);
	}
	
	@Override
	public String getIdentifier(AutomationContext context) {
		return null;
	}
}
