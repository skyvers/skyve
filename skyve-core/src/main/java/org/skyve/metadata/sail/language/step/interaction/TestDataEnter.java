package org.skyve.metadata.sail.language.step.interaction;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

/**
 * Generate a random instance from the test system and scatter the values into the view.
 * @author mike
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class TestDataEnter implements Step {
	// the name of a fixture defined using Data Factory mechanism
	private String fixture;

	public String getFixture() {
		return fixture;
	}

	@XmlAttribute(name = "fixture")
	public void setFixture(String fixture) {
		this.fixture = UtilImpl.processStringValue(fixture);
	}

	@Override
	public void execute(Executor executor) {
		executor.executeTestDataEnter(this);
	}
	
	@Override
	public String getIdentifier(AutomationContext context) {
		return null;
	}
}
