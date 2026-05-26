package org.skyve.metadata.sail.language.step.interaction;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Populates the current edit view with a generated test fixture, entering
 * random but valid values for all editable widgets in one step.
 *
 * <p>The optional {@code fixture} attribute names a specific
 * {@link org.skyve.util.DataBuilder} fixture to use for generation; if omitted
 * the executor uses the default test fixture for the current document.
 *
 * @see org.skyve.util.DataBuilder
 * @see DataEnter
 * @see org.skyve.metadata.sail.execution.Executor#executeTestDataEnter
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class TestDataEnter implements Step {

	// The name of a fixture defined using Data Factory mechanism
	private String fixture;

	/**
	 * Returns the fixture.
	 * @return the result
	 */
	public String getFixture() {
		return fixture;
	}

	/**
	 * Sets the fixture.
	 * @param fixture the fixture
	 */
	@XmlAttribute(name = "fixture")
	public void setFixture(String fixture) {
		this.fixture = UtilImpl.processStringValue(fixture);
	}

	/**
	 * Executes execute.
	 * @param executor the executor
	 */
	@Override
	public void execute(Executor executor) {
		executor.executeTestDataEnter(this);
	}
	
	/**
	 * Returns the identifier.
	 * @param context the context
	 * @return the result
	 */
	@Override
	public String getIdentifier(AutomationContext context) {
		return null;
	}
}
