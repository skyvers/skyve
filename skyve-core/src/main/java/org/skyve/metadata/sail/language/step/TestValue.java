package org.skyve.metadata.sail.language.step;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Asserts that the widget bound to the specified binding currently displays the expected value.
 *
 * <p>Both {@code binding} and {@code value} are required. The executor compares the
 * widget's current display value against {@code value} as a string and fails the
 * automation run if they do not match.
 *
 * @see org.skyve.metadata.sail.execution.Executor#executeTestValue
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class TestValue implements Step {

	private String binding;
	private String value;

	/**
	 * Returns the binding.
	 * @return the result
	 */
	public String getBinding() {
		return binding;
	}

	/**
	 * Sets the binding.
	 * @param binding the binding
	 */
	@XmlAttribute(name = "binding", required = true)
	public void setBinding(String binding) {
		this.binding = UtilImpl.processStringValue(binding);
	}

	/**
	 * Returns the value.
	 * @return the result
	 */
	public String getValue() {
		return value;
	}

	/**
	 * Sets the value.
	 * @param value the value
	 */
	@XmlAttribute(name = "value", required = true)
	public void setValue(String value) {
		this.value = UtilImpl.processStringValue(value);
	}

	/**
	 * Executes execute.
	 * @param executor the executor
	 */
	@Override
	public void execute(Executor executor) {
		executor.executeTestValue(this);
	}
	
	/**
	 * Returns the identifier.
	 * @param context the context
	 * @return the result
	 */
	@Override
	public String getIdentifier(AutomationContext context) {
		return binding;
	}
}
