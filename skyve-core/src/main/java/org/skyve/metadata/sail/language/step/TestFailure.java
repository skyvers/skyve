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
 * Asserts that the current view contains at least one validation error, optionally
 * matching the expected error message text.
 *
 * <p>Fails the automation run if <em>no</em> error is visible, or if a specific
 * {@code message} is provided and no displayed error matches it. Use this to verify
 * that the application correctly rejects invalid input.
 *
 * @see TestSuccess
 * @see org.skyve.metadata.sail.execution.Executor#executeTestFailure
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class TestFailure implements Step {

	private String message;

	/**
	 * Returns the message.
	 * @return the result
	 */
	public String getMessage() {
		return message;
	}

	/**
	 * Sets the message.
	 * @param message the message
	 */
	@XmlAttribute(name = "message")
	public void setMessage(String message) {
		this.message = UtilImpl.processStringValue(message);
	}

	/**
	 * Executes execute.
	 * @param executor the executor
	 */
	@Override
	public void execute(Executor executor) {
		executor.executeTestFailure(this);
	}
	
	/**
	 * Returns the identifier.
	 * @param context the context
	 * @return the result
	 */
	@Override
	public String getIdentifier(AutomationContext context) {
		return "Failure";
	}
}
