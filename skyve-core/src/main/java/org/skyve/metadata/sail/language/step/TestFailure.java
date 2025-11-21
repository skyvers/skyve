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
 * Verifies that one or more errors are present in the UI, optionally checking the error message.
 * 
 * @author mike
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class TestFailure implements Step {

	private String message;

	public String getMessage() {
		return message;
	}

	@XmlAttribute(name = "message")
	public void setMessage(String message) {
		this.message = UtilImpl.processStringValue(message);
	}

	@Override
	public void execute(Executor executor) {
		executor.executeTestFailure(this);
	}
	
	@Override
	public String getIdentifier(AutomationContext context) {
		return "Failure";
	}
}
