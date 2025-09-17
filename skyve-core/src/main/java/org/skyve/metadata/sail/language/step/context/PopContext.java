package org.skyve.metadata.sail.language.step.context;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.ExecutionOptions;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * A SAIL step that pops the current automation context from the context stack.
 * 
 * @author mike
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class PopContext implements Step {

	@Override
	public void execute(Executor executor, ExecutionOptions options) {
		executor.executePopContext(this);
	}

	@Override
	public String getIdentifier(AutomationContext context) {
		return null;
	}
}
