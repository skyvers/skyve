package org.skyve.metadata.sail.language.step.context;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Pops the current context from the automation execution stack, returning control
 * to the enclosing context that was active before the last push.
 *
 * <p>Precondition: at least one context must have been pushed via
 * {@link PushEditContext} or {@link PushListContext}; behaviour is undefined if
 * the stack is empty when this step executes.
 *
 * @see PushEditContext
 * @see PushListContext
 * @see ClearContext
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class PopContext implements Step {

	/**
	 * Executes execute.
	 * @param executor the executor
	 */
	@Override
	public void execute(Executor executor) {
		executor.executePopContext(this);
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
