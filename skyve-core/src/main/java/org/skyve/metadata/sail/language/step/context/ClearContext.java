package org.skyve.metadata.sail.language.step.context;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Removes all contexts from the automation execution stack, resetting the current
 * automation run to a neutral state with no active module or document context.
 *
 * <p>Use this step at the start of a {@link org.skyve.metadata.sail.language.Procedure}
 * to guarantee a clean context regardless of prior execution state.
 *
 * @see PopContext
 * @see PushEditContext
 * @see PushListContext
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class ClearContext implements Step {

	/**
	 * Executes execute.
	 * @param executor the executor
	 */
	@Override
	public void execute(Executor executor) {
		executor.executeClearContext(this);
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
