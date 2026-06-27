package org.skyve.metadata.sail.language.step.interaction.actions;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.sail.execution.Executor;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Clicks the Delete button ({@link org.skyve.metadata.controller.ImplicitActionName#Delete})
 * on the current edit view, permanently removing the current record.
 *
 * <p>Side effects: if the framework displays a confirmation dialog before deleting,
 * the executor handles the confirmation automatically.
 *
 * @see Cancel
 * @see Remove
 * @see org.skyve.metadata.sail.execution.Executor#executeDelete
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class Delete extends AbstractAction {

	/**
	 * Executes execute.
	 * @param executor the executor
	 */
	@Override
	public void execute(Executor executor) {
		executor.executeDelete(this);
	}
	
	/**
	 * Returns the identifier.
	 * @param context the context
	 * @return the result
	 */
	@Override
	public String getIdentifier(AutomationContext context) {
		return ImplicitActionName.Delete.toString();
	}
}
