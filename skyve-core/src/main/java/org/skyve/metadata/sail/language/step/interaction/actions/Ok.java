package org.skyve.metadata.sail.language.step.interaction.actions;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.sail.execution.Executor;

import jakarta.annotation.Nonnull;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Clicks the OK button ({@link org.skyve.metadata.controller.ImplicitActionName#OK}) on
 * a dialog or confirmation prompt, confirming the pending operation and continuing
 * automation execution.
 *
 * @see Cancel
 * @see org.skyve.metadata.sail.execution.Executor#executeOk
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class Ok extends AbstractAction {

	/**
	 * Executes execute.
	 * @param executor the executor
	 */
	@Override
	public void execute(@Nonnull Executor executor) {
		executor.executeOk(this);
	}
	
	/**
	 * Returns the identifier.
	 * @param context the context
	 * @return the result
	 */
	@Override
	public @Nonnull String getIdentifier(@Nonnull AutomationContext context) {
		return ImplicitActionName.OK.toString();
	}
}
