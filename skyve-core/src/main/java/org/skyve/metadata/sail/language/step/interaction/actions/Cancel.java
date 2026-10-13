package org.skyve.metadata.sail.language.step.interaction.actions;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.sail.execution.Executor;

import jakarta.annotation.Nonnull;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Clicks the Cancel button ({@link org.skyve.metadata.controller.ImplicitActionName#Cancel})
 * on the current edit view, discarding any unsaved changes and returning to the
 * enclosing context (typically a list view).
 *
 * @see Save
 * @see Delete
 * @see org.skyve.metadata.sail.execution.Executor#executeCancel
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class Cancel extends AbstractAction {

	/**
	 * Executes execute.
	 * @param executor the executor
	 */
	@Override
	public void execute(@Nonnull Executor executor) {
		executor.executeCancel(this);
	}
	
	/**
	 * Returns the identifier.
	 * @param context the context
	 * @return the result
	 */
	@Override
	public @Nonnull String getIdentifier(@Nonnull AutomationContext context) {
		return ImplicitActionName.Cancel.toString();
	}
}
