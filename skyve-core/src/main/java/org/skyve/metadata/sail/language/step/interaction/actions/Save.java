package org.skyve.metadata.sail.language.step.interaction.actions;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.sail.execution.Executor;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Clicks the Save button ({@link org.skyve.metadata.controller.ImplicitActionName#Save})
 * on the current edit view, persisting the bean's current state to the database.
 *
 * <p>Set {@code createView} to {@code true} when saving a newly created record
 * whose edit view opens in "create" mode so that the executor handles the correct
 * view variant.
 *
 * @see Cancel
 * @see Delete
 * @see org.skyve.metadata.sail.execution.Executor#executeSave
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class Save extends AbstractAction {

	private Boolean createView;

	/**
	 * Returns the createView.
	 * @return the result
	 */
	public Boolean getCreateView() {
		return createView;
	}
	
	/**
	 * Sets the createView.
	 * @param createView the createView
	 */
	@XmlAttribute(name = "createView")
	public void setCreateView(Boolean createView) {
		this.createView = createView;
	}

	/**
	 * Executes execute.
	 * @param executor the executor
	 */
	@Override
	public void execute(Executor executor) {
		executor.executeSave(this);
	}
	
	/**
	 * Returns the identifier.
	 * @param context the context
	 * @return the result
	 */
	@Override
	public String getIdentifier(AutomationContext context) {
		return ImplicitActionName.Save.toString();
	}
}
