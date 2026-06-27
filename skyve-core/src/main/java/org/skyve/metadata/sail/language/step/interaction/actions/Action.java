package org.skyve.metadata.sail.language.step.interaction.actions;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Executes a named custom action — such as BizImport, BizExport, Upload, Download,
 * Report, Server, or any other declared action — on the current edit view.
 *
 * <p>If the action produces a confirmation dialog, set {@code confirm} to {@code true}
 * so the executor clicks through the OK/Yes/Continue button automatically.
 *
 * @see AbstractAction
 * @see org.skyve.metadata.sail.execution.Executor#executeAction
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class Action extends AbstractAction {

	private String actionName;
	private Boolean confirm; // Need to press OK/Yes/Continue on the confirmation dialog

	/**
	 * Returns the actionName.
	 * @return the result
	 */
	public String getActionName() {
		return actionName;
	}

	/**
	 * Sets the actionName.
	 * @param actionName the actionName
	 */
	@XmlAttribute(name = "name", required = true)
	public void setActionName(String actionName) {
		this.actionName = UtilImpl.processStringValue(actionName);
	}

	/**
	 * Returns the confirm.
	 * @return the result
	 */
	public Boolean getConfirm() {
		return confirm;
	}

	/**
	 * Sets the confirm.
	 * @param confirm the confirm
	 */
	@XmlAttribute(name = "confirm")
	public void setConfirm(Boolean confirm) {
		this.confirm = confirm;
	}

	/**
	 * Executes execute.
	 * @param executor the executor
	 */
	@Override
	public void execute(Executor executor) {
		executor.executeAction(this);
	}

	/**
	 * Returns the identifier.
	 * @param context the context
	 * @return the result
	 */
	@Override
	public String getIdentifier(AutomationContext context) {
		return actionName;
	}
}
