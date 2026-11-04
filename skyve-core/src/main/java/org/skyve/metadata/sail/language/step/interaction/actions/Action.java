package org.skyve.metadata.sail.language.step.interaction.actions;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Executes a named custom action — such as BizImport, BizExport, Upload, Download,
 * Report, Server, or any other declared action — on the current edit view.
 *
 * <p>If the action produces a confirmation dialog or server-side question, it is answered
 * positively (OK/Yes/Continue) when {@code confirm} is {@code true} or absent, and negatively
 * (No/Cancel) when {@code confirm} is {@code false}; see {@link #setConfirm(Boolean)}.
 *
 * @see AbstractAction
 * @see org.skyve.metadata.sail.execution.Executor#executeAction
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class Action extends AbstractAction {

	private String actionName;
	private Boolean confirm; // null/true answers the confirmation positively, false negatively

	/**
	 * Returns the actionName.
	 * @return the result
	 */
	public @Nonnull String getActionName() {
		return actionName;
	}

	/**
	 * Sets the actionName.
	 * @param actionName the actionName
	 */
	@XmlAttribute(name = "name", required = true)
	public void setActionName(@Nonnull String actionName) {
		this.actionName = UtilImpl.processStringValue(actionName);
	}

	/**
	 * Returns how to answer the confirmation that this action can show.
	 * @return {@code true} or {@code null} to answer positively, {@code false} to answer negatively
	 */
	public @Nullable Boolean getConfirm() {
		return confirm;
	}

	/**
	 * Sets how to answer a confirmation dialog or server-side question shown after the action's button is pressed.
	 * <ul>
	 *   <li>{@code true} or absent: answer positively (Yes/OK/Continue) if a confirmation is shown.</li>
	 *   <li>{@code false}: answer negatively (No/Cancel); the step fails if no confirmation is shown.</li>
	 * </ul>
	 * Executors locate the positive and negative buttons by role, not by caption.
	 * @param confirm {@code true} or {@code null} to answer positively, {@code false} to answer negatively
	 */
	@XmlAttribute(name = "confirm")
	public void setConfirm(@Nullable Boolean confirm) {
		this.confirm = confirm;
	}

	/**
	 * Executes execute.
	 * @param executor the executor
	 */
	@Override
	public void execute(@Nonnull Executor executor) {
		executor.executeAction(this);
	}

	/**
	 * Returns the identifier.
	 * @param context the context
	 * @return the result
	 */
	@Override
	public @Nonnull String getIdentifier(@Nonnull AutomationContext context) {
		return actionName;
	}
}
