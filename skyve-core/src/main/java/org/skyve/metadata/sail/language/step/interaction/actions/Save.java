package org.skyve.metadata.sail.language.step.interaction.actions;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.sail.execution.Executor;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
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
 * <p>Set {@code confirm="false"} to answer the resulting confirmation negatively; see {@link #setConfirm(Boolean)}.
 *
 * @see Cancel
 * @see Delete
 * @see org.skyve.metadata.sail.execution.Executor#executeSave
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class Save extends AbstractAction {

	private Boolean createView;
	private Boolean confirm;

	/**
	 * Returns the createView.
	 * @return the result
	 */
	public @Nullable Boolean getCreateView() {
		return createView;
	}
	
	/**
	 * Sets the createView.
	 * @param createView the createView
	 */
	@XmlAttribute(name = "createView")
	public void setCreateView(@Nullable Boolean createView) {
		this.createView = createView;
	}

	/**
	 * Returns how to answer the confirmation that this step can show.
	 * @return {@code true} or {@code null} to answer positively, {@code false} to answer negatively
	 */
	public @Nullable Boolean getConfirm() {
		return confirm;
	}

	/**
	 * Sets how to answer a confirmation dialog or server-side question shown after the button is pressed.
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
		executor.executeSave(this);
	}
	
	/**
	 * Returns the identifier.
	 * @param context the context
	 * @return the result
	 */
	@Override
	public @Nonnull String getIdentifier(@Nonnull AutomationContext context) {
		return ImplicitActionName.Save.toString();
	}
}
