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
 * Clicks the Remove button ({@link org.skyve.metadata.controller.ImplicitActionName#Remove})
 * to detach the currently selected row from a collection without permanently deleting
 * the underlying record.
 *
 * <p>Set {@code confirm="false"} to answer the resulting confirmation negatively; see {@link #setConfirm(Boolean)}.
 *
 * @see Delete
 * @see ZoomIn
 * @see org.skyve.metadata.sail.execution.Executor#executeRemove
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class Remove extends AbstractAction {

	private Boolean confirm;

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
		executor.executeRemove(this);
	}
	
	/**
	 * Returns the identifier.
	 * @param context the context
	 * @return the result
	 */
	@Override
	public @Nonnull String getIdentifier(@Nonnull AutomationContext context) {
		return ImplicitActionName.Remove.toString();
	}
}
