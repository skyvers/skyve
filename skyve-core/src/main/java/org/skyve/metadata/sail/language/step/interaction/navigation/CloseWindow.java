package org.skyve.metadata.sail.language.step.interaction.navigation;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

import jakarta.annotation.Nonnull;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Closes the window that a {@code <navigateLink window="new"/>} opened, and returns to the window that opened it, for
 * example {@code <closeWindow/>}. The step has no attributes.
 *
 * <p>Windows nest: a step in a new window can open another, and each {@code <closeWindow/>} returns to the opener of
 * the window it closes. The contexts pushed in the closed window are dropped, and the opener's contexts are back as
 * they were when it opened the window. A window whose page is not HTML, such as a PDF, closes the same way.
 *
 * <p>In the window the automation started in, there is no window to close, so the step fails.
 *
 * @see NavigateLink#getWindow()
 * @see org.skyve.metadata.sail.execution.Executor#executeCloseWindow
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class CloseWindow implements Step {

	/**
	 * Dispatches to {@link Executor#executeCloseWindow(CloseWindow)}.
	 * @param executor the executor
	 */
	@Override
	public void execute(@Nonnull Executor executor) {
		executor.executeCloseWindow(this);
	}

	/**
	 * Returns {@code "CloseWindow"}.
	 * @param context the context
	 * @return the identifier
	 */
	@Override
	public @Nonnull String getIdentifier(@Nonnull AutomationContext context) {
		return "CloseWindow";
	}
}
