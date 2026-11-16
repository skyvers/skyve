package org.skyve.metadata.sail.language.step.interaction.session;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

import jakarta.annotation.Nonnull;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Switches the browser session to the renderer that the automation targets, using the
 * user-visible "Switch mode" control, for example {@code <login user="admin" password="admin"/><switchMode/>}.
 *
 * <p>The router picks the UX/UI for a session (desktop browsers get PrimeFaces by default), so a
 * script may land on a renderer other than the one its {@code uxui} targets. This step has no
 * attributes. The target is derived from the automation's {@code uxui}:
 * <ul>
 *   <li>{@code desktop} ({@link org.skyve.metadata.router.UxUi#DESKTOP_NAME}) targets SmartClient;</li>
 *   <li>any other UX/UI targets PrimeFaces.</li>
 * </ul>
 *
 * <p>The executor clicks the control so that the gesture itself is tested: the PrimeFaces
 * templates' "Switch mode" menu item, or the SmartClient header menu's item, which calls
 * {@code setUxUi} and reloads the page. Login does not switch implicitly.
 *
 * <ul>
 *   <li>If the session already shows the target renderer, the step does nothing.</li>
 *   <li>If the control is not available, because the user cannot switch modes
 *       ({@code User.canSwitchMode()} is false), the step fails with a message saying so.</li>
 * </ul>
 *
 * <p>Side effects: changes the session's UX/UI and reloads the page.
 *
 * @see Login
 * @see org.skyve.metadata.sail.execution.Executor#executeSwitchMode
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class SwitchMode implements Step {

	/**
	 * Dispatches to {@link Executor#executeSwitchMode(SwitchMode)}.
	 * @param executor the executor
	 */
	@Override
	public void execute(@Nonnull Executor executor) {
		executor.executeSwitchMode(this);
	}

	/**
	 * Returns {@code "SwitchMode"}.
	 * @param context the context
	 * @return the identifier
	 */
	@Override
	public @Nonnull String getIdentifier(@Nonnull AutomationContext context) {
		return "SwitchMode";
	}
}
