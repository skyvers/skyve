package org.skyve.metadata.sail.language.step.interaction.navigation;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

import jakarta.annotation.Nonnull;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Reloads the page, as the browser's Reload button does, for example {@code <refresh/>}. The step has no attributes.
 * Values entered and not saved are lost, as they are for a user.
 *
 * <p>The executor waits for the reloaded page to load (and, in SmartClient, to settle).
 *
 * <p>The contexts are unchanged, because the browser reloads the same URL. In SmartClient that URL is the one the
 * page was loaded with (by a navigate step, a link or a sign in), not the views opened in the page since then by
 * menus, zooms or New, so after those the reloaded page shows the URL's view: pop or push contexts to match it.
 *
 * @see NavigateBack
 * @see org.skyve.metadata.sail.execution.Executor#executeRefresh
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class Refresh implements Step {

	/**
	 * Dispatches to {@link Executor#executeRefresh(Refresh)}.
	 * @param executor the executor
	 */
	@Override
	public void execute(@Nonnull Executor executor) {
		executor.executeRefresh(this);
	}

	/**
	 * Returns {@code "Refresh"}.
	 * @param context the context
	 * @return the identifier
	 */
	@Override
	public @Nonnull String getIdentifier(@Nonnull AutomationContext context) {
		return "Refresh";
	}
}
