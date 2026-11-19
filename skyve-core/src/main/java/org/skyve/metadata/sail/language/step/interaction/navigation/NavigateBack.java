package org.skyve.metadata.sail.language.step.interaction.navigation;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

import jakarta.annotation.Nonnull;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Goes back to the previous page in the browser's history, as the browser's Back button does, for example
 * {@code <navigateBack/>}. The step has no attributes.
 *
 * <p>The executor waits for the page that results to load (and, in SmartClient, to settle). A back that leaves the
 * page where it is, such as to an earlier fragment of the same page, still counts once the URL has changed. With no
 * earlier page in the window's history, the step fails.
 *
 * <p>The executor cannot know which view the earlier page shows, so the top context, if any, loses its document and
 * view type, as after {@link NavigateLink}. Push a context afterwards if later steps need one.
 *
 * @see Refresh
 * @see org.skyve.metadata.sail.execution.Executor#executeNavigateBack
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class NavigateBack implements Step {

	/**
	 * Dispatches to {@link Executor#executeNavigateBack(NavigateBack)}.
	 * @param executor the executor
	 */
	@Override
	public void execute(@Nonnull Executor executor) {
		executor.executeNavigateBack(this);
	}

	/**
	 * Returns {@code "NavigateBack"}.
	 * @param context the context
	 * @return the identifier
	 */
	@Override
	public @Nonnull String getIdentifier(@Nonnull AutomationContext context) {
		return "NavigateBack";
	}
}
