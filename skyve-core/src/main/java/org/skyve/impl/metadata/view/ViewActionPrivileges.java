package org.skyve.impl.metadata.view;

import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.Action;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Decides whether a user's privileges let a view show an action, as the renderers decide it.
 *
 * <p>The decision depends only on the user's document and action privileges; the view's visible and enabled
 * conditions are not considered. The renderers ({@code ViewRenderer}) and the SAIL generator both use it, so that they
 * cannot drift apart.
 *
 * <p>Threading: stateless and thread-safe.
 *
 * @since 10.0
 */
public final class ViewActionPrivileges {
	private ViewActionPrivileges() {
		// static only
	}

	/**
	 * Decides whether the user's privileges let a view of {@code document} show an action:
	 * <ul>
	 * <li>a custom action, or a BizExport, BizImport, Download or Upload action, when the user can execute its class
	 * ({@link User#canExecuteAction(Document, String)} with the action's resource name);</li>
	 * <li>Add and New when the user can create the document;</li>
	 * <li>Delete and Remove when the user can delete it;</li>
	 * <li>Edit when the user can read it;</li>
	 * <li>OK and Save when the user can create or update it;</li>
	 * <li>any other built-in action (such as Cancel, ZoomOut, Report or Print) always.</li>
	 * </ul>
	 *
	 * @param user the user the view is shown to; must not be {@code null}
	 * @param document the document whose view shows the action; must not be {@code null}
	 * @param implicitName the built-in action that the action stands for, or {@code null} for a custom action
	 * @param action the action metadata; must not be {@code null}
	 * @return {@code true} if the user's privileges let the view show the action
	 * @throws MetaDataException if a custom or class-based action has no class (resource) name
	 */
	public static boolean permitted(@Nonnull User user,
										@Nonnull Document document,
										@Nullable ImplicitActionName implicitName,
										@Nonnull Action action) {
		if (implicitName == null) {
			return user.canExecuteAction(document, className(action));
		}
		return switch (implicitName) {
			case BizExport, BizImport, Download, Upload -> user.canExecuteAction(document, className(action));
			case Add, New -> user.canCreateDocument(document);
			case Delete, Remove -> user.canDeleteDocument(document);
			case Edit -> user.canReadDocument(document);
			case OK, Save -> user.canUpdateDocument(document) || user.canCreateDocument(document);
			default -> true;
		};
	}

	/**
	 * Returns the class (resource) name of a custom or class-based action.
	 *
	 * @param action the action; must not be {@code null}
	 * @return the action's resource name
	 * @throws MetaDataException if the action has no resource name
	 */
	private static @Nonnull String className(@Nonnull Action action) {
		String result = action.getResourceName();
		if (result == null) {
			throw new MetaDataException("Action " + action.getName() + " has no class name");
		}
		return result;
	}
}
