package org.skyve.metadata.view;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.NamedMetaData;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.util.Util;

import jakarta.xml.bind.annotation.XmlType;

/**
 * Represents an action that can be triggered from a Skyve view (edit view toolbar,
 * action panel, or list row).
 *
 * <p>An action resolves to either an implicit framework action (e.g. Save, Delete,
 * Cancel) identified by its {@link ImplicitActionName}, or a custom
 * {@link ServerSideAction} implementation identified by its resource name. The two
 * are mutually exclusive; use {@link #getImplicitName()} to test first.
 *
 * <p>Actions support conditional visibility ({@link Invisible}) and disable state
 * ({@link Disableable}), allowing them to be hidden or grayed out based on named
 * document conditions.
 *
 * @see ServerSideAction
 * @see ImplicitActionName
 * @see Disableable
 * @see Invisible
 */
public interface Action extends NamedMetaData, Disableable, Invisible, Parameterizable, DecoratedMetaData {
	/**
	 * Defines how an action button displays its label and icon.
	 */
	@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
	public enum ActionShow {
		/**
		 * Show icon only
		 */
		icon,
		
		/**
		 * Show text only
		 */
		text,
		
		/**
		 * Show icon and text
		 */
		both;
	}

	/**
	 * Returns the implicit (built-in) action name if this action is a framework action,
	 * or {@code null} if this is a custom server-side action.
	 *
	 * @return the implicit action name, or {@code null}
	 */
	public ImplicitActionName getImplicitName();
	
	/**
	 * Returns the resource name (simple class name or fully-qualified name) of the
	 * custom {@link ServerSideAction} implementation, or {@code null} for implicit actions.
	 *
	 * @return the action class resource name, or {@code null}
	 */
	public String getResourceName();
	
	/**
	 * Returns whether client-side validation runs before this action is submitted
	 * to the server.
	 *
	 * @return {@code Boolean.TRUE} to run client validation; {@code Boolean.FALSE} to
	 *         skip it; {@code null} to use the view default
	 */
	public Boolean getClientValidation();
	
	/**
	 * Returns the button label for this action.
	 *
	 * <p>For a localised version use {@link #getLocalisedDisplayName()}.
	 *
	 * @return the display name; may be {@code null} to use the implicit or resource name
	 */
	public String getDisplayName();

	/**
	 * Returns the localised button label for the current user locale.
	 *
	 * @return a non-{@code null} localised display name
	 */
	public default String getLocalisedDisplayName() {
		return Util.i18n(getDisplayName());
	}

	/**
	 * Returns the context-relative path to the 16-pixel icon shown on the action button.
	 *
	 * @return the relative icon path, or {@code null} if no icon file is specified
	 */
	public String getRelativeIconFileName();
	
	/**
	 * Returns the CSS class used to render a vector icon (e.g. Font Awesome) on the
	 * action button.
	 *
	 * @return the icon CSS class, or {@code null} if no CSS icon is specified
	 */
	public String getIconStyleClass();

	/**
	 * Returns the text of the confirmation dialog shown before executing this action.
	 *
	 * <p>When non-{@code null} a modal dialog presents this text and requires the
	 * user to confirm before the action is submitted.
	 * For a localised version use {@link #getLocalisedConfirmationText()}.
	 *
	 * @return the confirmation text, or {@code null} for no confirmation dialog
	 */
	public String getConfirmationText();
	
	/**
	 * Returns the localised confirmation dialog text for the current user locale.
	 *
	 * @return a non-{@code null} localised confirmation text
	 */
	public default String getLocalisedConfirmationText() {
		return Util.i18n(getConfirmationText());
	}
	
	/**
	 * Returns the tooltip text shown when the user hovers over the action button.
	 *
	 * <p>For a localised version use {@link #getLocalisedToolTip()}.
	 *
	 * @return the tooltip text, or {@code null} if none
	 */
	public String getToolTip();
	
	/**
	 * Returns the localised tooltip text for the current user locale.
	 *
	 * @return a non-{@code null} localised tooltip
	 */
	public default String getLocalisedToolTip() {
		return Util.i18n(getToolTip());
	}
	
	/**
	 * Returns whether this action should appear in the view's action panel.
	 *
	 * <p>{@code Boolean.FALSE} hides the action from the panel (it may still be
	 * referenced by a button widget elsewhere in the view).
	 *
	 * @return {@code Boolean.TRUE} to include in action panel; {@code Boolean.FALSE}
	 *         to exclude; {@code null} to use the view default
	 */
	public Boolean getInActionPanel();
	
	/**
	 * Returns whether to show icon only, text only, or both on the action button.
	 *
	 * @return the display mode; may be {@code null} to use the UX/UI default
	 */
	public ActionShow getShow();
	
	/**
	 * Resolves and returns the {@link ServerSideAction} implementation for this action
	 * within the given customer and document context.
	 *
	 * @param customer  the customer context for class resolution; must not be {@code null}
	 * @param document  the document this action is declared on; must not be {@code null}
	 * @return the resolved server-side action implementation; never {@code null}
	 */
	public ServerSideAction<?> getServerSideAction(Customer customer, Document document);
}
