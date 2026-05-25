package org.skyve.metadata.view;

import java.util.Collection;
import java.util.List;

import org.skyve.impl.metadata.view.container.Sidebar;
import org.skyve.impl.metadata.view.model.ModelMetaData;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.NamedMetaData;
import org.skyve.metadata.PersistentMetaData;
import org.skyve.metadata.ReloadableMetaData;
import org.skyve.metadata.SerializableMetaData;
import org.skyve.util.Util;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Represents a Skyve view declaration: the edit/create/list/pick/params screen
 * rendered for a document.
 *
 * <p>A view is the top-level container of a rendered screen in Skyve. It carries
 * metadata about the layout (widgets, actions, sidebar), auto-refresh behaviour,
 * display icons, help resources, and optional view-level parameters.
 *
 * <p>Views are declared in XML metadata under a document and loaded by the metadata
 * repository. Customer and UX/UI overrides are supported: a view may be overridden
 * per customer or per UX/UI name, and the override provenance is exposed via
 * {@link #getOverriddenCustomerName()} and {@link #getOverriddenUxUiName()}.
 *
 * <p>Not thread-safe; instances are reloaded when metadata is refreshed
 * ({@link org.skyve.metadata.ReloadableMetaData}).
 *
 * @see Action
 * @see ViewType
 */
public interface View extends NamedMetaData, PersistentMetaData, ReloadableMetaData, DecoratedMetaData {
	/**
	 * The type of a Skyve view, which determines the context in which it is rendered.
	 */
	@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
	public static enum ViewType {
		/** A tabular list view of document instances, driven by a query. */
		list,
		
		/** An edit view shown when creating a new document instance. */
		create,
		
		/** An edit view shown when editing an existing document instance. */
		edit, 
		
		/** A pick/lookup view used to select an existing document instance. */
		pick,
		
		/** A parameters input view used to collect input before executing an action. */
		params
	}

	/**
	 * A binding-to-binding mapping used to pass values from the parent context into
	 * a new document instance when the view is opened via a parameterised URL.
	 *
	 * <p>Each {@code ViewParameter} specifies a source binding in the current view's
	 * bean ({@link #getFromBinding()}) and a destination binding in the new bean
	 * ({@link #getBoundTo()}).
	 */
	@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
	public static class ViewParameter implements SerializableMetaData {
		private static final long serialVersionUID = 2845518081930588156L;

		private String fromBinding;
		private String boundTo;
		
		public String getFromBinding() {
			return fromBinding;
		}
		@XmlAttribute(required = true)
		public void setFromBinding(String fromBinding) {
			this.fromBinding = UtilImpl.processStringValue(fromBinding);
		}
		
		public String getBoundTo() {
			return boundTo;
		}
		@XmlAttribute(required = true)
		public void setBoundTo(String boundTo) {
			this.boundTo = UtilImpl.processStringValue(boundTo);
		}
	}
	
	/**
	 * Returns the name of the document condition that triggers an auto-refresh of
	 * this view when it evaluates to {@code true}.
	 *
	 * @return the refresh condition name, or {@code null} if not set
	 */
	public String getRefreshConditionName();
	
	/**
	 * Returns the polling interval (in seconds) used to auto-refresh this view.
	 *
	 * @return the refresh period in seconds, or {@code null} if auto-refresh is disabled
	 */
	public Integer getRefreshTimeInSeconds();
	
	/**
	 * Returns the name of the server-side action executed when the view auto-refreshes.
	 *
	 * @return the refresh action name, or {@code null} to refresh the view without an action
	 */
	public String getRefreshActionName();
	
	/**
	 * Returns the view title shown in the browser tab and view header.
	 *
	 * <p>For a localised version use {@link #getLocalisedTitle()}.
	 *
	 * @return the view title; may be {@code null}
	 */
	public String getTitle();

	/**
	 * Returns the localised view title for the current user locale.
	 *
	 * @return a non-{@code null} localised title
	 */
	public default String getLocalisedTitle() {
		return Util.i18n(getTitle());
	}
	
	/**
	 * Returns the context-relative path to the 32-pixel icon for this view.
	 *
	 * @return the relative icon path, or {@code null} if none
	 */
	public String getIcon32x32RelativeFileName();
	
	/**
	 * Returns the CSS class used to render the view icon (e.g. a Font Awesome class).
	 *
	 * @return the icon CSS class, or {@code null} if none
	 */
	public String getIconStyleClass();
	
	/**
	 * Returns the context-relative path to the help HTML file for this view.
	 *
	 * @return the relative path to the help file, or {@code null} if none
	 */
	public String getHelpRelativeFileName();

	/**
	 * Returns the absolute URL to the external help page for this view.
	 *
	 * @return the help URL, or {@code null} if none
	 */
	public String getHelpURL();

	/**
	 * Returns the sidebar configuration for this view, or {@code null} if no sidebar
	 * is declared.
	 *
	 * @return the sidebar metadata, or {@code null}
	 */
	public Sidebar getSidebar();
	
	/**
	 * The widgetId of the actions panel for the renderer to point at if needed in isolation.
	 * 
	 * @return
	 */
	public String getActionsWidgetId();

	/**
	 * Returns the action declared on this view with the given name, or {@code null}
	 * if no such action exists.
	 *
	 * @param actionName  the action name to look up; must not be {@code null}
	 * @return the action, or {@code null}
	 */
	public Action getAction(String actionName);
	
	/**
	 * Returns all actions declared on this view.
	 *
	 * @return a non-{@code null} collection of actions; may be empty
	 */
	public Collection<Action> getActions();

	/**
	 * Returns the named inline model metadata (e.g. a chart model) embedded within
	 * this view, or {@code null} if no such model exists.
	 *
	 * @param modelName  the model name to look up; must not be {@code null}
	 * @return the model metadata, or {@code null}
	 */
	public ModelMetaData getInlineModel(String modelName);
	
	/**
	 * Returns the list of view parameters that may be populated when creating a new
	 * record via a parameterised URL.
	 *
	 * <p>Each {@link ViewParameter} maps a URL parameter binding to a field in the
	 * newly created bean.
	 *
	 * @return a non-{@code null} list of view parameters; may be empty
	 */
	public List<ViewParameter> getParameters();
	
	/**
	 * Returns optional long-form documentation for this view.
	 *
	 * @return the documentation string, or {@code null} if none
	 */
	public String getDocumentation();
	
	/**
	 * The customer name for this overridden view, or null if not overridden.
	 * @return The overridden customer name or null. 
	 */
	public String getOverriddenCustomerName();
	
	/**
	 * The ux/ui name for this overridden view, or null if not overridden.
	 * @return The overridden ux/ui name or null. 
	 */
	public String getOverriddenUxUiName();
}
