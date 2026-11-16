package org.skyve.impl.web.faces.pipeline.component;

import java.util.List;
import java.util.Map;

import org.primefaces.behavior.ajax.AjaxBehavior;
import org.primefaces.behavior.ajax.AjaxBehaviorListenerImpl;
import org.primefaces.component.outputlabel.OutputLabel;
import org.skyve.domain.Bean;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.container.Collapsible;
import org.skyve.impl.metadata.view.container.Sidebar;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.event.EventAction;
import org.skyve.impl.metadata.view.event.RerenderEventAction;
import org.skyve.impl.metadata.view.event.ServerSideActionEventAction;
import org.skyve.impl.metadata.view.reference.ReferenceTarget;
import org.skyve.impl.metadata.view.reference.ReferenceTarget.ReferenceTargetType;
import org.skyve.impl.metadata.view.widget.Blurb;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.impl.metadata.view.widget.Chart;
import org.skyve.impl.metadata.view.widget.DialogButton;
import org.skyve.impl.metadata.view.widget.DynamicImage;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.widget.MapDisplay;
import org.skyve.impl.metadata.view.widget.Spacer;
import org.skyve.impl.metadata.view.widget.StaticImage;
import org.skyve.impl.metadata.view.widget.bound.Label;
import org.skyve.impl.metadata.view.widget.bound.ZoomIn;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;
import org.skyve.impl.metadata.view.widget.bound.input.ContentUpload;
import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.GeometryMap;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.ListMembership;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.Password;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.impl.metadata.view.widget.bound.input.Slider;
import org.skyve.impl.metadata.view.widget.bound.input.Spinner;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.widget.bound.tabular.AbstractDataWidget;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridContainerColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.web.faces.pipeline.AbstractFacesBuilder;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.query.QueryDefinition;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.TextOutput.Sanitisation;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.widget.FilterParameter;
import org.skyve.metadata.view.widget.bound.Parameter;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.el.MethodExpression;
import jakarta.faces.component.UICommand;
import jakarta.faces.component.UIComponent;
import jakarta.faces.component.UIComponentBase;
import jakarta.faces.component.html.HtmlOutputLink;
import jakarta.faces.component.html.HtmlOutputText;

/**
 * Defines the component-stage contract that maps Skyve metadata widgets to JSF UI components.
 */
@SuppressWarnings("java:S1192") // Repeated literals are deliberate JSF component binding/script fragments.
public abstract class ComponentBuilder extends AbstractFacesBuilder {
	public static final String ACTION_BAR_STYLE_CLASS = "skyve-action-bar action-bar";
	public static final String COLLECTION_BINDING_ATTRIBUTE_KEY = "collectionBinding";
	public static final String COMPONENT_BUILDER_CLASS_KEY = "componentBuilderClass";

	private boolean currentInputTitleEscape = true;

	public static class EventSourceComponent {
		private UIComponent component;
		private UIComponentBase eventSource;
		
		/**
		 * Creates an event-source wrapper pairing the visible component and its event source.
		 *
		 * @param component the rendered component
		 * @param eventSource the component that receives JSF client behaviors
		 */
		public EventSourceComponent(@Nonnull UIComponent component, @Nonnull UIComponentBase eventSource) {
			super();
			this.component = component;
			this.eventSource = eventSource;
		}

		/**
		 * Returns the rendered component.
		 *
		 * @return the rendered component
		 */
		public @Nonnull UIComponent getComponent() {
			return component;
		}

		/**
		 * Returns the component that receives event behaviors.
		 *
		 * @return the event source component
		 */
		public @Nonnull UIComponentBase getEventSource() {
			return eventSource;
		}
	}
	
	/**
	 * Creates the root view component.
	 *
	 * @param component the current component
	 * @param createView whether create-view mode is active
	 * @return the resulting view component
	 */
	public abstract @Nullable UIComponent view(@Nullable UIComponent component, boolean createView);

	/**
	 * Creates toolbar components for the current view.
	 *
	 * @param components existing toolbar components
	 * @param widgetId the optional widget identifier
	 * @return the resulting toolbar components, or {@code null} when no toolbar is required
	 */
	public abstract @Nullable List<UIComponent> toolbars(@Nullable List<UIComponent> components, @Nullable String widgetId);
	
	/**
	 * Creates a tab pane component.
	 *
	 * @param component the current component
	 * @param tabPane the tab pane metadata
	 * @param moduleName the module name
	 * @param documentName the document name
	 * @return the resulting tab-pane component
	 */
	public abstract @Nullable UIComponent tabPane(@Nullable UIComponent component,
														@Nonnull TabPane tabPane,
														@Nonnull String moduleName,
														@Nonnull String documentName);
	
	/**
	 * Creates a single tab component.
	 *
	 * @param component the current component
	 * @param title the tab title
	 * @param tab the tab metadata
	 * @return the resulting tab component
	 */
	public abstract @Nullable UIComponent tab(@Nullable UIComponent component, @Nullable String title, @Nonnull Tab tab);

	/**
	 * Sets the resolved escape decision for subsequently rendered input title attributes.
	 *
	 * @param currentInputTitleEscape {@code true} to escape at the component
	 *        boundary; {@code false} to allow trusted markup
	 */
	public void setCurrentInputTitleEscape(boolean currentInputTitleEscape) {
		this.currentInputTitleEscape = currentInputTitleEscape;
	}

	/**
	 * Returns the resolved escape decision for input title attributes.
	 *
	 * @return {@code true} to escape at the component boundary; {@code false} to
	 *         allow trusted markup
	 */
	protected boolean getCurrentInputTitleEscape() {
		return currentInputTitleEscape;
	}

	/**
	 * Creates an output-text component for metadata text that escapes by default.
	 *
	 * <p>Side effects: creates a JSF component through the current
	 * {@link jakarta.faces.application.Application} and assigns it a generated Skyve
	 * component ID.
	 *
	 * @param text raw metadata text and nullable escape flag; must not be {@code null}
	 * @return configured output-text component; never {@code null}
	 */
	protected @Nonnull HtmlOutputText outputText(@Nonnull EscapableText text) {
		HtmlOutputText result = EscapableComponentSupport.outputText(a, text);
		setId(result, null);
		return result;
	}

	/**
	 * Creates an output-label component for metadata text that escapes by default.
	 *
	 * <p>Side effects: creates a PrimeFaces component through the current
	 * {@link jakarta.faces.application.Application} and assigns it a generated Skyve
	 * component ID.
	 *
	 * @param text raw metadata label and nullable escape flag; must not be {@code null}
	 * @param forId optional target component ID for the label
	 * @return configured output-label component; never {@code null}
	 */
	protected @Nonnull OutputLabel outputLabel(@Nonnull EscapableText text, @Nullable String forId) {
		OutputLabel result = EscapableComponentSupport.outputLabel(a, text, forId);
		setId(result, null);
		return result;
	}

	/**
	 * Adds an output-text facet for metadata text when a value is present.
	 *
	 * <p>Side effects: creates a JSF component through the current
	 * {@link jakarta.faces.application.Application}, assigns it a generated Skyve
	 * component ID, and mutates {@code component.getFacets()} by adding or replacing
	 * {@code facetName}. No facet is added when {@code text} or its value is {@code null}.
	 *
	 * @param component component receiving the facet; must not be {@code null}
	 * @param facetName facet key to add or replace; must not be {@code null}
	 * @param text raw metadata text and nullable escape flag; may be {@code null}
	 * @return the added output-text facet, or {@code null} when no text value is present
	 */
	protected @Nullable HtmlOutputText putOutputTextFacet(@Nonnull UIComponent component, @Nonnull String facetName, @Nullable EscapableText text) {
		HtmlOutputText result = EscapableComponentSupport.putOutputTextFacet(a, component, facetName, text);
		if (result != null) {
			setId(result, null);
		}
		return result;
	}

	/**
	 * Adds a click confirmation behaviour for metadata text when a value is present.
	 *
	 * <p>Side effects: creates a PrimeFaces behaviour through the current
	 * {@link jakarta.faces.application.Application} and mutates {@code component} by
	 * attaching the behaviour to the {@code click} event. No behaviour is added when
	 * {@code confirmation} or its value is {@code null}.
	 *
	 * @param component component receiving the confirmation behaviour; must not be {@code null}
	 * @param confirmation raw confirmation text and nullable escape flag; may be {@code null}
	 */
	protected void addConfirmBehavior(@Nonnull UIComponentBase component, @Nullable EscapableText confirmation) {
		EscapableComponentSupport.addConfirmBehavior(a, component, confirmation);
	}
	
	/**
	 * Creates tab-pane script support markup.
	 *
	 * @param component the current component
	 * @param tabPane the tab pane metadata
	 * @param moduleName the module name
	 * @param documentName the document name
	 * @param tabPaneComponentId the tab-pane component id
	 * @return the resulting script component
	 */
	public abstract @Nullable UIComponent tabPaneScript(@Nullable UIComponent component,
															@Nonnull TabPane tabPane,
															@Nonnull String moduleName,
															@Nonnull String documentName,
															@Nonnull String tabPaneComponentId);

	/**
	 * Creates sidebar script support markup.
	 *
	 * @param component the current component
	 * @param sidebar the sidebar metadata
	 * @param createView whether create-view mode is active
	 * @param sidebarComponentId the sidebar component id
	 * @return the resulting script component
	 */
	public abstract @Nullable UIComponent sidebarScript(@Nullable UIComponent component,
															@Nonnull Sidebar sidebar,
															boolean createView,
															@Nonnull String sidebarComponentId);
	
	/**
	 * Creates a bordered container.
	 *
	 * @param component the current component
	 * @param title the border title
	 * @param invisibleCondition invisible condition name
	 * @param pixelWidth optional pixel width
	 * @param collapsible collapsible metadata
	 * @return the resulting border component
	 */
	public abstract @Nullable UIComponent border(@Nullable UIComponent component,
													@Nullable String title,
													@Nullable String invisibleCondition,
													@Nullable Integer pixelWidth,
													@Nullable Collapsible collapsible);

	/**
	 * Creates a static label component.
	 *
	 * @param component the current component
	 * @param value the label text value
	 * @return the resulting label component
	 */
	public abstract @Nullable UIComponent label(@Nullable UIComponent component, @Nonnull String value);

	/**
	 * Creates a dialog-button component.
	 *
	 * @param component the current component
	 * @param label raw button label and nullable escape flag
	 * @param button dialog-button metadata
	 * @param formDisabledCondition optional form-level disabled condition
	 * @return the resulting dialog-button component
	 */
	public abstract @Nullable UIComponent dialogButton(@Nullable UIComponent component,
															@Nullable EscapableText label,
															@Nonnull DialogButton button,
															@Nullable String formDisabledCondition);
	
	/**
	 * Creates a spacer component.
	 *
	 * @param component the current component
	 * @param spacer the spacer metadata
	 * @return the resulting spacer component
	 */
	public abstract @Nullable UIComponent spacer(@Nullable UIComponent component, @Nonnull Spacer spacer);
	
	/**
	 * Creates a zoom-in control component.
	 *
	 * @param component the current component
	 * @param label label text
	 * @param iconStyleClass icon style class
	 * @param toolTip tooltip text
	 * @param zoomIn zoom-in metadata
	 * @param formDisabledCondition form disabled condition
	 * @return the resulting component
	 */
	public abstract @Nullable UIComponent zoomIn(@Nullable UIComponent component,
														@Nullable String label,
														@Nullable String iconStyleClass,
														@Nullable String toolTip,
														@Nonnull ZoomIn zoomIn, 
														@Nullable String formDisabledCondition);

	/**
	 * Creates a generic action button component.
	 *
	 * @param component the current component
	 * @param dataWidgetBinding data widget binding
	 * @param dataWidgetVar data widget variable
	 * @param label label text
	 * @param iconStyleClass icon style class
	 * @param toolTip tooltip text
	 * @param confirmationText confirmation text
	 * @param button button metadata
	 * @param formDisabledCondition form disabled condition
	 * @param action action metadata
	 * @return the resulting component
	 */
	@SuppressWarnings("java:S107") // Long parameter list preserves the existing framework/API contract.
	public abstract @Nullable UIComponent actionButton(@Nullable UIComponent component,
															@Nullable String dataWidgetBinding, 
															@Nullable String dataWidgetVar, 
															@Nullable String label,
															@Nullable String iconStyleClass,
															@Nullable String toolTip,
															@Nullable String confirmationText, 
															@Nonnull Button button, 
															@Nullable String formDisabledCondition,
															@Nonnull Action action);

	/**
	 * Creates a report button component.
	 *
	 * @param component the current component
	 * @param label label text
	 * @param iconStyleClass icon style class
	 * @param toolTip tooltip text
	 * @param confirmationText confirmation text
	 * @param button button metadata
	 * @param formDisabledCondition form disabled condition
	 * @param action action metadata
	 * @return the resulting component
	 */
	@SuppressWarnings("java:S107") // Long parameter list preserves the existing framework/API contract.
	public abstract @Nullable UIComponent reportButton(@Nullable UIComponent component, 
															@Nullable String label,
															@Nullable String iconStyleClass,
															@Nullable String toolTip,
															@Nullable String confirmationText, 
															@Nonnull Button button, 
															@Nullable String formDisabledCondition,
															@Nonnull Action action);
												
	/**
	 * Creates a download button component.
	 *
	 * @param component the current component
	 * @param dataWidgetBinding data widget binding
	 * @param dataWidgetVar data widget variable
	 * @param label label text
	 * @param iconStyleClass icon style class
	 * @param toolTip tooltip text
	 * @param confirmationText confirmation text
	 * @param button button metadata
	 * @param formDisabledCondition form disabled condition
	 * @param action action metadata
	 * @return the resulting component
	 */
	@SuppressWarnings("java:S107") // Long parameter list preserves the existing framework/API contract.
	public abstract @Nullable UIComponent downloadButton(@Nullable UIComponent component,
															@Nullable String dataWidgetBinding, 
															@Nullable String dataWidgetVar, 
															@Nullable String label,
															@Nullable String iconStyleClass,
															@Nullable String toolTip,
															@Nullable String confirmationText, 
															@Nonnull Button button, 
															@Nullable String formDisabledCondition,
															@Nonnull Action action);

	/**
	 * Creates an upload button component.
	 *
	 * @param component the current component
	 * @param label label text
	 * @param iconStyleClass icon style class
	 * @param toolTip tooltip text
	 * @param confirmationText confirmation text
	 * @param button button metadata
	 * @param formDisabledCondition form disabled condition
	 * @param action action metadata
	 * @return the resulting component
	 */
	@SuppressWarnings("java:S107") // Long parameter list preserves the existing framework/API contract.
	public abstract @Nullable UIComponent uploadButton(@Nullable UIComponent component, 
															@Nullable String label,
															@Nullable String iconStyleClass,
															@Nullable String toolTip,
															@Nullable String confirmationText, 
															@Nonnull Button button, 
															@Nullable String formDisabledCondition,
															@Nonnull Action action);

	/**
	 * Creates a static image component.
	 *
	 * @param component the current component
	 * @param fileUrl the image URL
	 * @param image the static image metadata
	 * @return the resulting image component
	 */
	public abstract @Nullable UIComponent staticImage(@Nullable UIComponent component, @Nonnull String fileUrl, @Nonnull StaticImage image);
	
	/**
	 * Creates a dynamic image component.
	 *
	 * @param component the current component
	 * @param image the dynamic image metadata
	 * @param moduleName the module name
	 * @param documentName the document name
	 * @return the resulting image component
	 */
	public abstract @Nullable UIComponent dynamicImage(@Nullable UIComponent component, 
															@Nonnull DynamicImage image, 
															@Nonnull String moduleName, 
															@Nonnull String documentName);
	
	/**
	 * Creates a blurb component.
	 *
	 * @param component the current component
	 * @param dataWidgetVar the data-widget variable
	 * @param value the static blurb value
	 * @param binding the optional binding expression
	 * @param blurb the blurb metadata
	 * @return the resulting blurb component
	 */
	public abstract @Nullable UIComponent blurb(@Nullable UIComponent component, 
													@Nullable String dataWidgetVar, 
													@Nullable String value, 
													@Nullable String binding, 
													@Nonnull Blurb blurb);
										
	/**
	 * Creates a bound label component.
	 *
	 * @param component the current component
	 * @param dataWidgetVar the data-widget variable
	 * @param value the static label value
	 * @param binding the optional binding expression
	 * @param label the label metadata
	 * @return the resulting label component
	 */
	public abstract @Nullable UIComponent label(@Nullable UIComponent component,
													@Nullable String dataWidgetVar,
													@Nullable String value,
													@Nullable String binding,
													@Nonnull Label label);

	/**
	 * Creates a data-grid component.
	 *
	 * @param component the current component
	 * @param dataWidgetVar the data-widget variable
	 * @param ordered whether rows are ordered
	 * @param grid the data-grid metadata
	 * @return the resulting data-grid component
	 */
	public abstract @Nullable UIComponent dataGrid(@Nullable UIComponent component,
														@Nonnull String dataWidgetVar,
														boolean ordered,
														@Nonnull DataGrid grid);

	/**
	 * Creates a data repeater component.
	 *
	 * Data Repeater is just like a data grid but...
	 * The grid column headers can be turned off.
	 * The grid (borders) can be turned off.
	 * Any bound columns are editable inline.
	 * There is no action column.
	 * 
	 * @param component the current component
	 * @param dataWidgetVar the data-widget variable
	 * @param repeater the data-repeater metadata
	 * @return the resulting data-repeater component
	 */
	public abstract @Nullable UIComponent dataRepeater(@Nullable UIComponent component,
															@Nonnull String dataWidgetVar,
															@Nonnull DataRepeater repeater);

	/**
	 * Adds a bound column to a data grid.
	 *
	 * @param component the grid component
	 * @param current the current column container
	 * @param widget the parent widget metadata
	 * @param column the bound column metadata
	 * @param dataWidgetVar the data-widget variable
	 * @param columnTitle the resolved column title
	 * @param columnBinding the column binding
	 * @param gridColumnExpression expression used for cell rendering
	 * @param horizontalAlignment column alignment
	 * @param pixelWidth optional column width
	 * @return the resulting component after adding the column
	 */
	@SuppressWarnings("java:S107") // Long parameter list preserves the existing framework/API contract.
	public abstract @Nullable UIComponent addDataGridBoundColumn(@Nullable UIComponent component,
																	@Nonnull UIComponent current, 
																	@Nonnull AbstractDataWidget widget,
																	@Nonnull DataGridBoundColumn column,
																	@Nullable String dataWidgetVar,
																	@Nullable String columnTitle,
																	@Nonnull String columnBinding,
																	@Nonnull StringBuilder gridColumnExpression,
																	@Nullable HorizontalAlignment horizontalAlignment,
																	@Nullable Integer pixelWidth);

	/**
	 * Finalises a bound data-grid column after insertion.
	 *
	 * @param component the grid component
	 * @param current the current column component
	 * @param alignment alignment to apply
	 * @return the resulting component
	 */
	public abstract @Nullable UIComponent addedDataGridBoundColumn(@Nullable UIComponent component,
																		@Nonnull UIComponent current,
																		@Nonnull HorizontalAlignment alignment);

	/**
	 * Adds a container column to a data grid.
	 *
	 * @param component the grid component
	 * @param current the current column container
	 * @param widget the parent widget metadata
	 * @param title the column title
	 * @param column the container column metadata
	 * @param horizontalAlignment column alignment
	 * @return the resulting component after adding the column
	 */
	public abstract @Nullable UIComponent addDataGridContainerColumn(@Nullable UIComponent component,
																		@Nonnull UIComponent current,
																		@Nonnull AbstractDataWidget widget,
																		@Nullable String title,
																		@Nonnull DataGridContainerColumn column,
																		@Nullable HorizontalAlignment horizontalAlignment);

	/**
	 * Finalises a container data-grid column after insertion.
	 *
	 * @param component the grid component
	 * @param current the current column component
	 * @return the resulting component
	 */
	public abstract @Nullable UIComponent addedDataGridContainerColumn(@Nullable UIComponent component, @Nonnull UIComponent current);

	/**
	 * Adds an action column to a data grid.
	 *
	 * @param component the grid component
	 * @param current the current column container
	 * @param grid the data-grid metadata
	 * @param dataWidgetVar the data-widget variable
	 * @param gridColumnExpression expression used for cell rendering
	 * @param singluarDocumentAlias the singular document alias
	 * @param inline whether inline mode is enabled
	 * @param canCreate whether create is permitted
	 * @param canDelete whether delete is permitted
	 * @return the resulting component after adding the column
	 */
	@SuppressWarnings("java:S107") // Long parameter list preserves the existing framework/API contract.
	public abstract @Nullable UIComponent addDataGridActionColumn(@Nullable UIComponent component,
																		@Nonnull UIComponent current, 
																		@Nonnull DataGrid grid,
																		@Nullable String dataWidgetVar,
																		@Nonnull String gridColumnExpression,
																		@Nullable String singluarDocumentAlias,
																		boolean inline,
																		boolean canCreate,
																		boolean canDelete);
	
	/**
	 * Creates a list-grid component.
	 *
	 * @param component the current component
	 * @param moduleName the module name
	 * @param modelDocumentName the model document name
	 * @param modelName the model name
	 * @param uxui the UX/UI variant name
	 * @param model the resolved list model
	 * @param owningDocument the owning document definition
	 * @param listGrid the list-grid metadata
	 * @param stickyHeaderAnchorSelector optional CSS selector anchoring the sticky header
	 * @param aggregateQuery whether the model uses aggregate query semantics
	 * @return the resulting list-grid component
	 */
	@SuppressWarnings("java:S107") // Long parameter list preserves the existing framework/API contract.
	public abstract @Nullable UIComponent listGrid(@Nullable UIComponent component,
														@Nonnull String moduleName,
														@Nullable String modelDocumentName,
														@Nullable String modelName,
														@Nonnull String uxui,
														@Nonnull ListModel<Bean> model,
														@Nullable Document owningDocument,
														@Nonnull ListGrid listGrid,
														@Nullable String stickyHeaderAnchorSelector,
														boolean aggregateQuery);

	/**
	 * Creates a list-grid context menu component.
	 *
	 * @param component the current component
	 * @param listGridId the list-grid component id
	 * @param listGrid the list-grid metadata
	 * @return the resulting context-menu component
	 */
	public abstract @Nullable UIComponent listGridContextMenu(@Nullable UIComponent component,
																	@Nonnull String listGridId,
																	@Nonnull ListGrid listGrid);

	/**
	 * Creates a list repeater component.
	 * 
	 * List Repeater is just like a list grid but...
	 * The grid column headers can be turned off.
	 * The grid (borders) can be turned off.
	 * It implements infinite scrolling instead of the page controls.
	 * Any bound columns are editable inline.
	 * There is no action column.
	 * No CRUD.
	 *
	 * @param component the current component
	 * @param modelDocumentName the model document name
	 * @param modelName the model name
	 * @param uxui the UX/UI variant name
	 * @param model the resolved list model
	 * @param filterParameters optional filter parameters
	 * @param parameters optional model parameters
	 * @param showColumnHeaders whether column headers should be shown
	 * @param showGrid whether grid borders should be shown
	 * @return the resulting list-repeater component
	 */
	@SuppressWarnings("java:S107") // Long parameter list preserves the existing framework/API contract.
	public abstract @Nullable UIComponent listRepeater(@Nullable UIComponent component,
															@Nullable String modelDocumentName,
															@Nullable String modelName,
															@Nonnull String uxui,
															@Nonnull ListModel<Bean> model, 
															@Nullable List<FilterParameter> filterParameters,
															@Nullable List<Parameter> parameters,
															boolean showColumnHeaders,
															boolean showGrid);

	/**
	 * Creates a query-backed map component.
	 *
	 * @param component the current component
	 * @param map the map metadata
	 * @param moduleName the module name
	 * @param queryName the query name
	 * @param geometryBinding the geometry binding
	 * @return the resulting map component
	 */
	public abstract @Nullable UIComponent map(@Nullable UIComponent component, 
													@Nonnull MapDisplay map,
													@Nonnull String moduleName,
													@Nonnull String queryName,
													@Nonnull String geometryBinding);

	/**
	 * Creates a model-backed map component.
	 *
	 * @param component the current component
	 * @param map the map metadata
	 * @param modelName the model name
	 * @return the resulting map component
	 */
	public abstract @Nullable UIComponent map(@Nullable UIComponent component, @Nonnull MapDisplay map, @Nonnull String modelName);
	
	/**
	 * Creates a geometry input component wrapper.
	 *
	 * @param component the existing wrapper component
	 * @param dataWidgetVar the data-widget variable
	 * @param geometry the geometry metadata
	 * @param formDisabledCondition form disabled condition
	 * @param title field title
	 * @param requiredMessage optional required-message text
	 * @param textAlignment text alignment
	 * @return the resulting wrapper component
	 */
	public abstract @Nullable EventSourceComponent geometry(@Nullable EventSourceComponent component, 
																@Nullable String dataWidgetVar, 
																@Nonnull Geometry geometry, 
																@Nullable String formDisabledCondition,
																@Nullable String title, 
																@Nullable String requiredMessage,
																@Nullable HorizontalAlignment textAlignment);

	/**
	 * Creates a geometry-map input component wrapper.
	 *
	 * @param component the existing wrapper component
	 * @param geometry the geometry-map metadata
	 * @param formDisabledCondition form disabled condition
	 * @param title field title
	 * @param requiredMessage optional required-message text
	 * @return the resulting wrapper component
	 */
	public abstract @Nullable EventSourceComponent geometryMap(@Nullable EventSourceComponent component, 
																	@Nonnull GeometryMap geometry, 
																	@Nullable String formDisabledCondition,
																	@Nullable String title, 
																	@Nullable String requiredMessage);

	/**
	 * Creates a chart component.
	 *
	 * @param component the current component
	 * @param chart the chart metadata
	 * @return the resulting chart component
	 */
	public abstract @Nullable UIComponent chart(@Nullable UIComponent component, @Nonnull Chart chart);

	/**
	 * Creates a list-membership component wrapper.
	 *
	 * @param component the existing wrapper component
	 * @param candidatesHeading raw candidates-list heading and nullable escape flag;
	 *        {@code null} and {@code Boolean.TRUE} escape, while {@code Boolean.FALSE}
	 *        allows trusted markup
	 * @param membersHeading raw members-list heading and nullable escape flag;
	 *        {@code null} and {@code Boolean.TRUE} escape, while {@code Boolean.FALSE}
	 *        allows trusted markup
	 * @param membership the membership metadata
	 * @return the resulting wrapper component
	 */
	public abstract @Nullable EventSourceComponent listMembership(@Nullable EventSourceComponent component,
																		@Nullable EscapableText candidatesHeading,
																		@Nullable EscapableText membersHeading,
																		@Nonnull ListMembership membership);

	/**
	 * Creates a list-membership component wrapper for a membership whose collection may be ordered: a user can put an
	 * ordered membership's members in a new order. This default ignores {@code ordered} and calls
	 * {@link #listMembership(EventSourceComponent, EscapableText, EscapableText, ListMembership)}, so a builder that only
	 * implements that method still works.
	 *
	 * @param component the existing wrapper component
	 * @param candidatesHeading raw candidates-list heading and nullable escape flag, as for the other overload
	 * @param membersHeading raw members-list heading and nullable escape flag, as for the other overload
	 * @param membership the membership metadata
	 * @param ordered whether the membership's collection is ordered
	 * @return the resulting wrapper component
	 */
	public @Nullable EventSourceComponent listMembership(@Nullable EventSourceComponent component,
															@Nullable EscapableText candidatesHeading,
															@Nullable EscapableText membersHeading,
															@Nonnull ListMembership membership,
															boolean ordered) {
		return listMembership(component, candidatesHeading, membersHeading, membership);
	}
	
	/**
	 * Creates a checkbox component wrapper.
	 *
	 * @param component the existing wrapper component
	 * @param dataWidgetVar the data-widget variable
	 * @param checkBox the checkbox metadata
	 * @param formDisabledCondition form disabled condition
	 * @param title field title
	 * @param requiredMessage optional required-message text
	 * @return the resulting wrapper component
	 */
	public abstract @Nullable EventSourceComponent checkBox(@Nullable EventSourceComponent component, 
																@Nullable String dataWidgetVar, 
																@Nonnull CheckBox checkBox, 
																@Nullable String formDisabledCondition,
																@Nullable String title, 
																@Nullable String requiredMessage);

	/**
	 * Creates a colour-picker component wrapper.
	 *
	 * @param component the existing wrapper component
	 * @param dataWidgetVar the data-widget variable
	 * @param colour the colour-picker metadata
	 * @param formDisabledCondition form disabled condition
	 * @param title field title
	 * @param requiredMessage optional required-message text
	 * @param textAlignment text alignment
	 * @return the resulting wrapper component
	 */
	public abstract @Nullable EventSourceComponent colourPicker(@Nullable EventSourceComponent component, 
																	@Nullable String dataWidgetVar, 
																	@Nonnull ColourPicker colour, 
																	@Nullable String formDisabledCondition,
																	@Nullable String title, 
																	@Nullable String requiredMessage,
																	@Nullable HorizontalAlignment textAlignment);
	
	/**
	 * Creates a combo component wrapper.
	 *
	 * Note: We cannot set the text alignment of a combo easily with inline styling
	 * 
	 * @param component the existing wrapper component
	 * @param dataWidgetVar the data-widget variable
	 * @param combo the combo metadata
	 * @param formDisabledCondition form disabled condition
	 * @param title field title
	 * @param requiredMessage optional required-message text
	 * @return the resulting wrapper component
	 */
	public abstract @Nullable EventSourceComponent combo(@Nullable EventSourceComponent component, 
															@Nullable String dataWidgetVar, 
															@Nonnull Combo combo, 
															@Nullable String formDisabledCondition,
															@Nullable String title, 
															@Nullable String requiredMessage);

	/**
	 * Creates a unified content component.
	 *
	 * @param component the current component, or {@code null} when this builder must
	 *        create it
	 * @param dataWidgetVar the data-widget variable, or {@code null} outside a data
	 *        widget
	 * @param content the content metadata; must not be {@code null}
	 * @param formDisabledCondition form disabled condition, or {@code null}
	 * @param title field title, or {@code null}
	 * @param requiredMessage optional required-message text
	 * @param textAlignment text alignment, or {@code null} to use renderer defaults
	 * @param formContext whether the component is rendered in a form rather than a
	 *        grid/list column
	 * @param imageUpload whether uploads should use the image upload route during
	 *        the Phase 3 legacy upload-page seam
	 * @return the resulting content component, or {@code null} when a no-op builder
	 *         receives no existing component
	 */
	public abstract @Nullable UIComponent content(@Nullable UIComponent component,
											@Nullable String dataWidgetVar,
											@Nonnull ContentUpload content,
											@Nullable String formDisabledCondition,
											@Nullable String title,
											@Nullable String requiredMessage,
											@Nullable HorizontalAlignment textAlignment,
											boolean formContext,
											boolean imageUpload);
	
	/**
	 * Adds a content-signature component into a layout wrapper.
	 *
	 * @param component the current component
	 * @param layout the target layout component
	 * @param signature the content-signature metadata
	 * @param formDisabledCondition form disabled condition
	 * @param title field title
	 * @param requiredMessage optional required-message text
	 * @return the resulting layout component
	 */
	public abstract @Nullable UIComponent addContentSignature(@Nullable UIComponent component,
																	@Nonnull UIComponent layout, 
																	@Nonnull ContentSignature signature, 
																	@Nullable String formDisabledCondition,
																	@Nullable String title, 
																	@Nullable String requiredMessage);

	/**
	 * Creates an HTML editor component.
	 *
	 * @param component the current component
	 * @param dataWidgetVar the data-widget variable
	 * @param html the HTML metadata
	 * @param formDisabledCondition form disabled condition
	 * @param title field title
	 * @param requiredMessage optional required-message text
	 * @return the resulting HTML editor component
	 */
	public abstract @Nullable UIComponent html(@Nullable UIComponent component, 
													@Nullable String dataWidgetVar, 
													@Nonnull HTML html, 
													@Nullable String formDisabledCondition,
													@Nullable String title, 
													@Nullable String requiredMessage);

	/**
	 * Creates a lookup-description component wrapper.
	 *
	 * @param component the existing wrapper component
	 * @param dataWidgetVar the data-widget variable
	 * @param lookup the lookup metadata
	 * @param formDisabledCondition form disabled condition
	 * @param title field title
	 * @param requiredMessage optional required-message text
	 * @param textAlignment text alignment
	 * @param displayBinding display binding
	 * @param query backing query definition
	 * @param owningDocument document owning the lookup model
	 * @return the resulting wrapper component
	 */
	@SuppressWarnings("java:S107") // Long parameter list preserves the existing framework/API contract.
	public abstract @Nullable EventSourceComponent lookupDescription(@Nullable EventSourceComponent component, 
																		@Nullable String dataWidgetVar,
																		@Nonnull LookupDescription lookup,
																		@Nullable String formDisabledCondition,
																		@Nullable String title,
																		@Nullable String requiredMessage,
																		@Nullable HorizontalAlignment textAlignment,
																		@Nonnull String displayBinding,
																		@Nullable QueryDefinition query,
																		@Nonnull Document owningDocument);
	
	/**
	 * Creates a password input component wrapper.
	 *
	 * @param component the existing wrapper component
	 * @param dataWidgetVar the data-widget variable
	 * @param password the password metadata
	 * @param formDisabledCondition form disabled condition
	 * @param title field title
	 * @param requiredMessage optional required-message text
	 * @param textAlignment text alignment
	 * @return the resulting wrapper component
	 */
	public abstract @Nullable EventSourceComponent password(@Nullable EventSourceComponent component, 
																@Nullable String dataWidgetVar,
																@Nonnull Password password,
																@Nullable String formDisabledCondition,
																@Nullable String title,
																@Nullable String requiredMessage,
																@Nullable HorizontalAlignment textAlignment);

	/**
	 * Creates a radio input component wrapper.
	 *
	 * @param component the existing wrapper component
	 * @param dataWidgetVar the data-widget variable
	 * @param radio the radio metadata
	 * @param formDisabledCondition form disabled condition
	 * @param title field title
	 * @param requiredMessage optional required-message text
	 * @return the resulting wrapper component
	 */
	public abstract @Nullable EventSourceComponent radio(@Nullable EventSourceComponent component, 
															@Nullable String dataWidgetVar,
															@Nonnull Radio radio,
															@Nullable String formDisabledCondition,
															@Nullable String title,
															@Nullable String requiredMessage);
	
	/**
	 * Creates a rich-text input component wrapper.
	 *
	 * @param component the existing wrapper component
	 * @param dataWidgetVar the data-widget variable
	 * @param text the rich-text metadata
	 * @param formDisabledCondition form disabled condition
	 * @param title field title
	 * @param requiredMessage optional required-message text
	 * @return the resulting wrapper component
	 */
	public abstract @Nullable EventSourceComponent richText(@Nullable EventSourceComponent component, 
																@Nullable String dataWidgetVar,
																@Nonnull RichText text,
																@Nullable String formDisabledCondition,
																@Nullable String title,
																@Nullable String requiredMessage);
	
	/**
	 * Creates a spinner input component wrapper.
	 *
	 * @param component the existing wrapper component
	 * @param dataWidgetVar the data-widget variable
	 * @param spinner the spinner metadata
	 * @param formDisabledCondition form disabled condition
	 * @param title field title
	 * @param requiredMessage optional required-message text
	 * @param textAlignment text alignment
	 * @param facesConverter JSF converter instance
	 * @return the resulting wrapper component
	 */
	@SuppressWarnings("java:S107") // Long parameter list preserves the existing framework/API contract.
	public abstract @Nullable EventSourceComponent spinner(@Nullable EventSourceComponent component, 
																@Nullable String dataWidgetVar,
																@Nonnull Spinner spinner,
																@Nullable String formDisabledCondition,
																@Nullable String title,
																@Nullable String requiredMessage,
																@Nullable HorizontalAlignment textAlignment,
																@Nullable jakarta.faces.convert.Converter<?> facesConverter);
	
	/**
	 * Creates a slider input component wrapper.
	 *
	 * @param component the existing wrapper component
	 * @param dataWidgetVar the data-widget variable
	 * @param slider the slider metadata
	 * @param formDisabledCondition form disabled condition
	 * @param title field title
	 * @param requiredMessage optional required-message text
	 * @param facesConverter JSF converter instance
	 * @return the resulting wrapper component
	 */
	public abstract @Nullable EventSourceComponent slider(@Nullable EventSourceComponent component, 
																@Nullable String dataWidgetVar,
																@Nonnull Slider slider,
																@Nullable String formDisabledCondition,
																@Nullable String title,
																@Nullable String requiredMessage,
																@Nullable jakarta.faces.convert.Converter<?> facesConverter);

	/**
	 * Creates a text input component wrapper.
	 *
	 * @param component the existing wrapper component
	 * @param dataWidgetVar the data-widget variable
	 * @param text the text-field metadata
	 * @param formDisabledCondition form disabled condition
	 * @param title field title
	 * @param requiredMessage optional required-message text
	 * @param textAlignment text alignment
	 * @param length optional max length
	 * @param converter Skyve converter
	 * @param format Skyve format definition
	 * @param facesConverter JSF converter instance
	 * @return the resulting wrapper component
	 */
	@SuppressWarnings("java:S107") // Long parameter list preserves the existing framework/API contract.
	public abstract @Nullable EventSourceComponent text(@Nullable EventSourceComponent component, 
															@Nullable String dataWidgetVar, 
															@Nonnull TextField text, 
															@Nullable String formDisabledCondition,
															@Nullable String title, 
															@Nullable String requiredMessage,
															@Nullable HorizontalAlignment textAlignment,
															@Nullable Integer length,
															@Nullable Converter<?> converter,
															@Nullable Format<?> format,
															@Nullable jakarta.faces.convert.Converter<?> facesConverter);

	/**
	 * Creates a text-area input component wrapper.
	 *
	 * @param component the existing wrapper component
	 * @param dataWidgetVar the data-widget variable
	 * @param text the text-area metadata
	 * @param formDisabledCondition form disabled condition
	 * @param title field title
	 * @param requiredMessage optional required-message text
	 * @param textAlignment text alignment
	 * @param length optional max length
	 * @return the resulting wrapper component
	 */
	@SuppressWarnings("java:S107") // Long parameter list preserves the existing framework/API contract.
	public abstract @Nullable EventSourceComponent textArea(@Nullable EventSourceComponent component, 
																@Nullable String dataWidgetVar,
																@Nonnull TextArea text,
																@Nullable String formDisabledCondition,
																@Nullable String title,
																@Nullable String requiredMessage,
																@Nullable HorizontalAlignment textAlignment,
																@Nullable Integer length);
	
	/**
	 * Creates an output link component for reference fields.
	 *
	 * <p>Side effects: creates a JSF output-link component, assigns href/rendered
	 * expressions, and adds child output text when {@code value} is present. The
	 * child text remains raw until its output-text escape flag is applied.
	 *
	 * @param dataWidgetVar optional data widget variable
	 * @param value optional raw link label and nullable escape flag; {@code null} and
	 *        {@code Boolean.TRUE} escape at the child output-text boundary, and only
	 *        {@code Boolean.FALSE} allows trusted markup
	 * @param href link destination expression
	 * @param invisible invisible condition expression
	 * @param target optional reference target metadata
	 * @return the configured output link component
	 */
	public @Nonnull HtmlOutputLink outputLink(@Nullable String dataWidgetVar, 
												@Nullable EscapableText value,
												@Nonnull String href, 
												@Nullable String invisible,
												@Nullable ReferenceTarget target) {
		HtmlOutputLink result = (HtmlOutputLink) a.createComponent(HtmlOutputLink.COMPONENT_TYPE);
		if (dataWidgetVar != null) {
			result.setValueExpression("value", createValueExpressionFromFragment(dataWidgetVar, true, href, true, null, String.class, false, Sanitisation.relaxed));
		}
		else {
			result.setValueExpression("value", createValueExpressionFromFragment(href, true, null, String.class, false, Sanitisation.relaxed));
		}
		if ((value != null) && (value.getValue() != null)) {
			HtmlOutputText outputText = outputText(value);
			result.getChildren().add(outputText);
		}
		setInvisible(result, invisible, null);

		if (target != null) {
			// modal windows are not supported
			ReferenceTargetType type = target.getType();
			if (ReferenceTargetType.blankFrame.equals(type)) {
				result.setTarget("_blank");
			}
			else if (ReferenceTargetType.namedFame.equals(type)) {
				result.setTarget(target.getName());
			}
		}

		return result;
	}

	protected static class ActionFacesAttributes {
		protected String actionName;
		protected String process;
		protected String update;
	}
	
	/**
	 * Derives action name and AJAX process/update overrides from event actions.
	 *
	 * @param actions event action metadata
	 * @return derived faces action attributes
	 */
	protected static @Nonnull ActionFacesAttributes determineActionFacesAttributes(@Nonnull List<EventAction> actions) {
		ActionFacesAttributes result = new ActionFacesAttributes();
		boolean rerenderValidate = true;
		for (EventAction action : actions) {
	    	Map<String, String> properties = action.getProperties();
	    	String processOverride = properties.get(PROCESS_KEY);
	    	if (processOverride != null) {
	    		result.process = processOverride;
	    	}
	    	String updateOverride = properties.get(UPDATE_KEY);
	    	if (updateOverride != null) {
	    		result.update = updateOverride;
	    	}
			if (action instanceof ServerSideActionEventAction server) {
				result.actionName = server.getActionName();
				break;
			}
			else if (action instanceof RerenderEventAction rerender) {
				rerenderValidate = ! Boolean.FALSE.equals(rerender.getClientValidation());
				result.actionName = String.valueOf(rerenderValidate);
				break;
			}
		}
		
		return result;
	}
	
	/**
	 * Adds an AJAX behavior for event-driven rerender or action callbacks.
	 *
	 * @param component the event source component
	 * @param eventName the JSF event name
	 * @param collectionBinding optional collection binding
	 * @param dataWidgetVar optional data-widget variable
	 * @param rerenderSource rerender source identifier
	 * @param actions event actions to wire
	 */
	public void addAjaxBehavior(@Nonnull UIComponentBase component, 
									@Nonnull String eventName,
									@Nullable String collectionBinding,
									@Nullable String dataWidgetVar,
									@Nullable String rerenderSource, 
									@Nonnull List<EventAction> actions) {
		ActionFacesAttributes attributes = determineActionFacesAttributes(actions);
		AjaxBehavior ajax = (AjaxBehavior) a.createBehavior(AjaxBehavior.BEHAVIOR_ID);
		ajax.setProcess((attributes.process == null) ? process : attributes.process);
		ajax.setUpdate((attributes.update == null) ? update : attributes.update);
		if (Boolean.TRUE.toString().equals(attributes.actionName)) {
			MethodExpression me = methodExpressionForRerender(rerenderSource, true);
			ajax.addAjaxBehaviorListener(new AjaxBehaviorListenerImpl(me, me));
		}
		else if (Boolean.FALSE.toString().equals(attributes.actionName)) {
			MethodExpression me = methodExpressionForRerender(rerenderSource, false);
			ajax.addAjaxBehaviorListener(new AjaxBehaviorListenerImpl(me, me));
		}
		else {
			MethodExpression me = methodExpressionForAction(null, 
																attributes.actionName, 
																collectionBinding, 
																dataWidgetVar, 
																false, 
																null);
			ajax.addAjaxBehaviorListener(new AjaxBehaviorListenerImpl(me, me));
		}

		component.addClientBehavior(eventName, ajax);
	}

	/**
	 * Creates an action link component.
	 *
	 * @param component the current component
	 * @param dataWidgetBinding optional data-widget binding
	 * @param dataWidgetVar optional data-widget variable
	 * @param label label text
	 * @param iconStyleClass icon style class
	 * @param toolTip tooltip text
	 * @param confirmationText confirmation prompt text
	 * @param link link metadata
	 * @param action action metadata
	 * @return the resulting action-link component
	 */
	@SuppressWarnings("java:S107") // Long parameter list preserves the existing framework/API contract.
	public abstract @Nullable UIComponent actionLink(@Nullable UIComponent component,
														@Nullable String dataWidgetBinding,
														@Nullable String dataWidgetVar,
														@Nullable String label,
														@Nullable String iconStyleClass,
														@Nullable String toolTip,
														@Nullable String confirmationText,
														@Nonnull Link link,
														@Nonnull Action action);

	/**
	 * Creates a report action component.
	 *
	 * @param component the current component
	 * @param label label text
	 * @param iconStyleClass icon style class
	 * @param toolTip tooltip text
	 * @param confirmationText confirmation prompt text
	 * @param action action metadata
	 * @return the resulting report component
	 */
	public abstract @Nullable UIComponent report(@Nullable UIComponent component,
													@Nullable String label,
													@Nullable String iconStyleClass,
													@Nullable String toolTip,
													@Nullable String confirmationText,
													@Nonnull Action action);
	
	/**
	 * Creates a download action component.
	 *
	 * @param component the current component
	 * @param dataWidgetBinding optional data-widget binding
	 * @param dataWidgetVar optional data-widget variable
	 * @param label label text
	 * @param iconStyleClass icon style class
	 * @param toolTip tooltip text
	 * @param confirmationText confirmation prompt text
	 * @param action action metadata
	 * @return the resulting download component
	 */
	@SuppressWarnings("java:S107") // Long parameter list preserves the existing framework/API contract.
	public abstract @Nullable UIComponent download(@Nullable UIComponent component, 
														@Nullable String dataWidgetBinding,
														@Nullable String dataWidgetVar,
														@Nullable String label,
														@Nullable String iconStyleClass,
														@Nullable String toolTip,
														@Nullable String confirmationText,
														@Nonnull Action action);
	
	/**
	 * Creates an upload action component.
	 *
	 * @param component the current component
	 * @param label label text
	 * @param iconStyleClass icon style class
	 * @param toolTip tooltip text
	 * @param confirmationText confirmation prompt text
	 * @param action action metadata
	 * @return the resulting upload component
	 */
	public abstract @Nullable UIComponent upload(@Nullable UIComponent component,
													@Nullable String label,
													@Nullable String iconStyleClass,
													@Nullable String toolTip,
													@Nullable String confirmationText,
													@Nonnull Action action);

	/**
	 * Creates a remove action component.
	 *
	 * @param component the current component
	 * @param label label text
	 * @param iconStyleClass icon style class
	 * @param toolTip tooltip text
	 * @param confirmationText confirmation prompt text
	 * @param action action metadata
	 * @param canDelete whether delete is permitted
	 * @return the resulting remove component
	 */
	public abstract @Nullable UIComponent remove(@Nullable UIComponent component,
													@Nullable String label,
													@Nullable String iconStyleClass,
													@Nullable String toolTip,
													@Nullable String confirmationText,
													@Nonnull Action action,
													boolean canDelete);

	/**
	 * Creates an implicit or explicit action component.
	 *
	 * @param component the current component
	 * @param dataWidgetBinding optional data-widget binding
	 * @param dataWidgetVar optional data-widget variable
	 * @param label label text
	 * @param iconStyleClass icon style class
	 * @param toolTip tooltip text
	 * @param confirmationText confirmation prompt text
	 * @param name implicit action name
	 * @param action action metadata
	 * @return the resulting action component
	 */
	@SuppressWarnings("java:S107") // Long parameter list preserves the existing framework/API contract.
	public abstract @Nullable UIComponent action(@Nullable UIComponent component, 
													@Nullable String dataWidgetBinding, 
													@Nullable String dataWidgetVar,
													@Nullable String label,
													@Nullable String iconStyleClass,
													@Nullable String toolTip,
													@Nullable String confirmationText,
													@Nullable ImplicitActionName name, 
													@Nonnull Action action);
	
	static final Class<?>[] STRING = new Class<?>[] {String.class};
	static final Class<?>[] STRING_STRING = new Class<?>[] {String.class, String.class};
	static final Class<?>[] STRING_STRING_STRING = new Class<?>[] {String.class, String.class, String.class};
	static final Class<?>[] STRING_STRING_LIST = new Class<?>[] {String.class, String.class, List.class};
	static final Class<?>[] STRING_BOOLEAN = new Class<?>[] {String.class, Boolean.class};
	static final Class<?>[] NONE = new Class[0];
	
	/**
	 * Creates a method expression for rerender callbacks.
	 *
	 * @param source rerender source identifier
	 * @param validate whether client validation should run
	 * @return the JSF method expression
	 */
	protected @Nonnull MethodExpression methodExpressionForRerender(@Nullable String source, boolean validate) {
		StringBuilder expression = new StringBuilder(64);
		expression.append("#{").append(managedBeanName).append(".rerender(");
		if (source == null) {
			expression.append("null");
		}
		else {
			expression.append('\'').append(source).append('\'');
		}
		expression.append(',').append(validate).append(")}");

		return ef.createMethodExpression(elc, expression.toString(), null, STRING_BOOLEAN);
	}

	/**
	 * Wires a UI command with the appropriate download action expression.
	 *
	 * @param downloadActionName download action name
	 * @param dataWidgetBinding optional data-widget binding
	 * @param dataWidgetVar optional data-widget variable
	 * @param command the target UI command
	 */
	protected void downloadActionExpression(@Nonnull String downloadActionName,
											@Nullable String dataWidgetBinding,
											@Nullable String dataWidgetVar,
											@Nonnull UICommand command) {
		if (dataWidgetBinding != null) {
			StringBuilder expression = new StringBuilder(64);
			expression.append("#{").append(managedBeanName).append(".download('");
			expression.append(downloadActionName).append("','").append(dataWidgetBinding);
			expression.append("',").append(dataWidgetVar).append("['");
			expression.append(Bean.DOCUMENT_ID).append("'])}");
			MethodExpression method = ef.createMethodExpression(elc, expression.toString(), null, STRING_STRING_STRING);
			command.setActionExpression(method);
		}
		else {
			StringBuilder expression = new StringBuilder(48);
			expression.append("#{").append(managedBeanName).append(".download('");
			expression.append(downloadActionName).append("', null, null)}");
			MethodExpression method = ef.createMethodExpression(elc, expression.toString(), null, STRING_STRING_STRING);
			command.setActionExpression(method);
		}
	}

	/**
	 * Creates a method expression for implicit or explicit action callbacks.
	 *
	 * @param implicitActionName optional implicit action name
	 * @param actionName explicit action name when implicit action is null
	 * @param collectionBinding optional collection binding
	 * @param dataWidgetVar optional data-widget variable
	 * @param inline whether inline mode is active
	 * @param eventHandlerActionNames optional event handler names
	 * @return the JSF method expression
	 */
	@SuppressWarnings("java:S3776") // Complexity OK
	protected @Nonnull MethodExpression methodExpressionForAction(@Nullable ImplicitActionName implicitActionName, 
																	@Nullable String actionName,
																	@Nullable String collectionBinding, 
																	@Nullable String dataWidgetVar,
																	boolean inline,
																	@Nullable List<String> eventHandlerActionNames) {
		StringBuilder expression = new StringBuilder(64);
		expression.append("#{").append(managedBeanName).append('.');
		Class<?>[] parameterTypes = null;
		if (implicitActionName != null) {
			expression.append(implicitActionName.toString().toLowerCase());
			if (collectionBinding != null) {
				if (ImplicitActionName.Add.equals(implicitActionName)) {
					parameterTypes = STRING_BOOLEAN;
					expression.append("('").append(collectionBinding).append("',").append(inline).append(")");
				} 
				else if (ImplicitActionName.Remove.equals(implicitActionName)) {
					parameterTypes = STRING_STRING_LIST;
					expression.append("('").append(collectionBinding).append("',");
					expression.append(dataWidgetVar).append("['").append(Bean.DOCUMENT_ID).append("'],");

					// Add filter parameters to getModel call
					if ((eventHandlerActionNames != null) && (! eventHandlerActionNames.isEmpty())) {
						expression.append('[');
						for (String eventHandlerActionName : eventHandlerActionNames) {
							expression.append("'").append(eventHandlerActionName).append("',");
						}
						expression.setLength(expression.length() - 1); // remove last comma
						expression.append("])");
					}
					else {
						expression.append("null)");
					}
				} 
				else {
					parameterTypes = STRING_STRING;
					expression.append("('").append(collectionBinding).append("', ");
					expression.append(dataWidgetVar).append("['").append(Bean.DOCUMENT_ID).append("'])");
				}
			} 
			else {
				if (ImplicitActionName.Remove.equals(implicitActionName)) {
					parameterTypes = STRING_STRING_LIST;
					expression.append("(null,null,null)");
				} 
				else {
					parameterTypes = NONE;
				}
			}
		}
		else {
			parameterTypes = STRING_STRING_STRING;
			expression.append("action('").append(actionName).append('\'');
			if (collectionBinding != null) {
				expression.append(", '").append(collectionBinding).append("', ");
				expression.append(dataWidgetVar).append("['").append(Bean.DOCUMENT_ID).append("'])");
			} 
			else {
				expression.append(", null, null)");
			}
		}
		expression.append('}');

		return ef.createMethodExpression(elc, expression.toString(), null, parameterTypes);
	}
}
