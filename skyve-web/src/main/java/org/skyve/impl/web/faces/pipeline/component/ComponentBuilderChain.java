package org.skyve.impl.web.faces.pipeline.component;

import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.container.Collapsible;
import org.skyve.impl.metadata.view.container.Sidebar;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.widget.Blurb;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.impl.metadata.view.widget.Chart;
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
import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;
import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;
import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;
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
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.query.QueryDefinition;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.widget.FilterParameter;
import org.skyve.metadata.view.widget.bound.Parameter;
import org.skyve.web.UserAgentType;

import jakarta.annotation.Nullable;
import jakarta.faces.component.UIComponent;

/**
 * Delegates to a list of other builders that actually produce the components.
 */
public class ComponentBuilderChain extends ComponentBuilder {
	private ComponentBuilder[] builders;
	
	/**
	 * Creates a chain that delegates component construction to each supplied builder.
	 *
	 * @param builders ordered builders participating in the chain
	 */
	public ComponentBuilderChain(ComponentBuilder... builders) {
		this.builders = builders;
	}
	
	/**
	 * Sets the managed bean name on this chain and all delegated builders.
	 *
	 * @param managedBeanName the managed bean name
	 */
	@Override
	public void setManagedBeanName(String managedBeanName) {
		// Set the state of the chain too so that utility methods in AbstractFacesBuilder can work
		super.setManagedBeanName(managedBeanName);
		// Now set the state on all builders in the chain
		for (ComponentBuilder builder : builders) {
			builder.setManagedBeanName(managedBeanName);
		}
	}
	
	/**
	 * Sets the SAIL managed bean on this chain and all delegated builders.
	 *
	 * @param managedBean the SAIL managed bean
	 */
	@Override
	public void setSAILManagedBean(FacesView managedBean) {
		// Set the state of the chain too so that utility methods in AbstractFacesBuilder can work
		super.setSAILManagedBean(managedBean);
		// Now set the state on all builders in the chain
		for (ComponentBuilder builder : builders) {
			builder.setSAILManagedBean(managedBean);
		}
	}

	/**
	 * Sets the PrimeFaces process expression on this chain and delegates.
	 *
	 * @param process the process expression
	 */
	@Override
	public void setProcess(String process) {
		// Set the state of the chain too so that utility methods in AbstractFacesBuilder can work
		super.setProcess(process);
		// Now set the state on all builders in the chain
		for (ComponentBuilder builder : builders) {
			builder.setProcess(process);
		}
	}

	/**
	 * Sets the PrimeFaces update expression on this chain and delegates.
	 *
	 * @param update the update expression
	 */
	@Override
	public void setUpdate(String update) {
		// Set the state of the chain too so that utility methods in AbstractFacesBuilder can work
		super.setUpdate(update);
		// Now set the state on all builders in the chain
		for (ComponentBuilder builder : builders) {
			builder.setUpdate(update);
		}
	}

	/**
	 * Sets the user agent type on this chain and delegates.
	 *
	 * @param userAgentType the current user agent type
	 */
	@Override
	public void setUserAgentType(UserAgentType userAgentType) {
		// Set the state of the chain too so that utility methods in AbstractFacesBuilder can work
		super.setUserAgentType(userAgentType);
		// Now set the state on all builders in the chain
		for (ComponentBuilder builder : builders) {
			builder.setUserAgentType(userAgentType);
		}
	}
	
	/**
	 * Delegates top-level view component decoration.
	 *
	 * @param component the source component
	 * @param createView whether create-view mode is active
	 * @return the transformed component
	 */
	@Override
	public UIComponent view(UIComponent component, boolean createView) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.view(result, createView);
		}
		return result;
	}

	/**
	 * Delegates toolbar component generation.
	 *
	 * @param components existing toolbar components
	 * @param widgetId the optional widget ID
	 * @return the transformed toolbar components
	 */
	@Override
	public List<UIComponent> toolbars(List<UIComponent> components, String widgetId) {
		List<UIComponent> result = components;
		for (ComponentBuilder builder : builders) {
			result = builder.toolbars(result, widgetId);
		}
		return result;
	}

	/**
	 * Delegates tab-pane container construction.
	 *
	 * @param component the source component
	 * @param tabPane the tab pane metadata
	 * @param moduleName the module name
	 * @param documentName the document name
	 * @return the transformed component
	 */
	@Override
	public UIComponent tabPane(UIComponent component,
								TabPane tabPane,
								String moduleName,
								String documentName) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.tabPane(result, tabPane, moduleName, documentName);
		}
		return result;
	}

	/**
	 * Delegates tab construction.
	 *
	 * @param component the source component
	 * @param title the tab title
	 * @param tab the tab metadata
	 * @return the transformed component
	 */
	@Override
	public UIComponent tab(UIComponent component, String title, Tab tab) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.tab(result, title, tab);
		}
		return result;
	}

	/**
	 * Delegates tab-pane script component construction.
	 *
	 * @param component the source component
	 * @param tabPane the tab pane metadata
	 * @param moduleName the module name
	 * @param documentName the document name
	 * @param tabPaneComponentId the tab pane component ID
	 * @return the transformed component
	 */
	@Override
	public UIComponent tabPaneScript(UIComponent component,
										TabPane tabPane,
										String moduleName,
										String documentName,
										String tabPaneComponentId) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.tabPaneScript(result, tabPane, moduleName, documentName, tabPaneComponentId);
		}
		return result;
	}
	
	/**
	 * Delegates sidebar script component construction.
	 *
	 * @param component the source component
	 * @param sidebar the sidebar metadata
	 * @param createView whether create-view mode is active
	 * @param sidebarComponentId the sidebar component ID
	 * @return the transformed component
	 */
	@Override
	public UIComponent sidebarScript(UIComponent component,
										Sidebar sidebar,
										boolean createView,
										String sidebarComponentId) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.sidebarScript(result, sidebar, createView, sidebarComponentId);
		}
		return result;
	}
	
	/**
	 * Delegates bordered container construction.
	 *
	 * @param component the source component
	 * @param title the border title
	 * @param invisibileConditionName optional invisible condition
	 * @param pixelWidth optional pixel width
	 * @param collapsible collapsible metadata
	 * @return the transformed component
	 */
	@Override
	public UIComponent border(UIComponent component,
								String title,
								String invisibileConditionName,
								Integer pixelWidth,
								Collapsible collapsible) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.border(result, title, invisibileConditionName, pixelWidth, collapsible);
		}
		return result;
	}

	/**
	 * Delegates static label component construction.
	 *
	 * @param component the source component
	 * @param value the label text
	 * @return the transformed component
	 */
	@Override
	public UIComponent label(UIComponent component, String value) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.label(result, value);
		}
		return result;
	}

	/**
	 * Delegates spacer component construction.
	 *
	 * @param component the source component
	 * @param spacer the spacer metadata
	 * @return the transformed component
	 */
	@Override
	public UIComponent spacer(UIComponent component, Spacer spacer) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.spacer(result, spacer);
		}
		return result;
	}

	/**
	 * Delegates zoom-in action component construction.
	 *
	 * @param component the source component
	 * @param label button label
	 * @param iconStyleClass icon style class
	 * @param toolTip tooltip text
	 * @param zoomIn zoom-in metadata
	 * @param formDisabledConditionName form disabled condition
	 * @return the transformed component
	 */
	@Override
	public UIComponent zoomIn(UIComponent component, 
										String label,
										String iconStyleClass,
										String toolTip,
										ZoomIn zoomIn,
										String formDisabledConditionName) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.zoomIn(result, 
										label,
										iconStyleClass,
										toolTip,
										zoomIn,
										formDisabledConditionName);
		}
		return result;
	}

	/**
	 * Delegates generic action button construction.
	 *
	 * @param component the source component
	 * @param dataWidgetBinding data widget binding
	 * @param dataWidgetVar data widget variable
	 * @param label button label
	 * @param iconStyleClass icon style class
	 * @param toolTip tooltip text
	 * @param confirmationText confirmation text
	 * @param button button metadata
	 * @param formDisabledConditionName form disabled condition
	 * @param action action metadata
	 * @return the transformed component
	 */
	@Override
	public UIComponent actionButton(UIComponent component, 
										String dataWidgetBinding,
										String dataWidgetVar,
										String label,
										String iconStyleClass,
										String toolTip,
										String confirmationText, 
										Button button,
										String formDisabledConditionName,
										Action action) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.actionButton(result, 
											dataWidgetBinding,
											dataWidgetVar,
											label,
											iconStyleClass,
											toolTip,
											confirmationText,
											button,
											formDisabledConditionName,
											action);
		}
		return result;
	}

	/**
	 * Delegates report button construction.
	 *
	 * @param component the source component
	 * @param label button label
	 * @param iconStyleClass icon style class
	 * @param toolTip tooltip text
	 * @param confirmationText confirmation text
	 * @param button button metadata
	 * @param formDisabledConditionName form disabled condition
	 * @param action action metadata
	 * @return the transformed component
	 */
	@Override
	public UIComponent reportButton(UIComponent component,
										String label,
										String iconStyleClass,
										String toolTip,
										String confirmationText, 
										Button button,
										String formDisabledConditionName,
										Action action) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.reportButton(result,
											label,
											iconStyleClass,
											toolTip,
											confirmationText,
											button,
											formDisabledConditionName,
											action);
		}
		return result;
	}

	/**
	 * Delegates download button construction.
	 *
	 * @param component the source component
	 * @param dataWidgetBinding data widget binding
	 * @param dataWidgetVar data widget variable
	 * @param label button label
	 * @param iconStyleClass icon style class
	 * @param toolTip tooltip text
	 * @param confirmationText confirmation text
	 * @param button button metadata
	 * @param formDisabledConditionName form disabled condition
	 * @param action action metadata
	 * @return the transformed component
	 */
	@Override
	public UIComponent downloadButton(UIComponent component,
										String dataWidgetBinding,
										String dataWidgetVar,
										String label,
										String iconStyleClass,
										String toolTip,
										String confirmationText, 
										Button button,
										String formDisabledConditionName,
										Action action) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.downloadButton(result,
												dataWidgetBinding,
												dataWidgetVar,
												label,
												iconStyleClass,
												toolTip,
												confirmationText,
												button,
												formDisabledConditionName,
												action);
		}
		return result;
	}

	/**
	 * Delegates upload button construction.
	 *
	 * @param component the source component
	 * @param label button label
	 * @param iconStyleClass icon style class
	 * @param toolTip tooltip text
	 * @param confirmationText confirmation text
	 * @param button button metadata
	 * @param formDisabledConditionName form disabled condition
	 * @param action action metadata
	 * @return the transformed component
	 */
	@Override
	public UIComponent uploadButton(UIComponent component,
										String label,
										String iconStyleClass,
										String toolTip,
										String confirmationText, 
										Button button,
										String formDisabledConditionName,
										Action action) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.uploadButton(result,
											label,
											iconStyleClass,
											toolTip,
											confirmationText,
											button,
											formDisabledConditionName,
											action);
		}
		return result;
	}

	/**
	 * Delegates static image component construction.
	 *
	 * @param component the source component
	 * @param fileUrl the static image URL
	 * @param image static image metadata
	 * @return the transformed component
	 */
	@Override
	public UIComponent staticImage(UIComponent component, String fileUrl, StaticImage image) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.staticImage(result, fileUrl, image);
		}
		return result;
	}

	/**
	 * Delegates dynamic image component construction.
	 *
	 * @param component the source component
	 * @param image dynamic image metadata
	 * @param moduleName the module name
	 * @param documentName the document name
	 * @return the transformed component
	 */
	@Override
	public UIComponent dynamicImage(UIComponent component, DynamicImage image, String moduleName, String documentName) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.dynamicImage(result, image, moduleName, documentName);
		}
		return result;
	}

	/**
	 * Delegates blurb component construction.
	 *
	 * @param component the source component
	 * @param dataWidgetVar the data widget variable
	 * @param value the display value
	 * @param binding the binding path
	 * @param blurb blurb metadata
	 * @return the transformed component
	 */
	@Override
	public UIComponent blurb(UIComponent component, String dataWidgetVar, String value, String binding, Blurb blurb) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.blurb(result, dataWidgetVar, value, binding, blurb);
		}
		return result;
	}

	/**
	 * Delegates bound label component construction.
	 *
	 * @param component the source component
	 * @param dataWidgetVar the data widget variable
	 * @param value the display value
	 * @param binding the binding path
	 * @param label label metadata
	 * @return the transformed component
	 */
	@Override
	public UIComponent label(UIComponent component, String dataWidgetVar, String value, String binding, Label label) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.label(result, dataWidgetVar, value, binding, label);
		}
		return result;
	}

	/**
	 * Delegates data-grid container construction.
	 *
	 * @param component the source component
	 * @param dataWidgetVar the data widget variable
	 * @param ordered whether rows are ordered
	 * @param grid data-grid metadata
	 * @return the transformed component
	 */
	@Override
	public UIComponent dataGrid(UIComponent component, String dataWidgetVar, boolean ordered, DataGrid grid) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.dataGrid(result, dataWidgetVar, ordered, grid);
		}
		return result;
	}

	/**
	 * Delegates data-repeater container construction.
	 *
	 * @param component the source component
	 * @param dataWidgetVar the data widget variable
	 * @param repeater repeater metadata
	 * @return the transformed component
	 */
	@Override
	public UIComponent dataRepeater(UIComponent component, String dataWidgetVar, DataRepeater repeater) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.dataRepeater(result, dataWidgetVar, repeater);
		}
		return result;
	}

	/**
	 * Delegates data-grid bound-column construction.
	 *
	 * @param component the source component
	 * @param current the current column container component
	 * @param widget the owning data widget
	 * @param column the bound-column metadata
	 * @param dataWidgetVar the data widget variable
	 * @param columnTitle the column title
	 * @param columnBinding the column binding expression
	 * @param gridColumnExpression the grid column expression builder
	 * @param alignment the horizontal alignment
	 * @param pixelWidth the optional pixel width
	 * @return the transformed component
	 */
	@Override
	public UIComponent addDataGridBoundColumn(UIComponent component,
												UIComponent current,
												AbstractDataWidget widget,
												DataGridBoundColumn column,
												String dataWidgetVar,
												String columnTitle,
												String columnBinding,
												StringBuilder gridColumnExpression,
												HorizontalAlignment alignment,
												Integer pixelWidth) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.addDataGridBoundColumn(result, 
														current,
														widget,
														column,
														dataWidgetVar,
														columnTitle,
														columnBinding,
														gridColumnExpression,
														alignment,
														pixelWidth);
		}
		return result;
	}

	/**
	 * Delegates post-processing after a data-grid bound column has been added.
	 *
	 * @param component the source component
	 * @param current the current column component
	 * @param alignment the column alignment
	 * @return the transformed component
	 */
	@Override
	public UIComponent addedDataGridBoundColumn(UIComponent component,
													UIComponent current,
													HorizontalAlignment alignment) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.addedDataGridBoundColumn(result, current, alignment);
		}
		return result;
	}

	/**
	 * Delegates data-grid container-column construction.
	 *
	 * @param component the source component
	 * @param current the current container component
	 * @param widget the owning data widget
	 * @param title the column title
	 * @param column the container-column metadata
	 * @param alignment the horizontal alignment
	 * @return the transformed component
	 */
	@Override
	public UIComponent addDataGridContainerColumn(UIComponent component,
													UIComponent current,
													AbstractDataWidget widget,
													String title,
													DataGridContainerColumn column,
													HorizontalAlignment alignment) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.addDataGridContainerColumn(result, current, widget, title, column,alignment);
		}
		return result;
	}

	/**
	 * Delegates post-processing after a data-grid container column has been added.
	 *
	 * @param component the source component
	 * @param current the current container component
	 * @return the transformed component
	 */
	@Override
	public UIComponent addedDataGridContainerColumn(UIComponent component, UIComponent current) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.addedDataGridContainerColumn(result, current);
		}
		return result;
	}

	/**
	 * Delegates data-grid action-column construction.
	 *
	 * @param component the source component
	 * @param current the current grid component
	 * @param grid the data-grid metadata
	 * @param dataWidgetVar the data widget variable
	 * @param gridColumnExpression the action column expression
	 * @param singluarDocumentAlias the singular document alias
	 * @param inline whether inline actions are enabled
	 * @param canCreate whether create is permitted
	 * @param canDelete whether delete is permitted
	 * @return the transformed component
	 */
	@Override
	public UIComponent addDataGridActionColumn(UIComponent component,
												UIComponent current,
												DataGrid grid,
												String dataWidgetVar,
												String gridColumnExpression,
												String singluarDocumentAlias,
												boolean inline,
												boolean canCreate,
												boolean canDelete) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.addDataGridActionColumn(result,
														current,
														grid,
														dataWidgetVar,
														gridColumnExpression,
														singluarDocumentAlias,
														inline,
														canCreate,
														canDelete);
		}
		return result;
	}

	/**
	 * Delegates map component construction using a query-backed model.
	 *
	 * @param component the source component
	 * @param map the map metadata
	 * @param moduleName the module name
	 * @param queryName the query name
	 * @param geometryBinding the geometry binding expression
	 * @return the transformed component
	 */
	@Override
	public UIComponent map(UIComponent component,
							MapDisplay map,
							String moduleName,
							String queryName,
							String geometryBinding) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.map(result, map, moduleName, queryName, geometryBinding);
		}
		return result;
	}

	/**
	 * Delegates map component construction using a pre-resolved model name.
	 *
	 * @param component the source component
	 * @param map the map metadata
	 * @param modelName the list model name
	 * @return the transformed component
	 */
	@Override
	public UIComponent map(UIComponent component, MapDisplay map, String modelName) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.map(result, map, modelName);
		}
		return result;
	}
	
	/**
	 * Delegates geometry input construction.
	 *
	 * @param component the source event source component
	 * @param dataWidgetVar the data widget variable
	 * @param geometry the geometry metadata
	 * @param formDisabledConditionName the form disabled condition name
	 * @param title the field title
	 * @param requiredMessage the optional required message
	 * @param textAlignment the text alignment
	 * @return the transformed event source component
	 */
	@Override
	public EventSourceComponent geometry(EventSourceComponent component,
											String dataWidgetVar,
											Geometry geometry,
											String formDisabledConditionName,
											String title,
											@Nullable String requiredMessage,
											HorizontalAlignment textAlignment) {
		EventSourceComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.geometry(result, dataWidgetVar, geometry, formDisabledConditionName, title, requiredMessage, textAlignment);
		}
		return result;
	}

	/**
	 * Delegates geometry-map input construction.
	 *
	 * @param component the source event source component
	 * @param geometry the geometry-map metadata
	 * @param formDisabledConditionName the form disabled condition name
	 * @param title the field title
	 * @param requiredMessage the optional required message
	 * @return the transformed event source component
	 */
	@Override
	public EventSourceComponent geometryMap(EventSourceComponent component,
												GeometryMap geometry,
												String formDisabledConditionName,
												String title,
												@Nullable String requiredMessage) {
		EventSourceComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.geometryMap(result, geometry, formDisabledConditionName, title, requiredMessage);
		}
		return result;
	}
	
	/**
	 * Delegates chart component construction.
	 *
	 * @param component the source component
	 * @param chart the chart metadata
	 * @return the transformed component
	 */
	@Override
	public UIComponent chart(UIComponent component, Chart chart) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.chart(result, chart);
		}
		return result;
	}
	
	/**
	 * Delegates list-grid construction.
	 *
	 * @param component the source component
	 * @param moduleName module name
	 * @param modelDocumentName model document name
	 * @param modelName model name
	 * @param uxui UX/UI name
	 * @param model list model
	 * @param owningDocument owning document
	 * @param listGrid list-grid metadata
	 * @param aggregateQuery whether aggregate query mode is enabled
	 * @return the transformed component
	 */
	@Override
	public UIComponent listGrid(UIComponent component,
									String moduleName,
									String modelDocumentName,
									String modelName,
									String uxui,
									ListModel<Bean> model,
									Document owningDocument,
									ListGrid listGrid,
									boolean aggregateQuery) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.listGrid(result,
										moduleName,
										modelDocumentName,
										modelName,
										uxui,
										model,
										owningDocument,
										listGrid,
										aggregateQuery);
		}
		return result;
	}

	/**
	 * Delegates list-grid context-menu construction.
	 *
	 * @param component the source component
	 * @param listGridId the list-grid component ID
	 * @param listGrid the list-grid metadata
	 * @return the transformed component
	 */
	@Override
	public UIComponent listGridContextMenu(UIComponent component,
												String listGridId,
												ListGrid listGrid) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.listGridContextMenu(result, listGridId, listGrid);
		}
		return result;
	}
	
	/**
	 * Delegates list-repeater construction.
	 *
	 * @param component the source component
	 * @param modelDocumentName the model document name
	 * @param modelName the model name
	 * @param uxui the UX/UI profile
	 * @param model the list model
	 * @param filterParameters the configured filter parameters
	 * @param parameters the configured query parameters
	 * @param showColumnHeaders whether column headers are shown
	 * @param showGrid whether grid layout is shown
	 * @return the transformed component
	 */
	@Override
	public UIComponent listRepeater(UIComponent component,
										String modelDocumentName,
										String modelName,
										String uxui,
										ListModel<Bean> model,
										List<FilterParameter> filterParameters,
										List<Parameter> parameters,
										boolean showColumnHeaders,
										boolean showGrid) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.listRepeater(result,
											modelDocumentName,
											modelName,
											uxui,
											model,
											filterParameters,
											parameters,
											showColumnHeaders,
											showGrid);
		}
		return result;
	}

	/**
	 * Delegates list-membership input construction.
	 *
	 * @param component the source event source component
	 * @param candidatesHeading the candidates heading text
	 * @param membersHeading the members heading text
	 * @param membership the list-membership metadata
	 * @return the transformed event source component
	 */
	@Override
	public EventSourceComponent listMembership(EventSourceComponent component,
												String candidatesHeading,
												String membersHeading,
												ListMembership membership) {
		EventSourceComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.listMembership(result, candidatesHeading, membersHeading, membership);
		}
		return result;
	}

	/**
	 * Delegates checkbox input construction.
	 *
	 * @param component the source event source component
	 * @param dataWidgetVar the data widget variable
	 * @param checkBox the checkbox metadata
	 * @param formDisabledConditionName the form disabled condition name
	 * @param title the field title
	 * @param requiredMessage the optional required message
	 * @return the transformed event source component
	 */
	@Override
	public EventSourceComponent checkBox(EventSourceComponent component,
											String dataWidgetVar,
											CheckBox checkBox,
											String formDisabledConditionName,
											String title,
											@Nullable String requiredMessage) {
		EventSourceComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.checkBox(result,
										dataWidgetVar,
										checkBox,
										formDisabledConditionName,
										title,
										requiredMessage);
		}
		return result;
	}

	/**
	 * Delegates colour-picker input construction.
	 *
	 * @param component the source event source component
	 * @param dataWidgetVar the data widget variable
	 * @param colour the colour-picker metadata
	 * @param formDisabledConditionName the form disabled condition name
	 * @param title the field title
	 * @param requiredMessage the optional required message
	 * @param textAlignment the text alignment
	 * @return the transformed event source component
	 */
	@Override
	public EventSourceComponent colourPicker(EventSourceComponent component,
												String dataWidgetVar,
												ColourPicker colour,
												String formDisabledConditionName,
												String title,
												@Nullable String requiredMessage,
												HorizontalAlignment textAlignment) {
		EventSourceComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.colourPicker(result,
											dataWidgetVar,
											colour,
											formDisabledConditionName,
											title,
											requiredMessage,
											textAlignment);
		}
		return result;
	}

	/**
	 * Delegates combo input construction.
	 *
	 * @param component the source event source component
	 * @param dataWidgetVar the data widget variable
	 * @param combo the combo metadata
	 * @param formDisabledConditionName the form disabled condition name
	 * @param title the field title
	 * @param requiredMessage the optional required message
	 * @return the transformed event source component
	 */
	@Override
	public EventSourceComponent combo(EventSourceComponent component,
										String dataWidgetVar,
										Combo combo,
										String formDisabledConditionName,
										String title,
										@Nullable String requiredMessage) {
		EventSourceComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.combo(result, dataWidgetVar, combo, formDisabledConditionName,  title, requiredMessage);
		}
		return result;
	}

	/**
	 * Delegates content-image component construction.
	 *
	 * @param component the source component
	 * @param dataWidgetVar the data widget variable
	 * @param image the content-image metadata
	 * @param formDisabledConditionName the form disabled condition name
	 * @param title the field title
	 * @param requiredMessage the optional required message
	 * @return the transformed component
	 */
	@Override
	public UIComponent contentImage(UIComponent component,
										String dataWidgetVar,
										ContentImage image,
										String formDisabledConditionName,
										String title,
										@Nullable String requiredMessage) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.contentImage(result, dataWidgetVar, image, formDisabledConditionName, title, requiredMessage);
		}
		return result;
	}

	/**
	 * Delegates content-link component construction.
	 *
	 * @param component the source component
	 * @param dataWidgetVar the data widget variable
	 * @param link the content-link metadata
	 * @param formDisabledConditionName the form disabled condition name
	 * @param title the field title
	 * @param requiredMessage the optional required message
	 * @param textAlignment the text alignment
	 * @return the transformed component
	 */
	@Override
	public UIComponent contentLink(UIComponent component,
									String dataWidgetVar,
									ContentLink link,
									String formDisabledConditionName,
									String title,
									@Nullable String requiredMessage,
									HorizontalAlignment textAlignment) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.contentLink(result, dataWidgetVar, link, formDisabledConditionName, title, requiredMessage, textAlignment);
		}
		return result;
	}

	/**
	 * Delegates content-signature component insertion.
	 *
	 * @param component the source component
	 * @param layout the current layout component
	 * @param signature the signature metadata
	 * @param formDisabledConditionName the form disabled condition name
	 * @param title the field title
	 * @param requiredMessage the optional required message
	 * @return the transformed component
	 */
	@Override
	public UIComponent addContentSignature(UIComponent component,
											UIComponent layout,
											ContentSignature signature,
											String formDisabledConditionName,
											String title,
											@Nullable String requiredMessage) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.addContentSignature(result, layout, signature, formDisabledConditionName, title, requiredMessage);
		}
		return result;
	}

	/**
	 * Delegates HTML editor component construction.
	 *
	 * @param component the source component
	 * @param dataWidgetVar the data widget variable
	 * @param html the HTML metadata
	 * @param formDisabledConditionName the form disabled condition name
	 * @param title the field title
	 * @param requiredMessage the optional required message
	 * @return the transformed component
	 */
	@Override
	public UIComponent html(UIComponent component,
								String dataWidgetVar,
								HTML html,
								String formDisabledConditionName,
								String title,
								@Nullable String requiredMessage) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.html(result, dataWidgetVar, html, formDisabledConditionName, title, requiredMessage);
		}
		return result;
	}

	/**
	 * Delegates lookup-description input construction.
	 *
	 * @param component the source event source component
	 * @param dataWidgetVar the data widget variable
	 * @param lookup the lookup metadata
	 * @param formDisabledConditionName the form disabled condition name
	 * @param title the field title
	 * @param requiredMessage the optional required message
	 * @param textAlignment the text alignment
	 * @param displayBinding the display binding
	 * @param query the lookup query definition
	 * @return the transformed event source component
	 */
	@Override
	public EventSourceComponent lookupDescription(EventSourceComponent component,
													String dataWidgetVar,
													LookupDescription lookup,
													String formDisabledConditionName,
													String title,
													@Nullable String requiredMessage,
													HorizontalAlignment textAlignment,
													String displayBinding,
													QueryDefinition query) {
		EventSourceComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.lookupDescription(result,
												dataWidgetVar,
												lookup,
												formDisabledConditionName,
												title,
												requiredMessage,
												textAlignment,
												displayBinding,
												query);
		}
		return result;
	}

	/**
	 * Delegates password input construction.
	 *
	 * @param component the source event source component
	 * @param dataWidgetVar the data widget variable
	 * @param password the password metadata
	 * @param formDisabledConditionName the form disabled condition name
	 * @param title the field title
	 * @param requiredMessage the optional required message
	 * @param textAlignment the text alignment
	 * @return the transformed event source component
	 */
	@Override
	public EventSourceComponent password(EventSourceComponent component,
											String dataWidgetVar,
											Password password,
											String formDisabledConditionName,
											String title,
											@Nullable String requiredMessage,
											HorizontalAlignment textAlignment) {
		EventSourceComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.password(result, dataWidgetVar, password, formDisabledConditionName, title, requiredMessage, textAlignment);
		}
		return result;
	}

	/**
	 * Delegates radio input construction.
	 *
	 * @param component the source event source component
	 * @param dataWidgetVar the data widget variable
	 * @param radio the radio metadata
	 * @param formDisabledConditionName the form disabled condition name
	 * @param title the field title
	 * @param requiredMessage the optional required message
	 * @return the transformed event source component
	 */
	@Override
	public EventSourceComponent radio(EventSourceComponent component,
										String dataWidgetVar,
										Radio radio,
										String formDisabledConditionName,
										String title,
										@Nullable String requiredMessage) {
		EventSourceComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.radio(result, dataWidgetVar, radio, formDisabledConditionName, title, requiredMessage);
		}
		return result;
	}

	/**
	 * Delegates rich-text input construction.
	 *
	 * @param component the source event source component
	 * @param dataWidgetVar the data widget variable
	 * @param text the rich-text metadata
	 * @param formDisabledConditionName the form disabled condition name
	 * @param title the field title
	 * @param requiredMessage the optional required message
	 * @return the transformed event source component
	 */
	@Override
	public EventSourceComponent richText(EventSourceComponent component,
											String dataWidgetVar,
											RichText text,
											String formDisabledConditionName,
											String title,
											@Nullable String requiredMessage) {
		EventSourceComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.richText(result, dataWidgetVar, text, formDisabledConditionName, title, requiredMessage);
		}
		return result;
	}

	/**
	 * Delegates spinner input construction.
	 *
	 * @param component the source event source component
	 * @param dataWidgetVar the data widget variable
	 * @param spinner the spinner metadata
	 * @param formDisabledConditionName the form disabled condition name
	 * @param title the field title
	 * @param requiredMessage the optional required message
	 * @param textAlignment the text alignment
	 * @param facesConverter the Faces converter
	 * @return the transformed event source component
	 */
	@Override
	public EventSourceComponent spinner(EventSourceComponent component,
											String dataWidgetVar,
											Spinner spinner,
											String formDisabledConditionName,
											String title,
											@Nullable String requiredMessage,
											HorizontalAlignment textAlignment,
											jakarta.faces.convert.Converter<?> facesConverter) {
		EventSourceComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.spinner(result, dataWidgetVar, spinner, formDisabledConditionName, title, requiredMessage, textAlignment, facesConverter);
		}
		return result;
	}

	/**
	 * Delegates slider input construction.
	 *
	 * @param component the source event source component
	 * @param dataWidgetVar the data widget variable
	 * @param slider the slider metadata
	 * @param formDisabledConditionName the form disabled condition name
	 * @param title the field title
	 * @param requiredMessage the optional required message
	 * @param facesConverter the Faces converter
	 * @return the transformed event source component
	 */
	@Override
	public EventSourceComponent slider(EventSourceComponent component,
											String dataWidgetVar,
											Slider slider,
											String formDisabledConditionName,
											String title,
											@Nullable String requiredMessage,
											jakarta.faces.convert.Converter<?> facesConverter) {
		EventSourceComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.slider(result, dataWidgetVar, slider, formDisabledConditionName, title, requiredMessage, facesConverter);
		}
		return result;
	}

	/**
	 * Delegates text-area input construction.
	 *
	 * @param component the source event source component
	 * @param dataWidgetVar the data widget variable
	 * @param text the text-area metadata
	 * @param formDisabledConditionName the form disabled condition name
	 * @param title the field title
	 * @param requiredMessage the optional required message
	 * @param textAlignment the text alignment
	 * @param length the maximum field length
	 * @return the transformed event source component
	 */
	@Override
	public EventSourceComponent textArea(EventSourceComponent component,
											String dataWidgetVar,
											TextArea text,
											String formDisabledConditionName,
											String title,
											@Nullable String requiredMessage,
											HorizontalAlignment textAlignment,
											Integer length) {
		EventSourceComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.textArea(result, dataWidgetVar, text, formDisabledConditionName, title, requiredMessage, textAlignment, length);
		}
		return result;
	}

	/**
	 * Delegates text input construction.
	 *
	 * @param component the source component
	 * @param dataWidgetVar the data widget variable
	 * @param text text metadata
	 * @param formDisabledConditionName form disabled condition
	 * @param title field title
	 * @param requiredMessage optional required message
	 * @param textAlignment text alignment
	 * @param length field length
	 * @param converter Skyve converter
	 * @param format Skyve format
	 * @param facesConverter Faces converter
	 * @return the transformed event source component
	 */
	@Override
	public EventSourceComponent text(EventSourceComponent component,
										String dataWidgetVar,
										TextField text,
										String formDisabledConditionName,
										String title,
										@Nullable String requiredMessage,
										HorizontalAlignment textAlignment,
										Integer length,
										Converter<?> converter,
										Format<?> format,
										jakarta.faces.convert.Converter<?> facesConverter) {
		EventSourceComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.text(result,
									dataWidgetVar,
									text,
									formDisabledConditionName,
									title,
									requiredMessage,
									textAlignment,
									length,
									converter,
									format,
									facesConverter);
		}
		return result;
	}

	/**
	 * Delegates generic action-link construction.
	 *
	 * @param component the source component
	 * @param dataWidgetBinding data widget binding
	 * @param dataWidgetVar data widget variable
	 * @param value link value
	 * @param iconStyleClass icon style class
	 * @param toolTip tooltip text
	 * @param confirmationText confirmation text
	 * @param link link metadata
	 * @param action action metadata
	 * @return the transformed component
	 */
	@Override
	public UIComponent actionLink(UIComponent component,
									String dataWidgetBinding,
									String dataWidgetVar,
									String value,
									String iconStyleClass,
									String toolTip,
									String confirmationText, 
									Link link,
									Action action) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.actionLink(result, dataWidgetBinding, dataWidgetVar, value, iconStyleClass, toolTip, confirmationText, link, action);
		}
		return result;
	}

	/**
	 * Delegates report action-link construction.
	 *
	 * @param component the source component
	 * @param label the action label
	 * @param iconStyleClass the icon style class
	 * @param toolTip the tooltip text
	 * @param confirmationText the confirmation text
	 * @param action the action metadata
	 * @return the transformed component
	 */
	@Override
	public UIComponent report(UIComponent component,
								String label,
								String iconStyleClass,
								String toolTip,
								String confirmationText, 
								Action action) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.report(result, label, iconStyleClass, toolTip, confirmationText, action);
		}
		return result;
	}

	/**
	 * Delegates download action-link construction.
	 *
	 * @param component the source component
	 * @param dataWidgetBinding the data widget binding
	 * @param dataWidgetVar the data widget variable
	 * @param label the action label
	 * @param iconStyleClass the icon style class
	 * @param toolTip the tooltip text
	 * @param confirmationText the confirmation text
	 * @param action the action metadata
	 * @return the transformed component
	 */
	@Override
	public UIComponent download(UIComponent component,
									String dataWidgetBinding,
									String dataWidgetVar,
									String label,
									String iconStyleClass,
									String toolTip,
									String confirmationText, 
									Action action) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.download(result, dataWidgetBinding, dataWidgetVar, label, iconStyleClass, toolTip, confirmationText, action);
		}
		return result;
	}

	/**
	 * Delegates upload action-link construction.
	 *
	 * @param component the source component
	 * @param label the action label
	 * @param iconStyleClass the icon style class
	 * @param toolTip the tooltip text
	 * @param confirmationText the confirmation text
	 * @param action the action metadata
	 * @return the transformed component
	 */
	@Override
	public UIComponent upload(UIComponent component, 
								String label,
								String iconStyleClass,
								String toolTip,
								String confirmationText, 
								Action action) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.upload(result, label, iconStyleClass, toolTip, confirmationText, action);
		}
		return result;
	}

	/**
	 * Delegates remove action-link construction.
	 *
	 * @param component the source component
	 * @param label the action label
	 * @param iconStyleClass the icon style class
	 * @param toolTip the tooltip text
	 * @param confirmationText the confirmation text
	 * @param action the action metadata
	 * @param canDelete whether delete is permitted
	 * @return the transformed component
	 */
	@Override
	public UIComponent remove(UIComponent component, 
								String label,
								String iconStyleClass,
								String toolTip,
								String confirmationText, 
								Action action,
								boolean canDelete) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.remove(result, label, iconStyleClass, toolTip, confirmationText, action, canDelete);
		}
		return result;
	}

	/**
	 * Delegates implicit action link construction.
	 *
	 * @param component the source component
	 * @param dataWidgetBinding data widget binding
	 * @param dataWidgetVar data widget variable
	 * @param label link label
	 * @param iconStyleClass icon style class
	 * @param toolTip tooltip text
	 * @param confirmationText confirmation text
	 * @param name implicit action name
	 * @param action action metadata
	 * @return the transformed component
	 */
	@Override
	public UIComponent action(UIComponent component,
								String dataWidgetBinding,
								String dataWidgetVar,
								String label,
								String iconStyleClass,
								String toolTip,
								String confirmationText, 
								ImplicitActionName name,
								Action action) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.action(result,
										dataWidgetBinding,
										dataWidgetVar,
										label,
										iconStyleClass,
										toolTip,
										confirmationText,
										name,
										action);
		}
		return result;
	}
}
