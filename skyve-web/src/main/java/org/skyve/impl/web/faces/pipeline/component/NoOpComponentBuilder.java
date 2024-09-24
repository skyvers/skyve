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
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.query.QueryDefinition;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.widget.FilterParameter;
import org.skyve.metadata.view.widget.bound.Parameter;

import jakarta.faces.component.UIComponent;

public class NoOpComponentBuilder extends ComponentBuilder {
	@Override
	public UIComponent view(UIComponent component, boolean createView) {
		return component;
	}

	@Override
	public List<UIComponent> toolbars(List<UIComponent> components, String widgetId) {
		return components;
	}

	@Override
	public UIComponent tabPane(UIComponent component,
								TabPane tabPane,
								String moduleName,
								String documentName) {
		return component;
	}

	@Override
	public UIComponent tab(UIComponent component, String title, Tab tab) {
		return component;
	}

	@Override
	public UIComponent tabPaneScript(UIComponent component,
										TabPane tabPane,
										String moduleName,
										String documentName,
										String tabPaneComponentId) {
		return component;
	}

	@Override
	public UIComponent sidebarScript(UIComponent component,
										Sidebar sidebar,
										boolean createView,
										String sidebarComponentId) {
		return component;
	}
	
	@Override
	public UIComponent border(UIComponent component,
								String title,
								String invisibileConditionName,
								Integer pixelWidth,
								Collapsible collapsible) {
		return component;
	}

	@Override
	public UIComponent label(UIComponent component, String value) {
		return component;
	}

	@Override
	public UIComponent spacer(UIComponent component, Spacer spacer) {
		return component;
	}

	@Override
	public UIComponent zoomIn(UIComponent component, 
										String label,
										String iconStyleClass,
										String toolTip,
										ZoomIn zoomIn,
										String formDisabledConditionName) {
		return component;
	}

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
		return component;
	}

	@Override
	public UIComponent reportButton(UIComponent component,
										String label,
										String iconStyleClass,
										String toolTip,
										String confirmationText, 
										Button button,
										String formDisabledConditionName,
										Action action) {
		return component;
	}

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
		return component;
	}

	@Override
	public UIComponent uploadButton(UIComponent component,
										String label,
										String iconStyleClass,
										String toolTip,
										String confirmationText, 
										Button button,
										String formDisabledConditionName,
										Action action) {
		return component;
	}

	@Override
	public UIComponent staticImage(UIComponent component, String fileUrl, StaticImage image) {
		return component;
	}

	@Override
	public UIComponent dynamicImage(UIComponent component, DynamicImage image, String moduleName, String documentName) {
		return component;
	}

	@Override
	public UIComponent blurb(UIComponent component, String dataWidgetVar, String value, String binding, Blurb blurb) {
		return component;
	}

	@Override
	public UIComponent label(UIComponent component, String dataWidgetVar, String value, String binding, Label label) {
		return component;
	}

	@Override
	public UIComponent dataGrid(UIComponent component, String dataWidgetVar, boolean ordered, DataGrid grid) {
		return component;
	}

	@Override
	public UIComponent dataRepeater(UIComponent component, String dataWidgetVar, DataRepeater repeater) {
		return component;
	}

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
		return component;
	}

	@Override
	public UIComponent addedDataGridBoundColumn(UIComponent component, UIComponent current) {
		return component;
	}

	@Override
	public UIComponent addDataGridContainerColumn(UIComponent component,
													UIComponent current,
													AbstractDataWidget widget,
													String title,
													DataGridContainerColumn column,
													HorizontalAlignment alignment) {
		return component;
	}

	@Override
	public UIComponent addedDataGridContainerColumn(UIComponent component, UIComponent current) {
		return component;
	}

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
		return component;
	}

	@Override
	public UIComponent map(UIComponent component,
							MapDisplay map,
							String moduleName,
							String queryName,
							String geometryBinding) {
		return component;
	}

	@Override
	public UIComponent map(UIComponent component, MapDisplay map, String modelName) {
		return component;
	}

	@Override
	public EventSourceComponent geometry(EventSourceComponent component,
											String dataWidgetVar,
											Geometry geometry,
											String formDisabledConditionName,
											String title,
											boolean required,
											HorizontalAlignment textAlignment) {
		return component;
	}

	@Override
	public EventSourceComponent geometryMap(EventSourceComponent component,
												GeometryMap geometry,
												String formDisabledConditionName,
												String title,
												boolean required) {
		return component;
	}
	
	@Override
	public UIComponent chart(UIComponent component, Chart chart) {
		return component;
	}

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
		return component;
	}
	
	@Override
	public UIComponent listGridContextMenu(UIComponent component, String listGridId, ListGrid listGrid) {
		return component;
	}

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
		return component;
	}

	@Override
	public EventSourceComponent listMembership(EventSourceComponent component,
												String candidatesHeading,
												String membersHeading,
												ListMembership membership) {
		return component;
	}

	@Override
	public EventSourceComponent checkBox(EventSourceComponent component,
											String dataWidgetVar,
											CheckBox checkBox,
											String formDisabledConditionName,
											String title,
											boolean required) {
		return component;
	}

	@Override
	public EventSourceComponent colourPicker(EventSourceComponent component,
												String dataWidgetVar,
												ColourPicker colour,
												String formDisabledConditionName,
												String title,
												boolean required,
												HorizontalAlignment textAlignment) {
		return component;
	}

	@Override
	public EventSourceComponent combo(EventSourceComponent component,
										String dataWidgetVar,
										Combo combo,
										String formDisabledConditionName,
										String title,
										boolean required) {
		return component;
	}

	@Override
	public UIComponent contentImage(UIComponent component,
										String dataWidgetVar,
										ContentImage image,
										String formDisabledConditionName,
										String title,
										boolean required) {
		return component;
	}

	@Override
	public UIComponent contentLink(UIComponent component,
									String dataWidgetVar,
									ContentLink link,
									String formDisabledConditionName,
									String title,
									boolean required,
									HorizontalAlignment textAlignment) {
		return component;
	}

	@Override
	public UIComponent addContentSignature(UIComponent component,
												UIComponent layout,
												ContentSignature signature,
												String formDisabledConditionName,
												String title,
												boolean required) {
		return component;
	}

	@Override
	public UIComponent html(UIComponent component,
								String dataWidgetVar,
								HTML html,
								String formDisabledConditionName,
								String title,
								boolean required) {
		return component;
	}

	@Override
	public EventSourceComponent lookupDescription(EventSourceComponent component,
													String dataWidgetVar,
													LookupDescription lookup,
													String formDisabledConditionName,
													String title,
													boolean required,
													HorizontalAlignment textAlignment,
													String displayBinding,
													QueryDefinition query) {
		return component;
	}

	@Override
	public EventSourceComponent password(EventSourceComponent component,
											String dataWidgetVar,
											Password password,
											String formDisabledConditionName,
											String title,
											boolean required,
											HorizontalAlignment textAlignment) {
		return component;
	}

	@Override
	public EventSourceComponent radio(EventSourceComponent component,
										String dataWidgetVar,
										Radio radio,
										String formDisabledConditionName,
										String title, boolean required) {
		return component;
	}

	@Override
	public EventSourceComponent richText(EventSourceComponent component,
											String dataWidgetVar,
											RichText text,
											String formDisabledConditionName,
											String title,
											boolean required) {
		return component;
	}

	@Override
	public EventSourceComponent spinner(EventSourceComponent component,
											String dataWidgetVar,
											Spinner spinner,
											String formDisabledConditionName,
											String title,
											boolean required,
											HorizontalAlignment textAlignment,
											jakarta.faces.convert.Converter<?> facesConverter) {
		return component;
	}

	@Override
	public EventSourceComponent slider(EventSourceComponent component,
											String dataWidgetVar,
											Slider spinner,
											String formDisabledConditionName,
											String title,
											boolean required,
											jakarta.faces.convert.Converter<?> facesConverter) {
		return component;
	}

	@Override
	public EventSourceComponent textArea(EventSourceComponent component,
											String dataWidgetVar,
											TextArea text,
											String formDisabledConditionName,
											String title,
											boolean required,
											HorizontalAlignment textAlignment,
											Integer length) {
		return component;
	}

	@Override
	public EventSourceComponent text(EventSourceComponent component,
										String dataWidgetVar,
										TextField text,
										String formDisabledConditionName,
										String title,
										boolean required,
										HorizontalAlignment textAlignment,
										Integer length,
										Converter<?> converter,
										Format<?> format,
										jakarta.faces.convert.Converter<?> facesConverter) {
		return component;
	}

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
		return component;
	}

	@Override
	public UIComponent report(UIComponent component,
								String label,
								String iconStyleClass,
								String toolTip,
								String confirmationText, 
								Action action) {
		return component;
	}

	@Override
	public UIComponent download(UIComponent component,
									String dataWidgetBinding,
									String dataWidgetVar,
									String label,
									String iconStyleClass,
									String toolTip,
									String confirmationText, 
									Action action) {
		return component;
	}

	@Override
	public UIComponent upload(UIComponent component,
								String label,
								String iconStyleClass,
								String toolTip,
								String confirmationText, 
								Action action) {
		return component;
	}

	@Override
	public UIComponent remove(UIComponent component,
								String label,
								String iconStyleClass,
								String toolTip,
								String confirmationText, 
								Action action,
								boolean canDelete) {
		return component;
	}

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
		return component;
	}
}
