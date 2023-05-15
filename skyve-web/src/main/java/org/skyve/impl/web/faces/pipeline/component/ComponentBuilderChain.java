package org.skyve.impl.web.faces.pipeline.component;

import java.util.List;

import javax.faces.component.UIComponent;

import org.skyve.domain.Bean;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.impl.metadata.view.HorizontalAlignment;
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
import org.skyve.impl.metadata.view.widget.bound.input.Spinner;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.widget.bound.tabular.AbstractDataWidget;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridContainerColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.web.faces.beans.FacesView;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.query.QueryDefinition;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.widget.FilterParameter;
import org.skyve.metadata.view.widget.bound.Parameter;
import org.skyve.web.UserAgentType;

/**
 * Delegates to a list of other builders that actually produce the components.
 */
public class ComponentBuilderChain extends ComponentBuilder {
	private ComponentBuilder[] builders;
	
	public ComponentBuilderChain(ComponentBuilder... builders) {
		this.builders = builders;
	}
	
	@Override
	public void setManagedBeanName(String managedBeanName) {
		// Set the state of the chain too so that utility methods in AbstractFacesBuilder can work
		super.setManagedBeanName(managedBeanName);
		// Now set the state on all builders in the chain
		for (ComponentBuilder builder : builders) {
			builder.setManagedBeanName(managedBeanName);
		}
	}
	
	@Override
	public void setSAILManagedBean(FacesView<?> managedBean) {
		// Set the state of the chain too so that utility methods in AbstractFacesBuilder can work
		super.setSAILManagedBean(managedBean);
		// Now set the state on all builders in the chain
		for (ComponentBuilder builder : builders) {
			builder.setSAILManagedBean(managedBean);
		}
	}

	@Override
	public void setProcess(String process) {
		// Set the state of the chain too so that utility methods in AbstractFacesBuilder can work
		super.setProcess(process);
		// Now set the state on all builders in the chain
		for (ComponentBuilder builder : builders) {
			builder.setProcess(process);
		}
	}

	@Override
	public void setUpdate(String update) {
		// Set the state of the chain too so that utility methods in AbstractFacesBuilder can work
		super.setUpdate(update);
		// Now set the state on all builders in the chain
		for (ComponentBuilder builder : builders) {
			builder.setUpdate(update);
		}
	}

	@Override
	public void setUserAgentType(UserAgentType userAgentType) {
		// Set the state of the chain too so that utility methods in AbstractFacesBuilder can work
		super.setUserAgentType(userAgentType);
		// Now set the state on all builders in the chain
		for (ComponentBuilder builder : builders) {
			builder.setUserAgentType(userAgentType);
		}
	}
	
	@Override
	public UIComponent view(UIComponent component, String invisibleConditionName) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.view(result, invisibleConditionName);
		}
		return result;
	}

	@Override
	public List<UIComponent> toolbars(List<UIComponent> components, String widgetId) {
		List<UIComponent> result = components;
		for (ComponentBuilder builder : builders) {
			result = builder.toolbars(result, widgetId);
		}
		return result;
	}

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

	@Override
	public UIComponent tab(UIComponent component, String title, Tab tab) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.tab(result, title, tab);
		}
		return result;
	}

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
	
	@Override
	public UIComponent border(UIComponent component, String title, String invisibileConditionName, Integer pixelWidth) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.border(result, title, invisibileConditionName, pixelWidth);
		}
		return result;
	}

	@Override
	public UIComponent label(UIComponent component, String value) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.label(result, value);
		}
		return result;
	}

	@Override
	public UIComponent spacer(UIComponent component, Spacer spacer) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.spacer(result, spacer);
		}
		return result;
	}

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

	@Override
	public UIComponent staticImage(UIComponent component, String fileUrl, StaticImage image) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.staticImage(result, fileUrl, image);
		}
		return result;
	}

	@Override
	public UIComponent dynamicImage(UIComponent component, DynamicImage image, String moduleName, String documentName) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.dynamicImage(result, image, moduleName, documentName);
		}
		return result;
	}

	@Override
	public UIComponent blurb(UIComponent component, String dataWidgetVar, String value, String binding, Blurb blurb) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.blurb(result, dataWidgetVar, value, binding, blurb);
		}
		return result;
	}

	@Override
	public UIComponent label(UIComponent component, String dataWidgetVar, String value, String binding, Label label) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.label(result, dataWidgetVar, value, binding, label);
		}
		return result;
	}

	@Override
	public UIComponent dataGrid(UIComponent component, String dataWidgetVar, boolean ordered, String title, DataGrid grid) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.dataGrid(result, dataWidgetVar, ordered, title, grid);
		}
		return result;
	}

	@Override
	public UIComponent dataRepeater(UIComponent component, String dataWidgetVar, String title, DataRepeater repeater) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.dataRepeater(result, dataWidgetVar, title, repeater);
		}
		return result;
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
												HorizontalAlignment alignment) {
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
														alignment);
		}
		return result;
	}

	@Override
	public UIComponent addedDataGridBoundColumn(UIComponent component, UIComponent current) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.addedDataGridBoundColumn(result, current);
		}
		return result;
	}

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

	@Override
	public UIComponent addedDataGridContainerColumn(UIComponent component, UIComponent current) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.addedDataGridContainerColumn(result, current);
		}
		return result;
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

	@Override
	public UIComponent map(UIComponent component, MapDisplay map, String modelName) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.map(result, map, modelName);
		}
		return result;
	}
	
	@Override
	public EventSourceComponent geometry(EventSourceComponent component,
											String dataWidgetVar,
											Geometry geometry,
											String formDisabledConditionName,
											String title,
											boolean required) {
		EventSourceComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.geometry(result, dataWidgetVar, geometry, formDisabledConditionName, title, required);
		}
		return result;
	}

	@Override
	public EventSourceComponent geometryMap(EventSourceComponent component,
												GeometryMap geometry,
												String formDisabledConditionName,
												String title,
												boolean required) {
		EventSourceComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.geometryMap(result, geometry, formDisabledConditionName, title, required);
		}
		return result;
	}
	
	@Override
	public UIComponent chart(UIComponent component, Chart chart) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.chart(result, chart);
		}
		return result;
	}
	
	@Override
	public UIComponent listGrid(UIComponent component,
									String moduleName,
									String modelDocumentName,
									String modelName,
									ListModel<Bean> model,
									Document owningDocument,
									String title,
									ListGrid listGrid,
									boolean aggregateQuery) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.listGrid(result,
										moduleName,
										modelDocumentName,
										modelName,
										model,
										owningDocument,
										title,
										listGrid,
										aggregateQuery);
		}
		return result;
	}

	@Override
	public UIComponent listRepeater(UIComponent component,
										String modelDocumentName,
										String modelName,
										ListModel<Bean> model,
										List<FilterParameter> filterParameters,
										List<Parameter> parameters,
										String title,
										boolean showColumnHeaders,
										boolean showGrid) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.listRepeater(result,
											modelDocumentName,
											modelName,
											model,
											filterParameters,
											parameters,
											title,
											showColumnHeaders,
											showGrid);
		}
		return result;
	}

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

	@Override
	public EventSourceComponent checkBox(EventSourceComponent component,
											String dataWidgetVar,
											CheckBox checkBox,
											String formDisabledConditionName,
											String title,
											boolean required) {
		EventSourceComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.checkBox(result,
										dataWidgetVar,
										checkBox,
										formDisabledConditionName,
										title,
										required);
		}
		return result;
	}

	@Override
	public EventSourceComponent colourPicker(EventSourceComponent component,
												String dataWidgetVar,
												ColourPicker colour,
												String formDisabledConditionName,
												String title,
												boolean required) {
		EventSourceComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.colourPicker(result,
											dataWidgetVar,
											colour,
											formDisabledConditionName,
											title,
											required);
		}
		return result;
	}

	@Override
	public EventSourceComponent combo(EventSourceComponent component,
										String dataWidgetVar,
										Combo combo,
										String formDisabledConditionName,
										String title,
										boolean required) {
		EventSourceComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.combo(result, dataWidgetVar, combo, formDisabledConditionName,  title, required);
		}
		return result;
	}

	@Override
	public UIComponent contentImage(UIComponent component,
										String dataWidgetVar,
										ContentImage image,
										String formDisabledConditionName,
										String title,
										boolean required) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.contentImage(result, dataWidgetVar, image, formDisabledConditionName, title, required);
		}
		return result;
	}

	@Override
	public UIComponent contentLink(UIComponent component,
									String dataWidgetVar,
									ContentLink link,
									String formDisabledConditionName,
									String title,
									boolean required) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.contentLink(result, dataWidgetVar, link, formDisabledConditionName, title, required);
		}
		return result;
	}

	@Override
	public UIComponent addContentSignature(UIComponent component,
											UIComponent layout,
											ContentSignature signature,
											String formDisabledConditionName,
											String title,
											boolean required) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.addContentSignature(result, layout, signature, formDisabledConditionName, title, required);
		}
		return result;
	}

	@Override
	public UIComponent html(UIComponent component,
								String dataWidgetVar,
								HTML html,
								String formDisabledConditionName,
								String title,
								boolean required) {
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.html(result, dataWidgetVar, html, formDisabledConditionName, title, required);
		}
		return result;
	}

	@Override
	public EventSourceComponent lookupDescription(EventSourceComponent component,
													String dataWidgetVar,
													LookupDescription lookup,
													String formDisabledConditionName,
													String title,
													boolean required,
													String displayBinding,
													QueryDefinition query) {
		EventSourceComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.lookupDescription(result,
												dataWidgetVar,
												lookup,
												formDisabledConditionName,
												title,
												required,
												displayBinding,
												query);
		}
		return result;
	}

	@Override
	public EventSourceComponent password(EventSourceComponent component,
											String dataWidgetVar,
											Password password,
											String formDisabledConditionName,
											String title,
											boolean required) {
		EventSourceComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.password(result, dataWidgetVar, password, formDisabledConditionName, title, required);
		}
		return result;
	}

	@Override
	public EventSourceComponent radio(EventSourceComponent component,
										String dataWidgetVar,
										Radio radio,
										String formDisabledConditionName,
										String title,
										boolean required) {
		EventSourceComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.radio(result, dataWidgetVar, radio, formDisabledConditionName, title, required);
		}
		return result;
	}

	@Override
	public EventSourceComponent richText(EventSourceComponent component,
											String dataWidgetVar,
											RichText text,
											String formDisabledConditionName,
											String title,
											boolean required) {
		EventSourceComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.richText(result, dataWidgetVar, text, formDisabledConditionName, title, required);
		}
		return result;
	}

	@Override
	public EventSourceComponent spinner(EventSourceComponent component,
											String dataWidgetVar,
											Spinner spinner,
											String formDisabledConditionName,
											String title,
											boolean required,
											javax.faces.convert.Converter facesConverter) {
		EventSourceComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.spinner(result, dataWidgetVar, spinner, formDisabledConditionName, title, required, facesConverter);
		}
		return result;
	}

	@Override
	public EventSourceComponent textArea(EventSourceComponent component,
											String dataWidgetVar,
											TextArea text,
											String formDisabledConditionName,
											String title,
											boolean required,
											Integer length) {
		EventSourceComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.textArea(result, dataWidgetVar, text, formDisabledConditionName, title, required, length);
		}
		return result;
	}

	@Override
	public EventSourceComponent text(EventSourceComponent component,
										String dataWidgetVar,
										TextField text,
										String formDisabledConditionName,
										String title,
										boolean required,
										Integer length,
										Converter<?> converter,
										Format<?> format,
										javax.faces.convert.Converter facesConverter) {
		EventSourceComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.text(result,
									dataWidgetVar,
									text,
									formDisabledConditionName,
									title,
									required,
									length,
									converter,
									format,
									facesConverter);
		}
		return result;
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
		UIComponent result = component;
		for (ComponentBuilder builder : builders) {
			result = builder.actionLink(result, dataWidgetBinding, dataWidgetVar, value, iconStyleClass, toolTip, confirmationText, link, action);
		}
		return result;
	}

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
