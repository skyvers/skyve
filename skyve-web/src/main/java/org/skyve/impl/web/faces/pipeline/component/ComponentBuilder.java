package org.skyve.impl.web.faces.pipeline.component;

import java.util.List;
import java.util.Map;

import javax.el.MethodExpression;
import javax.faces.component.UICommand;
import javax.faces.component.UIComponent;
import javax.faces.component.UIComponentBase;
import javax.faces.component.UIOutput;
import javax.faces.component.html.HtmlOutputLink;

import org.primefaces.behavior.ajax.AjaxBehavior;
import org.primefaces.behavior.ajax.AjaxBehaviorListenerImpl;
import org.skyve.domain.Bean;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
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
import org.skyve.impl.metadata.view.widget.DynamicImage;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.widget.MapDisplay;
import org.skyve.impl.metadata.view.widget.Spacer;
import org.skyve.impl.metadata.view.widget.StaticImage;
import org.skyve.impl.metadata.view.widget.bound.Label;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;
import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;
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
import org.skyve.impl.web.faces.pipeline.AbstractFacesBuilder;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.query.QueryDefinition;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.TextOutput.Sanitisation;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.widget.FilterParameter;
import org.skyve.metadata.view.widget.bound.Parameter;

public abstract class ComponentBuilder extends AbstractFacesBuilder {
	public static final String COLLECTION_BINDING_ATTRIBUTE_KEY = "collectionBinding";

	public static class EventSourceComponent {
		private UIComponent component;
		private UIComponentBase eventSource;
		
		public EventSourceComponent(UIComponent component, UIComponentBase eventSource) {
			super();
			this.component = component;
			this.eventSource = eventSource;
		}

		public UIComponent getComponent() {
			return component;
		}

		public UIComponentBase getEventSource() {
			return eventSource;
		}
	}
	
	/**
	 * Used to create a visible/invisible panel for a view based to switch between create and edit views.
	 * @param invisibleConditionName
	 * @return
	 */
	public abstract UIComponent view(UIComponent component, String invisibleConditionName);

	/**
	 * 
	 * @return	The toolbar components, or null if there is no toolbar components required for this renderer.
	 */
	public abstract List<UIComponent> toolbars(List<UIComponent> components, String widgetId);
	
	/**
	 * 
	 * @param invisible
	 * @return
	 */
	public abstract UIComponent tabPane(UIComponent component,
											TabPane tabPane,
											String moduleName,
											String documentName,
											StringBuilder stickyTabScript);
	
	public abstract UIComponent tab(UIComponent component, String title, Tab tab);
	
	public abstract UIComponent border(UIComponent component,
										String title,
										String invisibileConditionName,
										Integer pixelWidth);
	public abstract UIComponent label(UIComponent component, String value);
	
	public abstract UIComponent spacer(UIComponent component, Spacer spacer);
	
	public abstract UIComponent actionButton(UIComponent component,
												String dataWidgetBinding, 
												String dataWidgetVar, 
												String label,
												String iconStyleClass,
												String toolTip,
												String confirmationText, 
												Button button, 
												String formDisabledConditionName,
												Action action);
	public abstract UIComponent reportButton(UIComponent component, 
												String label,
												String iconStyleClass,
												String toolTip,
												String confirmationText, 
												Button button, 
												String formDisabledConditionName,
												Action action);
	public abstract UIComponent downloadButton(UIComponent component,
												String dataWidgetBinding, 
												String dataWidgetVar, 
												String label,
												String iconStyleClass,
												String toolTip,
												String confirmationText, 
												Button button, 
												String formDisabledConditionName,
												Action action);
	public abstract UIComponent uploadButton(UIComponent component, 
												String label,
												String iconStyleClass,
												String toolTip,
												String confirmationText, 
												Button button, 
												String formDisabledConditionName,
												Action action);

	public abstract UIComponent staticImage(UIComponent component, String fileUrl, StaticImage image);
	public abstract UIComponent dynamicImage(UIComponent component, 
												DynamicImage image, 
												String moduleName, 
												String documentName);
	
	public abstract UIComponent blurb(UIComponent component, 
										String dataWidgetVar, 
										String value, 
										String binding, 
										Blurb blurb);
	public abstract UIComponent label(UIComponent component,
										String dataWidgetVar,
										String value,
										String binding,
										Label label);

	public abstract UIComponent dataGrid(UIComponent component,
											String dataWidgetVar,
											boolean ordered,
											String title,
											DataGrid grid);

	/*
	 * Data Repeater is just like a data grid but...
	 * The grid column headers can be turned off.
	 * The grid (borders) can be turned off.
	 * Any bound columns are editable inline.
	 * There is no action column.
	 */
	public abstract UIComponent dataRepeater(UIComponent component,
												String dataWidgetVar,
												String title,
												DataRepeater repeater);
	public abstract UIComponent addDataGridBoundColumn(UIComponent component,
														UIComponent current, 
														AbstractDataWidget widget,
														DataGridBoundColumn column,
														String dataWidgetVar,
														String columnTitle,
														String columnBinding,
														StringBuilder gridColumnExpression);
	public abstract UIComponent addedDataGridBoundColumn(UIComponent component, UIComponent current);
	public abstract UIComponent addDataGridContainerColumn(UIComponent component,
															UIComponent current,
															AbstractDataWidget widget,
															String title,
															DataGridContainerColumn column);
	public abstract UIComponent addedDataGridContainerColumn(UIComponent component, UIComponent current);
	public abstract UIComponent addDataGridActionColumn(UIComponent component,
															UIComponent current, 
															DataGrid grid,
															String dataWidgetVar,
															String gridColumnExpression,
															String singluarDocumentAlias,
															boolean inline);
	
	public abstract UIComponent listGrid(UIComponent component,
											String moduleName,
											String modelDocumentName,
											String modelName,
											ListModel<? extends Bean> model,
											Document owningDocument,
											String title,
											ListGrid listGrid,
											boolean canCreateDocument,
											boolean aggregateQuery);

	/*
	 * List Repeater is just like a list grid but...
	 * The grid column headers can be turned off.
	 * The grid (borders) can be turned off.
	 * It implements infinite scrolling instead of the page controls.
	 * Any bound columns are editable inline.
	 * There is no action column.
	 * No CRUD.
	 */
	public abstract UIComponent listRepeater(UIComponent component,
												String modelDocumentName,
												String modelName,
												ListModel<? extends Bean> model, 
												List<FilterParameter> filterParameters,
												List<Parameter> parameters,
												String title,
												boolean showColumnHeaders,
												boolean showGrid);

	public abstract UIComponent map(UIComponent component, 
										MapDisplay map,
										String moduleName,
										String queryName,
										String geometryBinding);

	public abstract UIComponent map(UIComponent component, MapDisplay map, String modelName);
	
	public abstract EventSourceComponent geometry(EventSourceComponent component, 
													String dataWidgetVar, 
													Geometry geometry, 
													String formDisabledConditionName,
													String title, 
													boolean required);

	public abstract EventSourceComponent geometryMap(EventSourceComponent component, 
														GeometryMap geometry, 
														String formDisabledConditionName,
														String title, 
														boolean required);

	public abstract UIComponent chart(UIComponent component, Chart chart);

	public abstract EventSourceComponent listMembership(EventSourceComponent component,
															String candidatesHeading,
															String membersHeading,
															ListMembership membership);
	
	public abstract EventSourceComponent checkBox(EventSourceComponent component, 
													String dataWidgetVar, 
													CheckBox checkBox, 
													String formDisabledConditionName,
													String title, 
													boolean required);

	public abstract EventSourceComponent colourPicker(EventSourceComponent component, 
														String dataWidgetVar, 
														ColourPicker colour, 
														String formDisabledConditionName,
														String title, 
														boolean required);
	
	public abstract EventSourceComponent combo(EventSourceComponent component, 
												String dataWidgetVar, 
												Combo combo, 
												String formDisabledConditionName,
												String title, 
												boolean required);

	public abstract UIComponent contentImage(UIComponent component, 
												String dataWidgetVar, 
												ContentImage image, 
												String formDisabledConditionName,
												String title, 
												boolean required);

	public abstract UIComponent contentLink(UIComponent component, 
												String dataWidgetVar, 
												ContentLink link, 
												String formDisabledConditionName,
												String title, 
												boolean required);
	
	public abstract UIComponent html(UIComponent component, 
										String dataWidgetVar, 
										HTML html, 
										String formDisabledConditionName,
										String title, 
										boolean required);

	public abstract EventSourceComponent lookupDescription(EventSourceComponent component, 
															String dataWidgetVar,
															LookupDescription lookup,
															String formDisabledConditionName,
															String title,
															boolean required,
															String displayBinding,
															QueryDefinition query);
	
	public abstract EventSourceComponent password(EventSourceComponent component, 
													String dataWidgetVar,
													Password password,
													String formDisabledConditionName,
													String title,
													boolean required);

	public abstract EventSourceComponent radio(EventSourceComponent component, 
												String dataWidgetVar,
												Radio radio,
												String formDisabledConditionName,
												String title,
												boolean required);
	
	public abstract EventSourceComponent richText(EventSourceComponent component, 
													String dataWidgetVar,
													RichText text,
													String formDisabledConditionName,
													String title,
													boolean required);
	
	public abstract EventSourceComponent spinner(EventSourceComponent component, 
													String dataWidgetVar,
													Spinner spinner,
													String formDisabledConditionName,
													String title,
													boolean required,
													javax.faces.convert.Converter facesConverter);
	
	public abstract EventSourceComponent text(EventSourceComponent component, 
												String dataWidgetVar, 
												TextField text, 
												String formDisabledConditionName,
												String title, 
												boolean required,
												Integer length,
												Converter<?> converter,
												Format<?> format,
												javax.faces.convert.Converter facesConverter);

	public abstract EventSourceComponent textArea(EventSourceComponent component, 
													String dataWidgetVar,
													TextArea text,
													String formDisabledConditionName,
													String title,
													boolean required,
													Integer length);
	
	public HtmlOutputLink outputLink(String dataWidgetVar, 
										String value, 
										String href, 
										String invisible,
										ReferenceTarget target) {
		HtmlOutputLink result = (HtmlOutputLink) a.createComponent(HtmlOutputLink.COMPONENT_TYPE);
		if (dataWidgetVar != null) {
			result.setValueExpression("value", createValueExpressionFromFragment(dataWidgetVar, true, href, true, null, String.class, false, Sanitisation.relaxed));
		}
		else {
			result.setValueExpression("value", createValueExpressionFromFragment(href, true, null, String.class, false, Sanitisation.relaxed));
		}
		if (value != null) {
			UIOutput outputText = (UIOutput) a.createComponent(UIOutput.COMPONENT_TYPE);
			outputText.setValue(value);
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
	
	protected static ActionFacesAttributes determineActionFacesAttributes(List<EventAction> actions) {
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
			if (action instanceof ServerSideActionEventAction) {
				result.actionName = ((ServerSideActionEventAction) action).getActionName();
				break;
			}
			else if (action instanceof RerenderEventAction) {
				rerenderValidate = ! Boolean.FALSE.equals(((RerenderEventAction) action).getClientValidation());
				result.actionName = String.valueOf(rerenderValidate);
				break;
			}
		}
		
		return result;
	}
	
	public void addAjaxBehavior(UIComponentBase component, 
									String eventName,
									String collectionBinding,
									String dataWidgetVar,
									String rerenderSource, 
									List<EventAction> actions) {
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

	public abstract UIComponent actionLink(UIComponent component,
											   String dataWidgetBinding,
											   String dataWidgetVar,
											   String value,
											   Link link,
											   Action action);

	public abstract UIComponent report(UIComponent component, Action action);
	
	public abstract UIComponent download(UIComponent component, 
											String dataWidgetBinding,
											String dataWidgetVar,
											Action action);
	
	public abstract UIComponent upload(UIComponent component, Action action);

	public abstract UIComponent action(UIComponent component, 
										String dataWidgetBinding, 
										String dataWidgetVar,
										Action action, 
										ImplicitActionName name, 
										String title);
	
	static final Class<?>[] STRING_STRING = new Class<?>[] {String.class, String.class};
	static final Class<?>[] STRING_STRING_STRING = new Class<?>[] {String.class, String.class, String.class};
	static final Class<?>[] STRING_STRING_LIST = new Class<?>[] {String.class, String.class, List.class};
	static final Class<?>[] STRING_BOOLEAN = new Class<?>[] {String.class, Boolean.class};
	static final Class<?>[] NONE = new Class[0];
	
	protected MethodExpression methodExpressionForRerender(String source, boolean validate) {
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

	protected void downloadActionExpression(String downloadActionName,
											String dataWidgetBinding,
											String dataWidgetVar,
											UICommand command) {
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

	protected MethodExpression methodExpressionForAction(ImplicitActionName implicitActionName, 
															String actionName,
															String collectionBinding, 
															String dataWidgetVar,
															boolean inline,
															List<String> eventHandlerActionNames) {
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
