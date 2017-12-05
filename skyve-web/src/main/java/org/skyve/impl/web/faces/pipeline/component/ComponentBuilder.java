package org.skyve.impl.web.faces.pipeline.component;

import java.util.List;
import java.util.Map;

import javax.el.MethodExpression;
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
import org.skyve.impl.metadata.view.widget.DynamicImage;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.widget.Spacer;
import org.skyve.impl.metadata.view.widget.StaticImage;
import org.skyve.impl.metadata.view.widget.bound.Label;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;
import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.ListMembership;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.Password;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.impl.metadata.view.widget.bound.input.Spinner;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridContainerColumn;
import org.skyve.impl.web.faces.pipeline.AbstractFacesBuilder;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.module.query.QueryDefinition;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.widget.bound.FilterParameter;

public abstract class ComponentBuilder extends AbstractFacesBuilder {
	/**
	 * Used to create a visible/invisible panel for a view based to switch between create and edit views.
	 * @param invisibleConditionName
	 * @return
	 */
	public abstract UIComponent view(String invisibleConditionName);

	/**
	 * 
	 * @return	The toolbar components, or null if there is no toolbar components required for this renderer.
	 */
	public abstract List<UIComponent> toolbars(String widgetId);
	
	/**
	 * 
	 * @param invisible
	 * @return
	 */
	public abstract UIComponent tabPane(TabPane tabPane);
	
	public abstract UIComponent tab(Tab tab);
	
	public abstract UIComponent border(String title,
										String invisibileConditionName,
										Integer pixelWidth);
	public abstract UIComponent label(String value);
	
	public abstract UIComponent spacer(Spacer spacer);
	
	public abstract UIComponent actionButton(String listBinding, String listVar, Button button, Action action);
	public abstract UIComponent reportButton(Button button, Action action);
	public abstract UIComponent downloadButton(Button button, Action action, String moduleName, String documentName);
	
	public abstract UIComponent staticImage(StaticImage image);
	public abstract UIComponent dynamicImage(DynamicImage image, String moduleName, String documentName);
	
	public abstract UIComponent blurb(String listVar, String value, String binding, Blurb blurb);
	public abstract UIComponent label(String listVar, String value, String binding, Label label);

	public abstract UIComponent dataGrid(String listVar, DataGrid grid);
	public abstract UIComponent addDataGridBoundColumn(UIComponent current, 
														DataGrid grid,
														DataGridBoundColumn column,
														String listVar,
														String columnTitle,
														String columnBinding,
														StringBuilder gridColumnExpression);
	public abstract UIComponent addedDataGridBoundColumn(UIComponent current);
	public abstract UIComponent addDataGridContainerColumn(UIComponent current,
															DataGrid grid,
															DataGridContainerColumn column);
	public abstract UIComponent addedDataGridContainerColumn(UIComponent current);
	public abstract UIComponent addDataGridActionColumn(UIComponent current, 
															DataGrid grid,
															String listVar,
															String gridColumnExpression,
															String singluarDocumentAlias,
															boolean inline);
	
	public abstract UIComponent listGrid(String modelDocumentName,
											String modelName,
											ListModel<? extends Bean> model, 
											List<FilterParameter> filterParameters,
											boolean canCreateDocument,
											boolean createRendered,
											String[] createDisabledConditionNames,
											boolean zoomRendered,
											String zoomDisabledConditionName,
											String selectedIdBinding,
											List<EventAction> selectedActions,
											boolean showPaginator,
											boolean stickyHeader);

	public abstract UIComponent listMembership(ListMembership membership);
	
	public abstract UIComponent checkBox(String listVar, CheckBox checkBox, String title, boolean required);

	public abstract UIComponent colourPicker(String listVar, ColourPicker colour, String title, boolean required);
	
	public abstract UIComponent combo(String listVar, Combo combo, String title, boolean required);

	public abstract UIComponent contentImage(String listVar, ContentImage image, String title, boolean required);

	public abstract UIComponent contentLink(String listVar, ContentLink link, String title, boolean required);
	
	public abstract UIComponent html(String listVar, HTML html, String title, boolean required);

	public abstract UIComponent lookupDescription(String listVar,
													LookupDescription lookup,
													String title,
													boolean required,
													String displayBinding,
													QueryDefinition query);
	
	public abstract UIComponent password(String listVar, Password password, String title, boolean required);

	public abstract UIComponent radio(String listVar, Radio radio, String title, boolean required);
	
	public abstract UIComponent richText(String listVar, RichText text, String title, boolean required);
	
	public abstract UIComponent spinner(String listVar, Spinner spinner, String title, boolean required);
	
	public abstract UIComponent textArea(String listVar, TextArea text, String title, boolean required, Integer length);
	
	public abstract UIComponent text(String listVar, 
										TextField text, 
										String title, 
										boolean required,
										Integer length,
										Converter<?> converter,
										Format<?> format,
										javax.faces.convert.Converter facesConverter);
	
	public HtmlOutputLink outputLink(String listVar, 
										String value, 
										String href, 
										String invisible,
										ReferenceTarget target) {
		HtmlOutputLink result = (HtmlOutputLink) a.createComponent(HtmlOutputLink.COMPONENT_TYPE);
		if (listVar != null) {
			result.setValueExpression("value", createValueExpressionFromFragment(listVar, true, href, true, null, String.class));
		}
		else {
			result.setValueExpression("value", createValueExpressionFromFragment(href, true, null, String.class));
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
									String listVar,
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
																listVar, 
																false, 
																null);
			ajax.addAjaxBehaviorListener(new AjaxBehaviorListenerImpl(me, me));
		}

		component.addClientBehavior(eventName, ajax);
	}

	public abstract UIComponent actionLink(String listBinding, String listVar, Link link, String actionName);

	public abstract UIComponent report(Action action);
	public abstract UIComponent download(Action action, String moduleName, String documentName);
	
	public abstract UIComponent action(String listBinding, 
										String listVar,
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
	
	protected MethodExpression methodExpressionForAction(ImplicitActionName implicitActionName, 
															String actionName,
															String collectionBinding, 
															String listVar,
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
					expression.append(listVar).append("['").append(Bean.DOCUMENT_ID).append("'],");

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
					expression.append(listVar).append("['").append(Bean.DOCUMENT_ID).append("'])");
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
				expression.append(listVar).append("['").append(Bean.DOCUMENT_ID).append("'])");
			} 
			else {
				expression.append(", null, null)");
			}
		}
		expression.append('}');

		return ef.createMethodExpression(elc, expression.toString(), null, parameterTypes);
	}
}
