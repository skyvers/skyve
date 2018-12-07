package org.skyve.impl.generate.pwa;

import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.event.EventAction;
import org.skyve.impl.metadata.view.reference.ReferenceTarget;
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
import org.skyve.impl.metadata.view.widget.bound.tabular.AbstractDataWidget;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridContainerColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.module.query.QueryDefinition;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.widget.bound.FilterParameter;

public abstract class ComponentRenderer extends AbstractRenderer {
	/**
	 * Used to create a visible/invisible panel for a view based to switch between create and edit views.
	 * @param invisibleConditionName
	 * @return
	 */
	public abstract RenderedComponent view(RenderedComponent component, String invisibleConditionName);

	/**
	 * 
	 * @return	The toolbar components, or null if there is no toolbar components required for this renderer.
	 */
	public abstract List<RenderedComponent> toolbars(List<RenderedComponent> components, String widgetId);

	/**
	 * 
	 * @param invisible
	 * @return
	 */
	public abstract RenderedComponent tabPane(RenderedComponent component,
												TabPane tabPane,
												String moduleName,
												String documentName,
												StringBuilder stickyTabScript);
	
	public abstract RenderedComponent tab(RenderedComponent component, Tab tab);
	
	public abstract RenderedComponent border(RenderedComponent component,
												String title,
												String invisibileConditionName,
												Integer pixelWidth);
	public abstract RenderedComponent label(RenderedComponent component, String value);
	
	public abstract RenderedComponent spacer(RenderedComponent component, Spacer spacer);
	
	public abstract RenderedComponent actionButton(RenderedComponent component,
													String listBinding, 
													String listVar, 
													Button button, 
													Action action);
	public abstract RenderedComponent reportButton(RenderedComponent component, 
												Button button, 
												Action action);
	public abstract RenderedComponent downloadButton(RenderedComponent component,
														Button button, 
														Action action, 
														String moduleName, 
														String documentName);
	
	public abstract RenderedComponent staticImage(RenderedComponent component, StaticImage image);
	public abstract RenderedComponent dynamicImage(RenderedComponent component, 
													DynamicImage image, 
													String moduleName, 
													String documentName);
	
	public abstract RenderedComponent blurb(RenderedComponent component, 
												String listVar, 
												String value, 
												String binding, 
												Blurb blurb);
	public abstract RenderedComponent label(RenderedComponent component,
												String listVar,
												String value,
												String binding,
												Label label);

	public abstract RenderedComponent dataGrid(RenderedComponent component, String listVar, boolean ordered, DataGrid grid);

	/*
	 * Data Repeater is just like a data grid but...
	 * The grid column headers can be turned off.
	 * The grid (borders) can be turned off.
	 * Any bound columns are editable inline.
	 * There is no action column.
	 */
	public abstract RenderedComponent dataRepeater(RenderedComponent component, String listVar, DataRepeater repeater);
	public abstract RenderedComponent addDataGridBoundColumn(RenderedComponent component,
																RenderedComponent current, 
																AbstractDataWidget widget,
																DataGridBoundColumn column,
																String listVar,
																String columnTitle,
																String columnBinding,
																StringBuilder gridColumnExpression);
	public abstract RenderedComponent addedDataGridBoundColumn(RenderedComponent component, RenderedComponent current);
	public abstract RenderedComponent addDataGridContainerColumn(RenderedComponent component,
																	RenderedComponent current,
																	AbstractDataWidget widget,
																	DataGridContainerColumn column);
	public abstract RenderedComponent addedDataGridContainerColumn(RenderedComponent component, RenderedComponent current);
	public abstract RenderedComponent addDataGridActionColumn(RenderedComponent component,
																RenderedComponent current, 
																DataGrid grid,
																String listVar,
																String gridColumnExpression,
																String singluarDocumentAlias,
																boolean inline);
	
	public abstract RenderedComponent listGrid(RenderedComponent component,
												String modelDocumentName,
												String modelName,
												ListModel<? extends Bean> model,
												ListGrid listGrid,
												boolean canCreateDocument);

	/*
	 * List Repeater is just like a list grid but...
	 * The grid column headers can be turned off.
	 * The grid (borders) can be turned off.
	 * It implements infinite scrolling instead of the page controls.
	 * Any bound columns are editable inline.
	 * There is no action column.
	 * No CRUD.
	 */
	public abstract RenderedComponent listRepeater(RenderedComponent component,
													String modelDocumentName,
													String modelName,
													ListModel<? extends Bean> model, 
													List<FilterParameter> filterParameters,
													String title,
													boolean showColumnHeaders,
													boolean showGrid);

	public abstract RenderedComponent listMembership(RenderedComponent component, ListMembership membership);
	
	public abstract RenderedComponent checkBox(RenderedComponent component, 
												String listVar, 
												CheckBox checkBox, 
												String title, 
												boolean required);

	public abstract RenderedComponent colourPicker(RenderedComponent component, 
													String listVar, 
													ColourPicker colour, 
													String title, 
													boolean required);
	
	public abstract RenderedComponent combo(RenderedComponent component, 
												String listVar, 
												Combo combo, 
												String title, 
												boolean required);

	public abstract RenderedComponent contentImage(RenderedComponent component, 
													String listVar, 
													ContentImage image, 
													String title, 
													boolean required);

	public abstract RenderedComponent contentLink(RenderedComponent component, 
													String listVar, 
													ContentLink link, 
													String title, 
													boolean required);
	
	public abstract RenderedComponent html(RenderedComponent component, 
											String listVar, 
											HTML html, 
											String title, 
											boolean required);

	public abstract RenderedComponent lookupDescription(RenderedComponent component, 
															String listVar,
															LookupDescription lookup,
															String title,
															boolean required,
															String displayBinding,
															QueryDefinition query);
	
	public abstract RenderedComponent password(RenderedComponent component, 
												String listVar,
												Password password,
												String title,
												boolean required);

	public abstract RenderedComponent radio(RenderedComponent component, 
												String listVar,
												Radio radio,
												String title,
												boolean required);
	
	public abstract RenderedComponent richText(RenderedComponent component, 
												String listVar,
												RichText text,
												String title,
												boolean required);
	
	public abstract RenderedComponent spinner(RenderedComponent component, 
												String listVar,
												Spinner spinner,
												String title,
												boolean required);
	
	public abstract RenderedComponent text(RenderedComponent component, 
											String listVar, 
											TextField text, 
											String title, 
											boolean required,
											Integer length,
											Converter<?> converter,
											Format<?> format);

	public abstract RenderedComponent textArea(RenderedComponent component, 
												String listVar,
												TextArea text,
												String title,
												boolean required,
												Integer length);
	
	public String outputLink(String listVar, 
										String value, 
										String href, 
										String invisible,
										ReferenceTarget target) {
/*
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
*/
		return "";
	}
/*		
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
*/

	public void addAjaxBehavior(RenderedComponent component, 
									String eventName,
									String collectionBinding,
									String listVar,
									String rerenderSource, 
									List<EventAction> actions) {
/*
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
*/
	}

	public abstract RenderedComponent actionLink(RenderedComponent component,
													String listBinding,
													String listVar,
													Link link,
													String actionName);

	public abstract RenderedComponent report(RenderedComponent component, Action action);
	
	public abstract RenderedComponent download(RenderedComponent component, 
												Action action,
												String moduleName,
												String documentName);
	
	public abstract RenderedComponent action(RenderedComponent component, 
												String listBinding, 
												String listVar,
												Action action, 
												ImplicitActionName name, 
												String title);
}
