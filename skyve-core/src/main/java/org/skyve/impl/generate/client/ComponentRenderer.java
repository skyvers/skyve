package org.skyve.impl.generate.client;

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
import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;
import org.skyve.impl.metadata.view.widget.bound.input.ContentUpload;
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
import org.skyve.metadata.view.widget.FilterParameter;
import org.skyve.metadata.view.widget.bound.Parameter;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Abstract renderer for individual view widgets/components.
 *
 * <p>Subclasses emit target-client code for each widget type.
 */
public abstract class ComponentRenderer extends AbstractRenderer {
	/**
	 * Creates the root view component used to switch create and edit visibility states.
	 *
	 * @param component the parent render node
	 * @param invisibleConditionName the visibility expression for the generated wrapper
	 * @return the generated root view component
	 */
	public abstract RenderedComponent view(RenderedComponent component, String invisibleConditionName);

	/**
	 * Creates toolbar components for the current rendered view.
	 *
	 * @param components the current render context nodes
	 * @param widgetId the source actions widget identifier
	 * @return toolbar components, or {@code null} when no toolbar should be emitted
	 */
	public abstract List<RenderedComponent> toolbars(List<RenderedComponent> components, String widgetId);

	/**
	 * Creates the component used to render a tab-pane container.
	 *
	 * @param component the parent render node
	 * @param tabPane the source Skyve tab-pane metadata
	 * @return the generated tab-pane component
	 */
	public abstract RenderedComponent tabPane(RenderedComponent component, TabPane tabPane);
	
	/**
	 * Creates a rendered tab component for the supplied tab metadata.
	 *
	 * @param component the parent render node
	 * @param title resolved tab title
	 * @param tab the source tab metadata
	 * @return the generated tab component
	 */
	public abstract RenderedComponent tab(RenderedComponent component, String title, Tab tab);
	
	/**
	 * Creates a border wrapper component used by bordered containers.
	 *
	 * @param component the parent render node
	 * @param title border caption
	 * @param invisibileConditionName expression controlling wrapper visibility
	 * @param pixelWidth explicit width for fixed-width borders, if any
	 * @return the generated border wrapper component
	 */
	public abstract RenderedComponent border(RenderedComponent component,
												String title,
												String invisibileConditionName,
												Integer pixelWidth);

	/**
	 * Creates a static text label component.
	 *
	 * @param component the parent render node
	 * @param value label text value
	 * @return the generated label component
	 */
	public abstract RenderedComponent label(RenderedComponent component, String value);
	
	/**
	 * Creates a spacer component.
	 *
	 * @param component the parent render node
	 * @param spacer source spacer metadata
	 * @return the generated spacer component
	 */
	public abstract RenderedComponent spacer(RenderedComponent component, Spacer spacer);
	
	/**
	 * Creates an action button component bound to a document or row context.
	 *
	 * @param component the parent render node
	 * @param dataWidgetBinding binding for the owning data-widget context
	 * @param dataWidgetVar row variable for repeating contexts
	 * @param button source button metadata
	 * @param action resolved action metadata
	 * @return the generated action button component
	 */
	public abstract RenderedComponent actionButton(RenderedComponent component,
													String dataWidgetBinding, 
													String dataWidgetVar, 
													Button button, 
													Action action);

	/**
	 * Creates a report button component.
	 *
	 * @param component the parent render node
	 * @param button source button metadata
	 * @param action resolved report action
	 * @return the generated report button component
	 */
	public abstract RenderedComponent reportButton(RenderedComponent component, 
												Button button, 
												Action action);

	/**
	 * Creates a download button component.
	 *
	 * @param component the parent render node
	 * @param button source button metadata
	 * @param action resolved download action
	 * @param moduleName module owning the download target document
	 * @param documentName target document name
	 * @return the generated download button component
	 */
	public abstract RenderedComponent downloadButton(RenderedComponent component,
														Button button, 
														Action action, 
														String moduleName, 
														String documentName);
	

	/**
	 * Creates a static image component from a resolved file URL.
	 *
	 * @param component the parent render node
	 * @param fileUrl resolved static image URL
	 * @param image source static image metadata
	 * @return the generated static image component
	 */
	public abstract RenderedComponent staticImage(RenderedComponent component, String fileUrl, StaticImage image);

	/**
	 * Creates a dynamic image component that resolves content from the target document.
	 *
	 * @param component the parent render node
	 * @param image source dynamic image metadata
	 * @param moduleName module owning the target document
	 * @param documentName target document name
	 * @return the generated dynamic image component
	 */
	public abstract RenderedComponent dynamicImage(RenderedComponent component, 
													DynamicImage image, 
													String moduleName, 
													String documentName);
	
	/**
	 * Creates a blurb component from literal markup or a binding expression.
	 *
	 * @param component the parent render node
	 * @param dataWidgetVar row variable for repeating contexts
	 * @param value resolved literal value
	 * @param binding optional binding expression for deferred resolution
	 * @param blurb source blurb metadata
	 * @return the generated blurb component
	 */
	public abstract RenderedComponent blurb(RenderedComponent component, 
												String dataWidgetVar, 
												String value, 
												String binding, 
												Blurb blurb);

	/**
	 * Creates a label component that may resolve its value from the bound row context.
	 *
	 * @param component the parent render node
	 * @param dataWidgetVar row variable for repeating contexts
	 * @param value resolved literal value
	 * @param binding optional binding expression for deferred resolution
	 * @param label source label metadata
	 * @return the generated bound-label component
	 */
	public abstract RenderedComponent label(RenderedComponent component,
												String dataWidgetVar,
												String value,
												String binding,
												Label label);

	/**
	 * Creates a data-grid component.
	 *
	 * @param component the parent render node
	 * @param dataWidgetVar row variable for repeating contexts
	 * @param ordered whether drag/drop ordering is enabled
	 * @param title resolved component title
	 * @param grid source data-grid metadata
	 * @return the generated data-grid component
	 */

	public abstract RenderedComponent dataGrid(RenderedComponent component,
												String dataWidgetVar,
												boolean ordered,
												String title,
												DataGrid grid);

	/**
	 * Creates a data repeater component.
	 * 
	 * Data Repeater is just like a data grid but...
	 * The grid column headers can be turned off.
	 * The grid (borders) can be turned off.
	 * Any bound columns are editable inline.
	 * There is no action column.
	 *
	 * @param component the parent render node
	 * @param dataWidgetVar row variable for repeating contexts
	 * @param title resolved component title
	 * @param repeater source repeater metadata
	 * @return the generated data-repeater component
	 */
	public abstract RenderedComponent dataRepeater(RenderedComponent component,
													String dataWidgetVar,
													String title,
													DataRepeater repeater);

	/**
	 * Adds a bound column component to a data-grid style widget.
	 *
	 * @param component root component for this grid render operation
	 * @param current current component insertion point
	 * @param widget owning tabular widget metadata
	 * @param column source bound-column metadata
	 * @param dataWidgetVar row variable for repeating contexts
	 * @param columnTitle resolved column title
	 * @param columnBinding resolved column binding
	 * @param gridColumnExpression mutable aggregate expression builder for columns
	 * @return the updated insertion point component
	 */
	@SuppressWarnings("java:S107") // Long parameter list preserves the existing framework/API contract.
	public abstract RenderedComponent addDataGridBoundColumn(RenderedComponent component,
																RenderedComponent current, 
																AbstractDataWidget widget,
																DataGridBoundColumn column,
																String dataWidgetVar,
																String columnTitle,
																String columnBinding,
																StringBuilder gridColumnExpression);

	/**
	 * Finalises insertion of a bound column and returns the active render node.
	 *
	 * @param component root component for this grid render operation
	 * @param current current component insertion point
	 * @return the updated insertion point component
	 */
	public abstract RenderedComponent addedDataGridBoundColumn(RenderedComponent component, RenderedComponent current);

	/**
	 * Adds a container column component to a data-grid style widget.
	 *
	 * @param component root component for this grid render operation
	 * @param current current component insertion point
	 * @param widget owning tabular widget metadata
	 * @param columnTitle resolved column title
	 * @param column source container-column metadata
	 * @return the updated insertion point component
	 */
	public abstract RenderedComponent addDataGridContainerColumn(RenderedComponent component,
																	RenderedComponent current,
																	AbstractDataWidget widget,
																	String columnTitle,
																	DataGridContainerColumn column);

	/**
	 * Finalises insertion of a container column and returns the active render node.
	 *
	 * @param component root component for this grid render operation
	 * @param current current component insertion point
	 * @return the updated insertion point component
	 */
	public abstract RenderedComponent addedDataGridContainerColumn(RenderedComponent component, RenderedComponent current);

	/**
	 * Adds the synthetic action column for a data grid.
	 *
	 * @param component root component for this grid render operation
	 * @param current current component insertion point
	 * @param grid source data-grid metadata
	 * @param dataWidgetVar row variable for repeating contexts
	 * @param gridColumnExpression resolved expression for emitted grid columns
	 * @param singluarDocumentAlias singular alias for the target document
	 * @param inline whether actions render inline
	 * @return the updated insertion point component
	 */
	public abstract RenderedComponent addDataGridActionColumn(RenderedComponent component,
																RenderedComponent current, 
																DataGrid grid,
																String dataWidgetVar,
																String gridColumnExpression,
																String singluarDocumentAlias,
																boolean inline);
	
	/**
	 * Creates a list-grid component.
	 *
	 * @param component the parent render node
	 * @param modelDocumentName document name bound to the list model
	 * @param modelName list-model name
	 * @param model resolved list model metadata
	 * @param title resolved component title
	 * @param listGrid source list-grid metadata
	 * @param aggregateQuery whether the backing query is aggregate
	 * @return the generated list-grid component
	 */
	public abstract RenderedComponent listGrid(RenderedComponent component,
												String modelDocumentName,
												String modelName,
												ListModel<Bean> model,
												String title,
												ListGrid listGrid,
												boolean aggregateQuery);

	/**
	 * Creates a list-repeater component from list-model metadata.
	 * 
	 * List Repeater is just like a list grid but...
	 * The grid column headers can be turned off.
	 * The grid (borders) can be turned off.
	 * It implements infinite scrolling instead of the page controls.
	 * Any bound columns are editable inline.
	 * There is no action column.
	 * No CRUD.
	 *
	 * @param component the parent render node
	 * @param modelDocumentName document name bound to the list model
	 * @param modelName list-model name
	 * @param model resolved list model metadata
	 * @param filterParameters query filter parameters
	 * @param parameters widget parameters
	 * @param title resolved component title
	 * @param showColumnHeaders whether headers should be rendered
	 * @param showGrid whether the grid shell should be rendered
	 * @return the generated list-repeater component
	 */
	@SuppressWarnings("java:S107") // Long parameter list preserves the existing framework/API contract.
	public abstract RenderedComponent listRepeater(RenderedComponent component,
													String modelDocumentName,
													String modelName,
													ListModel<Bean> model, 
													List<FilterParameter> filterParameters,
													List<Parameter> parameters,
													String title,
													boolean showColumnHeaders,
													boolean showGrid);

	/**
	 * Creates a list-membership component.
	 *
	 * @param component the parent render node
	 * @param membership source list-membership metadata
	 * @return the generated list-membership component
	 */
	public abstract RenderedComponent listMembership(RenderedComponent component, ListMembership membership);
	
	/**
	 * Creates a checkbox input component.
	 *
	 * @param component the parent render node
	 * @param dataWidgetVar row variable for repeating contexts
	 * @param checkBox source checkbox metadata
	 * @param title resolved input title
	 * @param requiredMessage optional required-message expression
	 * @return the generated checkbox component
	 */
	public abstract RenderedComponent checkBox(RenderedComponent component, 
												String dataWidgetVar, 
												CheckBox checkBox, 
												String title, 
												@Nullable String requiredMessage);

	/**
	 * Creates a colour-picker input component.
	 *
	 * @param component the parent render node
	 * @param dataWidgetVar row variable for repeating contexts
	 * @param colour source colour-picker metadata
	 * @param title resolved input title
	 * @param requiredMessage optional required-message expression
	 * @return the generated colour-picker component
	 */
	public abstract RenderedComponent colourPicker(RenderedComponent component, 
													String dataWidgetVar, 
													ColourPicker colour, 
													String title, 
													@Nullable String requiredMessage);
	
	/**
	 * Creates a combo/select input component.
	 *
	 * @param component the parent render node
	 * @param dataWidgetVar row variable for repeating contexts
	 * @param combo source combo metadata
	 * @param title resolved input title
	 * @param requiredMessage optional required-message expression
	 * @return the generated combo component
	 */
	public abstract RenderedComponent combo(RenderedComponent component, 
												String dataWidgetVar, 
												Combo combo, 
												String title, 
												@Nullable String requiredMessage);

	/**
	 * Creates a managed-content input component.
	 *
	 * @param component the parent render node
	 * @param dataWidgetVar row variable for repeating contexts
	 * @param content source managed-content upload metadata
	 * @param title resolved input title
	 * @param requiredMessage optional required-message expression
	 * @return the generated managed-content component
	 */
	public abstract @Nonnull RenderedComponent content(@Nullable RenderedComponent component,
														@Nullable String dataWidgetVar,
														@Nonnull ContentUpload content,
														@Nullable String title,
														@Nullable String requiredMessage);

	/**
	 * Creates a content-signature input component.
	 *
	 * @param component the parent render node
	 * @param dataWidgetVar row variable for repeating contexts
	 * @param signature source signature metadata
	 * @param title resolved input title
	 * @param requiredMessage optional required-message expression
	 * @return the generated content-signature component
	 */
	public abstract RenderedComponent contentSignature(RenderedComponent component, 
														String dataWidgetVar, 
														ContentSignature signature, 
														String title, 
														@Nullable String requiredMessage);

	/**
	 * Creates an HTML input component.
	 *
	 * @param component the parent render node
	 * @param dataWidgetVar row variable for repeating contexts
	 * @param html source HTML metadata
	 * @param title resolved input title
	 * @param requiredMessage optional required-message expression
	 * @return the generated HTML input component
	 */
	public abstract RenderedComponent html(RenderedComponent component, 
											String dataWidgetVar, 
											HTML html, 
											String title, 
											@Nullable String requiredMessage);

	/**
	 * Creates a lookup-description input component.
	 *
	 * @param component the parent render node
	 * @param dataWidgetVar row variable for repeating contexts
	 * @param lookup source lookup metadata
	 * @param title resolved input title
	 * @param requiredMessage optional required-message expression
	 * @param displayBinding optional binding used for display formatting
	 * @param query resolved lookup query definition
	 * @return the generated lookup-description component
	 */
	public abstract RenderedComponent lookupDescription(RenderedComponent component, 
															String dataWidgetVar,
															LookupDescription lookup,
															String title,
															@Nullable String requiredMessage,
															String displayBinding,
															QueryDefinition query);
	
	/**
	 * Creates a password input component.
	 *
	 * @param component the parent render node
	 * @param dataWidgetVar row variable for repeating contexts
	 * @param password source password metadata
	 * @param title resolved input title
	 * @param requiredMessage optional required-message expression
	 * @return the generated password component
	 */
	public abstract RenderedComponent password(RenderedComponent component, 
												String dataWidgetVar,
												Password password,
												String title,
												@Nullable String requiredMessage);

	/**
	 * Creates a radio input component.
	 *
	 * @param component the parent render node
	 * @param dataWidgetVar row variable for repeating contexts
	 * @param radio source radio metadata
	 * @param title resolved input title
	 * @param requiredMessage optional required-message expression
	 * @return the generated radio component
	 */
	public abstract RenderedComponent radio(RenderedComponent component, 
												String dataWidgetVar,
												Radio radio,
												String title,
												@Nullable String requiredMessage);
	
	/**
	 * Creates a rich-text input component.
	 *
	 * @param component the parent render node
	 * @param dataWidgetVar row variable for repeating contexts
	 * @param text source rich-text metadata
	 * @param title resolved input title
	 * @param requiredMessage optional required-message expression
	 * @return the generated rich-text component
	 */
	public abstract RenderedComponent richText(RenderedComponent component, 
												String dataWidgetVar,
												RichText text,
												String title,
												@Nullable String requiredMessage);
	
	/**
	 * Creates a spinner input component.
	 *
	 * @param component the parent render node
	 * @param dataWidgetVar row variable for repeating contexts
	 * @param spinner source spinner metadata
	 * @param title resolved input title
	 * @param requiredMessage optional required-message expression
	 * @return the generated spinner component
	 */
	public abstract RenderedComponent spinner(RenderedComponent component, 
												String dataWidgetVar,
												Spinner spinner,
												String title,
												@Nullable String requiredMessage);
	
	/**
	 * Creates a text-field input component.
	 *
	 * @param component the parent render node
	 * @param dataWidgetVar row variable for repeating contexts
	 * @param text source text-field metadata
	 * @param title resolved input title
	 * @param requiredMessage optional required-message expression
	 * @param length maximum rendered/editable length
	 * @param converter converter to apply when present
	 * @param format formatter to apply when present
	 * @return the generated text-field component
	 */
	@SuppressWarnings("java:S107") // Long parameter list preserves the existing framework/API contract.
	public abstract RenderedComponent text(RenderedComponent component, 
											String dataWidgetVar, 
											TextField text, 
											String title, 
											@Nullable String requiredMessage,
											Integer length,
											Converter<?> converter,
											Format<?> format);

	/**
	 * Creates a text-area input component.
	 *
	 * @param component the parent render node
	 * @param dataWidgetVar row variable for repeating contexts
	 * @param text source text-area metadata
	 * @param title resolved input title
	 * @param requiredMessage optional required-message expression
	 * @param length maximum rendered/editable length
	 * @return the generated text-area component
	 */
	public abstract RenderedComponent textArea(RenderedComponent component, 
												String dataWidgetVar,
												TextArea text,
												String title,
												@Nullable String requiredMessage,
												Integer length);
	
	/**
	 * Returns an output-link fragment for renderers that support direct hyperlink generation.
	 *
	 * <p>The default implementation returns an empty fragment because this capability is optional.
	 *
	 * @param dataWidgetVar the row variable for data-widget contexts
	 * @param value the display value when not resolved from {@code href}
	 * @param href the target URL expression
	 * @param invisible the invisible-condition expression controlling link visibility
	 * @param target target-frame metadata for the generated link
	 * @return renderer-specific link output, or an empty string when unsupported
	 */
	@SuppressWarnings("static-method")
	public String outputLink(String dataWidgetVar, 
										String value, 
										String href, 
										String invisible,
										ReferenceTarget target) {
/*
		HtmlOutputLink result = (HtmlOutputLink) a.createComponent(HtmlOutputLink.COMPONENT_TYPE);
		if (dataWidgetVar != null) {
			result.setValueExpression("value", createValueExpressionFromFragment(dataWidgetVar, true, href, true, null, String.class));
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
*/

	@SuppressWarnings("unused")
	public void addAjaxBehavior(RenderedComponent component, 
									String eventName,
									String collectionBinding,
									String dataWidgetVar,
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
																dataWidgetVar, 
																false, 
																null);
			ajax.addAjaxBehaviorListener(new AjaxBehaviorListenerImpl(me, me));
		}

		component.addClientBehavior(eventName, ajax);
*/
	}

	/**
	 * Creates a link component that invokes an action.
	 *
	 * @param component the parent render node
	 * @param dataWidgetBinding binding for the owning data-widget context
	 * @param dataWidgetVar row variable for repeating contexts
	 * @param link source link metadata
	 * @param action resolved action metadata
	 * @return the generated action link component
	 */
	public abstract RenderedComponent actionLink(RenderedComponent component,
													String dataWidgetBinding,
													String dataWidgetVar,
													Link link,
													Action action);

	/**
	 * Creates a report action component.
	 *
	 * @param component the parent render node
	 * @param action resolved report action metadata
	 * @return the generated report action component
	 */
	public abstract RenderedComponent report(RenderedComponent component, Action action);
	
	/**
	 * Creates a download action component.
	 *
	 * @param component the parent render node
	 * @param action resolved download action metadata
	 * @param moduleName module owning the download target document
	 * @param documentName target document name
	 * @return the generated download action component
	 */
	public abstract RenderedComponent download(RenderedComponent component, 
												Action action,
												String moduleName,
												String documentName);

	/**
	 * Creates an upload action component.
	 *
	 * @param component the parent render node
	 * @param action resolved upload action metadata
	 * @return the generated upload action component
	 */
	public abstract RenderedComponent upload(RenderedComponent component, Action action);

	/**
	 * Creates a generic action component for implicit or explicit action metadata.
	 *
	 * @param component the parent render node
	 * @param dataWidgetBinding binding for the owning data-widget context
	 * @param dataWidgetVar row variable for repeating contexts
	 * @param action resolved action metadata
	 * @param name implicit action name when available
	 * @param title resolved action title
	 * @return the generated action component
	 */
	public abstract RenderedComponent action(RenderedComponent component, 
												String dataWidgetBinding, 
												String dataWidgetVar,
												Action action, 
												ImplicitActionName name, 
												String title);
}
