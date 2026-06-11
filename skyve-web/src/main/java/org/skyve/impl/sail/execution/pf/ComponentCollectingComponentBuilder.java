package org.skyve.impl.sail.execution.pf;

import java.util.List;

import org.primefaces.component.button.Button;
import org.primefaces.component.commandbutton.CommandButton;
import org.skyve.domain.Bean;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.widget.bound.ZoomIn;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;
import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.Password;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.impl.metadata.view.widget.bound.input.Slider;
import org.skyve.impl.metadata.view.widget.bound.input.Spinner;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.web.faces.pipeline.component.NoOpComponentBuilder;
import org.skyve.metadata.MetaData;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.query.QueryDefinition;
import org.skyve.metadata.sail.language.Step;
import org.skyve.metadata.sail.language.step.context.PushEditContext;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.widget.bound.Bound;

import jakarta.annotation.Nullable;
import jakarta.faces.component.UIComponent;
import jakarta.faces.component.html.HtmlPanelGroup;

/**
 * Collects generated PrimeFaces components and indexes them in the automation context.
 */
@SuppressWarnings("java:S1192") // Repeated literals are deliberate component selector fragments.
class ComponentCollectingComponentBuilder extends NoOpComponentBuilder {
	private PrimeFacesAutomationContext context;
	private Step step;
	// component that was built in the component builder but added in the layout builder
	private MetaData addedViewComponent;
	private String addedViewComponentIdentifier;
	
	/**
	 * Creates a component builder that records generated PrimeFaces components for SAIL execution steps.
	 *
	 * @param context the automation context receiving identifier-to-component mappings
	 * @param step the SAIL step currently being rendered
	 */
	ComponentCollectingComponentBuilder(PrimeFacesAutomationContext context, Step step) {
		this.context = context;
		this.step = step;
	}
	
	/**
	 * Registers a rendered action component in the automation context.
	 *
	 * @param component the rendered component
	 * @param dataWidgetBinding optional data-widget binding
	 * @param dataWidgetVar optional data-widget variable
	 * @param label action label
	 * @param iconStyleClass icon style class
	 * @param toolTip tooltip text
	 * @param confirmationText optional confirmation text
	 * @param name implicit action name when present
	 * @param action action metadata definition
	 * @return the supplied component
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
		if (component != null) {
			if (name != null) {
				context.put(name.toString(), component, name);
			}
			else {
				context.put(action.getName(), component, action);
			}
		}
		return component;
	}
	
	/**
	 * Registers a rendered button-backed action component in the automation context.
	 *
	 * @param component the rendered component
	 * @param dataWidgetBinding optional data-widget binding
	 * @param dataWidgetVar optional data-widget variable
	 * @param label action label
	 * @param iconStyleClass icon style class
	 * @param toolTip tooltip text
	 * @param confirmationText optional confirmation text
	 * @param button button metadata definition
	 * @param formDisabledConditionName optional form disabled condition
	 * @param action action metadata definition
	 * @return the supplied component
	 */
	@Override
	public UIComponent actionButton(UIComponent component,
										String dataWidgetBinding,
										String dataWidgetVar,
										String label,
										String iconStyleClass,
										String toolTip,
										String confirmationText, 
										org.skyve.impl.metadata.view.widget.Button button,
										String formDisabledConditionName,
										Action action) {
		if (component != null) {
			context.put(button.getActionName(), component, button);
		}
		return component;
	}
	
	/**
	 * Registers a rendered download action component in the automation context.
	 *
	 * @param component the rendered component
	 * @param dataWidgetBinding optional data-widget binding
	 * @param dataWidgetVar optional data-widget variable
	 * @param label action label
	 * @param iconStyleClass icon style class
	 * @param toolTip tooltip text
	 * @param confirmationText optional confirmation text
	 * @param action action metadata definition
	 * @return the supplied component
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
		if (component != null) {
			context.put(action.getName(), component, action);
		}
		
		return component;
	}
	
	/**
	 * Registers a rendered button-backed download action in the automation context.
	 *
	 * @param component the rendered component
	 * @param dataWidgetBinding optional data-widget binding
	 * @param dataWidgetVar optional data-widget variable
	 * @param label action label
	 * @param iconStyleClass icon style class
	 * @param toolTip tooltip text
	 * @param confirmationText optional confirmation text
	 * @param button button metadata definition
	 * @param formDisabledConditionName optional form disabled condition
	 * @param action action metadata definition
	 * @return the supplied component
	 */
	@Override
	public UIComponent downloadButton(UIComponent component,
										String dataWidgetBinding,
										String dataWidgetVar,
										String label,
										String iconStyleClass,
										String toolTip,
										String confirmationText, 
										org.skyve.impl.metadata.view.widget.Button button,
										String formDisabledConditionName,
										Action action) {
		if (component != null) {
			context.put(action.getName(), component, action);
		}
		
		return component;
	}

	/**
	 * Registers a rendered report action component in the automation context.
	 *
	 * @param component the rendered component
	 * @param label action label
	 * @param iconStyleClass icon style class
	 * @param toolTip tooltip text
	 * @param confirmationText optional confirmation text
	 * @param action action metadata definition
	 * @return the supplied component
	 */
	@Override
	public UIComponent report(UIComponent component,
								String label,
								String iconStyleClass,
								String toolTip,
								String confirmationText, 
								Action action) {
		if (component != null) {
			context.put(action.getName(), component, action);
		}
		
		return component;
	}
	
	/**
	 * Registers a rendered button-backed report action in the automation context.
	 *
	 * @param component the rendered component
	 * @param label action label
	 * @param iconStyleClass icon style class
	 * @param toolTip tooltip text
	 * @param confirmationText optional confirmation text
	 * @param button button metadata definition
	 * @param formDisabledConditionName optional form disabled condition
	 * @param action action metadata definition
	 * @return the supplied component
	 */
	@Override
	public UIComponent reportButton(UIComponent component,
										String label,
										String iconStyleClass,
										String toolTip,
										String confirmationText, 
										org.skyve.impl.metadata.view.widget.Button button,
										String formDisabledConditionName,
										Action action) {
		if (component != null) {
			context.put(action.getName(), component, action);
		}
		
		return component;
	}
	
	/**
	 * Registers a rendered upload action component in the automation context.
	 *
	 * @param component the rendered component
	 * @param label action label
	 * @param iconStyleClass icon style class
	 * @param toolTip tooltip text
	 * @param confirmationText optional confirmation text
	 * @param action action metadata definition
	 * @return the supplied component
	 */
	@Override
	public UIComponent upload(UIComponent component,
								String label,
								String iconStyleClass,
								String toolTip,
								String confirmationText, 
								Action action) {
		if (component != null) {
			context.put(action.getName(), component, action);
		}
		
		return component;
	}
	
	/**
	 * Registers a rendered button-backed upload action in the automation context.
	 *
	 * @param component the rendered component
	 * @param label action label
	 * @param iconStyleClass icon style class
	 * @param toolTip tooltip text
	 * @param confirmationText optional confirmation text
	 * @param button button metadata definition
	 * @param formDisabledConditionName optional form disabled condition
	 * @param action action metadata definition
	 * @return the supplied component
	 */
	@Override
	public UIComponent uploadButton(UIComponent component,
										String label,
										String iconStyleClass,
										String toolTip,
										String confirmationText, 
										org.skyve.impl.metadata.view.widget.Button button,
										String formDisabledConditionName,
										Action action) {
		if (component != null) {
			context.put(action.getName(), component, action);
		}
		
		return component;
	}
	
	/**
	 * Tracks a list-grid component and stores its generated identifier for later container attachment.
	 *
	 * @param component the rendered list-grid component
	 * @param moduleName list model module name
	 * @param modelDocumentName list model document name
	 * @param modelName list model name
	 * @param uxui active UX/UI name
	 * @param model list model instance
	 * @param owningDocument owning document metadata
	 * @param listGrid list-grid metadata definition
	 * @param aggregateQuery whether aggregate-query mode is active
	 * @return the supplied component
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
		if (component != null) {
			addedViewComponent = listGrid;
			if (step instanceof PushEditContext pushEditContext) { // an edit view
				addedViewComponentIdentifier = String.format("%s.%s", pushEditContext.getModuleName(), modelName);
			}
			else {
				addedViewComponentIdentifier = step.getIdentifier(context);
				listGrid(component);
			}
		}
		
		return component;
	}
	
	/**
	 * Adds a deferred component into the active container flow when a view component was staged earlier.
	 *
	 * @param componentToAdd the component being attached to the rendered container
	 */
	void addToContainer(UIComponent componentToAdd) {
		if (addedViewComponent instanceof ListGrid) {
			listGrid(componentToAdd);
		}
	}

	/**
	 * Registers list-grid component handles and row-action controls in the automation context.
	 *
	 * @param listGridComponent the rendered list-grid component instance
	 */
	private void listGrid(UIComponent listGridComponent) {
		context.put(addedViewComponentIdentifier, listGridComponent, addedViewComponentIdentifier);
		int childCount = listGridComponent.getChildCount();
		if (childCount > 0) {
			UIComponent potentialActionColumn = listGridComponent.getChildren().get(childCount - 1);
			UIComponent header = potentialActionColumn.getFacet("header");
			if (header instanceof HtmlPanelGroup) { // flex grid inside column header
				if (header.getChildCount() >= 2) { // add button is 2nd in the list if it exists
					UIComponent addButton = header.getChildren().get(1);
					if ((addButton instanceof CommandButton) || (addButton instanceof Button)) { // there is an add button (might be no create privilege)
						context.put(addedViewComponentIdentifier + ".new", addButton, addedViewComponent);
					}
				}
			}
			else if (header instanceof CommandButton) { // command button straight in the header facet
				context.put(addedViewComponentIdentifier + ".new", header, addedViewComponent);
			}
			UIComponent zoomButton = potentialActionColumn.getChildren().get(0);
			if ((zoomButton instanceof CommandButton) || (zoomButton instanceof Button)) {
				context.put(addedViewComponentIdentifier + ".zoom", zoomButton, addedViewComponent);
			}
			context.put(addedViewComponentIdentifier + ".select", listGridComponent.getChildren().get(0), addedViewComponent);
		}
		
		addedViewComponent = null;
		addedViewComponentIdentifier = null;
	}
	
	/**
	 * Registers DataGrid action-column controls (new, zoom, remove, and select) for automation lookup.
	 *
	 * @param component the rendered data-grid component
	 * @param current current component in the chained builder
	 * @param grid data-grid metadata definition
	 * @param dataWidgetVar optional data-widget variable
	 * @param gridColumnExpression configured grid-column expression
	 * @param singluarDocumentAlias singular document alias for row context
	 * @param inline whether inline editing is enabled
	 * @param canCreate whether create actions are enabled
	 * @param canDelete whether delete actions are enabled
	 * @return the supplied component
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
		// component = current as it was set in the previous builder in the chain
		if (component != null) {
			String dataGridIdentifier = grid.getBinding();
			context.put(dataGridIdentifier, component, grid);
			UIComponent potentialActionColumn = component.getChildren().get(component.getChildCount() - 1);
			UIComponent header = potentialActionColumn.getFacet("header");
			if (header instanceof HtmlPanelGroup) { // flex grid inside column header
				if (header.getChildCount() > 0) { // add button is 1st in the list if it exists
					UIComponent addButton = header.getChildren().get(0);
					if (addButton instanceof CommandButton) { // there is an add button (might be no create privilege)
						context.put(dataGridIdentifier + ".new", addButton, grid);
					}
				}
			}
			else if (header instanceof CommandButton) { // command button straight in the header facet
				context.put(dataGridIdentifier + ".new", header, grid);
			}
			List<UIComponent> potentialActionColumnChildren = potentialActionColumn.getChildren();
			if (potentialActionColumnChildren.size() > 0) {
				UIComponent zoomButton = potentialActionColumnChildren.get(0);
				if (zoomButton instanceof CommandButton) {
					context.put(dataGridIdentifier + ".zoom", zoomButton, grid);
				}
			}
			// child 1 is a spacer.
			if (potentialActionColumnChildren.size() > 2) {
				UIComponent removeButton = potentialActionColumnChildren.get(2);
				if (removeButton instanceof CommandButton) {
					context.put(dataGridIdentifier + ".remove", removeButton, grid);
				}
			}
			context.put(dataGridIdentifier + ".select", component.getChildren().get(0), grid);
		}
		
		return component;
	}

	/**
	 * Registers a checkbox component against its binding for later automation lookup.
	 *
	 * @param component the rendered checkbox component
	 * @param dataWidgetVar optional data-widget variable
	 * @param checkBox checkbox widget metadata
	 * @param formDisabledConditionName form-level disabled condition
	 * @param title field title
	 * @param requiredMessage optional required-field message
	 * @return the supplied component
	 */
	@Override
	public EventSourceComponent checkBox(EventSourceComponent component,
											String dataWidgetVar,
											CheckBox checkBox,
											String formDisabledConditionName,
											String title,
											@Nullable String requiredMessage) {
		return putByBinding(checkBox, component);
	}
	
	/**
	 * Registers a colour-picker component against its binding for later automation lookup.
	 *
	 * @param component the rendered colour-picker component
	 * @param dataWidgetVar optional data-widget variable
	 * @param colour colour-picker metadata
	 * @param formDisabledConditionName form-level disabled condition
	 * @param title field title
	 * @param requiredMessage optional required-field message
	 * @param textAlignment text alignment metadata
	 * @return the supplied component
	 */
	@Override
	public EventSourceComponent colourPicker(EventSourceComponent component,
												String dataWidgetVar,
												ColourPicker colour,
												String formDisabledConditionName,
												String title,
												@Nullable String requiredMessage,
												HorizontalAlignment textAlignment) {
		return putByBinding(colour, component);
	}
	
	/**
	 * Registers a combo component against its binding for later automation lookup.
	 *
	 * @param component the rendered combo component
	 * @param dataWidgetVar optional data-widget variable
	 * @param combo combo metadata
	 * @param formDisabledConditionName form-level disabled condition
	 * @param title field title
	 * @param requiredMessage optional required-field message
	 * @return the supplied component
	 */
	@Override
	public EventSourceComponent combo(EventSourceComponent component,
										String dataWidgetVar,
										Combo combo,
										String formDisabledConditionName,
										String title,
										@Nullable String requiredMessage) {
		return putByBinding(combo, component);
	}
	
	/**
	 * Registers a content-image component against its binding for later automation lookup.
	 *
	 * @param component the rendered content-image component
	 * @param dataWidgetVar optional data-widget variable
	 * @param image content-image metadata
	 * @param formDisabledConditionName form-level disabled condition
	 * @param title field title
	 * @param requiredMessage optional required-field message
	 * @return the supplied component
	 */
	@Override
	public UIComponent contentImage(UIComponent component,
										String dataWidgetVar,
										ContentImage image,
										String formDisabledConditionName,
										String title,
										@Nullable String requiredMessage) {
		return putByBinding(image, component);
	}
	
	/**
	 * Registers a content-link component against its binding for later automation lookup.
	 *
	 * @param component the rendered content-link component
	 * @param dataWidgetVar optional data-widget variable
	 * @param link content-link metadata
	 * @param formDisabledConditionName form-level disabled condition
	 * @param title field title
	 * @param requiredMessage optional required-field message
	 * @param textAlignment text alignment metadata
	 * @return the supplied component
	 */
	@Override
	public UIComponent contentLink(UIComponent component,
									String dataWidgetVar,
									ContentLink link,
									String formDisabledConditionName,
									String title,
									@Nullable String requiredMessage,
									HorizontalAlignment textAlignment) {
		return putByBinding(link, component);
	}
	
	/**
	 * Registers an HTML component against its binding for later automation lookup.
	 *
	 * @param component the rendered HTML component
	 * @param dataWidgetVar optional data-widget variable
	 * @param html HTML widget metadata
	 * @param formDisabledConditionName form-level disabled condition
	 * @param title field title
	 * @param requiredMessage optional required-field message
	 * @return the supplied component
	 */
	@Override
	public UIComponent html(UIComponent component,
								String dataWidgetVar,
								HTML html,
								String formDisabledConditionName,
								String title,
								@Nullable String requiredMessage) {
		return putByBinding(html, component);
	}
	
	/**
	 * Registers a lookup-description component against its binding for later automation lookup.
	 *
	 * @param component the rendered lookup component
	 * @param dataWidgetVar optional data-widget variable
	 * @param lookup lookup widget metadata
	 * @param formDisabledConditionName form-level disabled condition
	 * @param title field title
	 * @param requiredMessage optional required-field message
	 * @param textAlignment text alignment metadata
	 * @param displayBinding display binding
	 * @param query lookup query definition
	 * @return the supplied component
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
		return putByBinding(lookup, component);
	}
	
	/**
	 * Registers a zoom-in component using a binding-specific zoom identifier in the automation context.
	 *
	 * @param component the rendered zoom component
	 * @param label action label
	 * @param iconStyleClass icon style class
	 * @param toolTip tooltip text
	 * @param zoomIn zoom metadata
	 * @param formDisabledConditionName form-level disabled condition
	 * @return the supplied component
	 */
	@Override
	public UIComponent zoomIn(UIComponent component,
								String label,
								String iconStyleClass,
								String toolTip,
								ZoomIn zoomIn,
								String formDisabledConditionName) {
		if (component != null) {
			context.put(zoomIn.getBinding() + ".zoomIn", component, zoomIn);
		}

		return component;
	}
	
	/**
	 * Registers a password component against its binding for later automation lookup.
	 *
	 * @param component the rendered password component
	 * @param dataWidgetVar optional data-widget variable
	 * @param password password metadata
	 * @param formDisabledConditionName form-level disabled condition
	 * @param title field title
	 * @param requiredMessage optional required-field message
	 * @param textAlignment text alignment metadata
	 * @return the supplied component
	 */
	@Override
	public EventSourceComponent password(EventSourceComponent component,
											String dataWidgetVar,
											Password password,
											String formDisabledConditionName,
											String title,
											@Nullable String requiredMessage,
											HorizontalAlignment textAlignment) {
		return putByBinding(password, component);
	}
	
	/**
	 * Registers a radio component against its binding for later automation lookup.
	 *
	 * @param component the rendered radio component
	 * @param dataWidgetVar optional data-widget variable
	 * @param radio radio metadata
	 * @param formDisabledConditionName form-level disabled condition
	 * @param title field title
	 * @param requiredMessage optional required-field message
	 * @return the supplied component
	 */
	@Override
	public EventSourceComponent radio(EventSourceComponent component,
										String dataWidgetVar,
										Radio radio,
										String formDisabledConditionName,
										String title,
										@Nullable String requiredMessage) {
		return putByBinding(radio, component);
	}
	
	/**
	 * Registers a rich-text component against its binding for later automation lookup.
	 *
	 * @param component the rendered rich-text component
	 * @param dataWidgetVar optional data-widget variable
	 * @param text rich-text metadata
	 * @param formDisabledConditionName form-level disabled condition
	 * @param title field title
	 * @param requiredMessage optional required-field message
	 * @return the supplied component
	 */
	@Override
	public EventSourceComponent richText(EventSourceComponent component,
											String dataWidgetVar,
											RichText text,
											String formDisabledConditionName,
											String title,
											@Nullable String requiredMessage) {
		return putByBinding(text, component);
	}
	
	/**
	 * Registers a spinner component against its binding for later automation lookup.
	 *
	 * @param component the rendered spinner component
	 * @param dataWidgetVar optional data-widget variable
	 * @param spinner spinner metadata
	 * @param formDisabledConditionName form-level disabled condition
	 * @param title field title
	 * @param requiredMessage optional required-field message
	 * @param textAlignment text alignment metadata
	 * @param facesConverter resolved Faces converter
	 * @return the supplied component
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
		return putByBinding(spinner, component);
	}
	
	/**
	 * Registers a slider component against its binding for later automation lookup.
	 *
	 * @param component the rendered slider component
	 * @param dataWidgetVar optional data-widget variable
	 * @param spinner slider metadata
	 * @param formDisabledConditionName form-level disabled condition
	 * @param title field title
	 * @param requiredMessage optional required-field message
	 * @param facesConverter resolved Faces converter
	 * @return the supplied component
	 */
	@Override
	public EventSourceComponent slider(EventSourceComponent component,
										String dataWidgetVar,
										Slider spinner,
										String formDisabledConditionName,
										String title,
										String requiredMessage,
										jakarta.faces.convert.Converter<?> facesConverter) {
		return putByBinding(spinner, component);
	}
	
	/**
	 * Registers a rendered tab component by localized tab title in the automation context.
	 *
	 * @param component the rendered tab component
	 * @param title tab title
	 * @param tab tab metadata
	 * @return the supplied component
	 */
	@Override
	public UIComponent tab(UIComponent component, String title, Tab tab) {
		if (component != null) {
			// TODO this needs to take into account that it could be nested in other tabs and have a name clash
			context.put(tab.getLocalisedTitle() + " Tab", component, tab);
		}
		return component;
	}
	
	/**
	 * Registers a text-field component against its binding for later automation lookup.
	 *
	 * @param component the rendered text component
	 * @param dataWidgetVar optional data-widget variable
	 * @param text text-field widget metadata
	 * @param formDisabledConditionName form-level disabled condition
	 * @param title field title
	 * @param requiredMessage optional required-field message
	 * @param textAlignment text alignment metadata
	 * @param length optional length constraint
	 * @param converter metadata converter
	 * @param format metadata format
	 * @param facesConverter resolved Faces converter
	 * @return the supplied component
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
		return putByBinding(text, component);
	}

	/**
	 * Registers a text-area component against its binding for later automation lookup.
	 *
	 * @param component the rendered text-area component
	 * @param dataWidgetVar optional data-widget variable
	 * @param text text-area metadata
	 * @param formDisabledConditionName form-level disabled condition
	 * @param title field title
	 * @param requiredMessage optional required-field message
	 * @param textAlignment text alignment metadata
	 * @param length optional length constraint
	 * @return the supplied component
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
		return putByBinding(text, component);
	}
	
	/**
	 * Registers a bound component in the automation context using its binding as the identifier.
	 *
	 * @param bound the bound metadata item contributing the identifier
	 * @param component the rendered component instance
	 * @return the supplied component
	 */
	private UIComponent putByBinding(Bound bound, UIComponent component) {
		if (component != null) {
			context.put(bound.getBinding(), component, bound);
		}

		return component;
	}

	/**
	 * Registers an event-source component in the automation context using its binding as the identifier.
	 *
	 * @param bound the bound metadata item contributing the identifier
	 * @param component the rendered event-source wrapper
	 * @return the supplied event-source wrapper
	 */
	private EventSourceComponent putByBinding(Bound bound, EventSourceComponent component) {
		if (component != null) {
			context.put(bound.getBinding(), component.getComponent(), bound);
		}

		return component;
	}
}
