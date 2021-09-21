package org.skyve.impl.sail.execution;

import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.component.html.HtmlPanelGroup;

import org.primefaces.component.button.Button;
import org.primefaces.component.commandbutton.CommandButton;
import org.skyve.domain.Bean;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
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

class ComponentCollectingComponentBuilder extends NoOpComponentBuilder {
	private PrimeFacesAutomationContext context;
	private Step step;
	// component that was built in the component builder but added in the layout builder
	private MetaData addedViewComponent;
	private String addedViewComponentIdentifier;
	
	ComponentCollectingComponentBuilder(PrimeFacesAutomationContext context, Step step) {
		this.context = context;
		this.step = step;
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
	
	@Override
	public UIComponent listGrid(UIComponent component,
									String moduleName,
									String modelDocumentName,
									String modelName,
									ListModel<? extends Bean> model,
									Document owningDocument,
									String title,
									ListGrid listGrid,
									boolean canCreateDocument,
									boolean aggregateQuery) {
		if (component != null) {
			addedViewComponent = listGrid;
			if (step instanceof PushEditContext) { // an edit view
				addedViewComponentIdentifier = String.format("%s.%s", ((PushEditContext) step).getModuleName(), modelName);
			}
			else {
				addedViewComponentIdentifier = step.getIdentifier(context);
				listGrid(component);
			}
		}
		
		return component;
	}
	
	void addToContainer(UIComponent componentToAdd) {
		if (addedViewComponent instanceof ListGrid) {
			listGrid(componentToAdd);
		}
	}

	private void listGrid(UIComponent listGridComponent) {
		context.put(addedViewComponentIdentifier, listGridComponent, addedViewComponentIdentifier);
		UIComponent potentialActionColumn = listGridComponent.getChildren().get(listGridComponent.getChildCount() - 1);
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

		addedViewComponent = null;
		addedViewComponentIdentifier = null;
	}
	
	@Override
	public UIComponent addDataGridActionColumn(UIComponent component,
												UIComponent current,
												DataGrid grid,
												String dataWidgetVar,
												String gridColumnExpression,
												String singluarDocumentAlias,
												boolean inline) {
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

	@Override
	public EventSourceComponent checkBox(EventSourceComponent component,
											String dataWidgetVar,
											CheckBox checkBox,
											String formDisabledConditionName,
											String title,
											boolean required) {
		return putByBinding(checkBox, component);
	}
	
	@Override
	public EventSourceComponent colourPicker(EventSourceComponent component,
												String dataWidgetVar,
												ColourPicker colour,
												String formDisabledConditionName,
												String title,
												boolean required) {
		return putByBinding(colour, component);
	}
	
	@Override
	public EventSourceComponent combo(EventSourceComponent component,
										String dataWidgetVar,
										Combo combo,
										String formDisabledConditionName,
										String title,
										boolean required) {
		return putByBinding(combo, component);
	}
	
	@Override
	public UIComponent contentImage(UIComponent component,
										String dataWidgetVar,
										ContentImage image,
										String formDisabledConditionName,
										String title,
										boolean required) {
		return putByBinding(image, component);
	}
	
	@Override
	public UIComponent contentLink(UIComponent component,
									String dataWidgetVar,
									ContentLink link,
									String formDisabledConditionName,
									String title,
									boolean required) {
		return putByBinding(link, component);
	}
	
	@Override
	public UIComponent html(UIComponent component,
								String dataWidgetVar,
								HTML html,
								String formDisabledConditionName,
								String title,
								boolean required) {
		return putByBinding(html, component);
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
		return putByBinding(lookup, component);
	}
	
	@Override
	public UIComponent zoomIn(UIComponent component,
								String label,
								String iconStyleClass,
								String toolTip,
								ZoomIn zoomIn,
								String formDisabledConditionName) {
		return putByBinding(zoomIn, component);
	}
	
	@Override
	public EventSourceComponent password(EventSourceComponent component,
											String dataWidgetVar,
											Password password,
											String formDisabledConditionName,
											String title,
											boolean required) {
		return putByBinding(password, component);
	}
	
	@Override
	public EventSourceComponent radio(EventSourceComponent component,
										String dataWidgetVar,
										Radio radio,
										String formDisabledConditionName,
										String title,
										boolean required) {
		return putByBinding(radio, component);
	}
	
	@Override
	public EventSourceComponent richText(EventSourceComponent component,
											String dataWidgetVar,
											RichText text,
											String formDisabledConditionName,
											String title,
											boolean required) {
		return putByBinding(text, component);
	}
	
	@Override
	public EventSourceComponent spinner(EventSourceComponent component,
											String dataWidgetVar,
											Spinner spinner,
											String formDisabledConditionName,
											String title,
											boolean required,
											javax.faces.convert.Converter facesConverter) {
		return putByBinding(spinner, component);
	}
	
	@Override
	public UIComponent tab(UIComponent component, String title, Tab tab) {
		if (component != null) {
			// TODO this needs to take into account that it could be nested in other tabs and have a name clash
			context.put(tab.getLocalisedTitle() + " Tab", component, tab);
		}
		return component;
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
		return putByBinding(text, component);
	}

	@Override
	public EventSourceComponent textArea(EventSourceComponent component,
											String dataWidgetVar,
											TextArea text,
											String formDisabledConditionName,
											String title,
											boolean required,
											Integer length) {
		return putByBinding(text, component);
	}
	
	private UIComponent putByBinding(Bound bound, UIComponent component) {
		if (component != null) {
			context.put(bound.getBinding(), component, bound);
		}

		return component;
	}

	private EventSourceComponent putByBinding(Bound bound, EventSourceComponent component) {
		if (component != null) {
			context.put(bound.getBinding(), component.getComponent(), bound);
		}

		return component;
	}
}
