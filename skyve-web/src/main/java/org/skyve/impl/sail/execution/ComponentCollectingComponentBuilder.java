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
								String listBinding,
								String listVar,
								Action action,
								ImplicitActionName name,
								String title) {
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
										String listBinding,
										String listVar,
										org.skyve.impl.metadata.view.widget.Button button,
										Action action) {
		if (component != null) {
			context.put(button.getActionName(), component, button);
		}
		return component;
	}
	
	@Override
	public UIComponent download(UIComponent component, Action action, String moduleName, String documentName) {
		if (component != null) {
			context.put(action.getName(), component, action);
		}
		
		return component;
	}
	
	@Override
	public UIComponent downloadButton(UIComponent component,
										org.skyve.impl.metadata.view.widget.Button button,
										Action action,
										String moduleName,
										String documentName) {
		if (component != null) {
			context.put(action.getName(), component, action);
		}
		
		return component;
	}

	@Override
	public UIComponent report(UIComponent component, Action action) {
		if (component != null) {
			context.put(action.getName(), component, action);
		}
		
		return component;
	}
	
	@Override
	public UIComponent reportButton(UIComponent component,
										org.skyve.impl.metadata.view.widget.Button button,
										Action action) {
		if (component != null) {
			context.put(action.getName(), component, action);
		}
		
		return component;
	}
	
	@Override
	public UIComponent upload(UIComponent component, Action action) {
		if (component != null) {
			context.put(action.getName(), component, action);
		}
		
		return component;
	}
	
	@Override
	public UIComponent uploadButton(UIComponent component,
										org.skyve.impl.metadata.view.widget.Button button,
										Action action) {
		if (component != null) {
			context.put(action.getName(), component, action);
		}
		
		return component;
	}
	
	@Override
	public UIComponent listGrid(UIComponent component,
									String modelDocumentName,
									String modelName,
									ListModel<? extends Bean> model,
									ListGrid listGrid,
									boolean canCreateDocument) {
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
				if (addButton instanceof Button) { // there is an add button (might be no create privilege)
					context.put(addedViewComponentIdentifier + ".new", addButton, addedViewComponent);
				}
			}
		}
		else if (header instanceof CommandButton) { // command button straight in the header facet
			context.put(addedViewComponentIdentifier + ".new", header, addedViewComponent);
		}
		UIComponent zoomButton = potentialActionColumn.getChildren().get(0);
		if (zoomButton instanceof Button) {
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
												String listVar,
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
	public UIComponent checkBox(UIComponent component,
									String listVar,
									CheckBox checkBox,
									String title,
									boolean required) {
		return putByBinding(checkBox, component);
	}
	
	@Override
	public UIComponent colourPicker(UIComponent component,
										String listVar,
										ColourPicker colour,
										String title,
										boolean required) {
		return putByBinding(colour, component);
	}
	
	@Override
	public UIComponent combo(UIComponent component, String listVar, Combo combo, String title, boolean required) {
		return putByBinding(combo, component);
	}
	
	@Override
	public UIComponent contentImage(UIComponent component,
										String listVar,
										ContentImage image,
										String title,
										boolean required) {
		return putByBinding(image, component);
	}
	
	@Override
	public UIComponent contentLink(UIComponent component,
									String listVar,
									ContentLink link,
									String title,
									boolean required) {
		return putByBinding(link, component);
	}
	
	@Override
	public UIComponent html(UIComponent component, String listVar, HTML html, String title, boolean required) {
		return putByBinding(html, component);
	}
	
	@Override
	public UIComponent lookupDescription(UIComponent component,
											String listVar,
											LookupDescription lookup,
											String title,
											boolean required,
											String displayBinding,
											QueryDefinition query) {
		return putByBinding(lookup, component);
	}
	
	@Override
	public UIComponent password(UIComponent component,
									String listVar,
									Password password,
									String title,
									boolean required) {
		return putByBinding(password, component);
	}
	
	@Override
	public UIComponent radio(UIComponent component,
								String listVar,
								Radio radio,
								String title,
								boolean required) {
		return putByBinding(radio, component);
	}
	
	@Override
	public UIComponent richText(UIComponent component, String listVar, RichText text, String title, boolean required) {
		return putByBinding(text, component);
	}
	
	@Override
	public UIComponent spinner(UIComponent component, String listVar, Spinner spinner, String title, boolean required) {
		return putByBinding(spinner, component);
	}
	
	@Override
	public UIComponent tab(UIComponent component, Tab tab) {
		if (component != null) {
			// TODO this needs to take into account that it could be nested in other tabs and have a name clash
			context.put(tab.getTitle() + " Tab", component, tab);
		}
		return component;
	}
	
	@Override
	public UIComponent text(UIComponent component,
								String listVar,
								TextField text,
								String title,
								boolean required,
								Integer length,
								Converter<?> converter,
								Format<?> format,
								javax.faces.convert.Converter facesConverter) {
		return putByBinding(text, component);
	}

	@Override
	public UIComponent textArea(UIComponent component,
									String listVar,
									TextArea text,
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
}
