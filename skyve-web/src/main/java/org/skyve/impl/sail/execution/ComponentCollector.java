package org.skyve.impl.sail.execution;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import javax.faces.component.NamingContainer;
import javax.faces.component.UIComponent;

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
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.module.query.QueryDefinition;
import org.skyve.metadata.sail.language.Step;
import org.skyve.metadata.sail.language.step.context.PushEditContext;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.widget.bound.Bound;

class ComponentCollector extends NoOpComponentBuilder {
	private AutomationContext context;
	private Step step;
	private Map<String, List<UIComponent>> components = new TreeMap<>();
	private Map<String, List<Object>> widgets = new TreeMap<>();
	
	ComponentCollector(AutomationContext context, Step step) {
		this.context = context;
		this.step = step;
	}
	
	private void put(String identifier, UIComponent component, Object widget) {
//System.out.println(identifier + " -> " + clientId(component) + " & " + widget);
		List<UIComponent> componentList = components.get(identifier);
		if (componentList == null) {
			componentList = new ArrayList<>();
			components.put(identifier, componentList);
		}
		componentList.add(component);

		List<Object> widgetList = widgets.get(identifier);
		if (widgetList == null) {
			widgetList = new ArrayList<>();
			widgets.put(identifier, widgetList);
		}
		widgetList.add(widget);
	}
	
	List<UIComponent> getFacesComponents(String identifier) {
		return components.get(identifier);
	}

	List<Object> getSkyveWidgets(String identifier) {
		return widgets.get(identifier);
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
				put(name.toString(), component, name);
			}
			else {
				put(action.getName(), component, action);
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
			put(button.getActionName(), component, button);
		}
		return component;
	}
	
	@Override
	public UIComponent download(UIComponent component, Action action, String moduleName, String documentName) {
		if (component != null) {
			put(action.getName(), component, action);
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
			put(action.getName(), component, action);
		}
		
		return component;
	}

	@Override
	public UIComponent report(UIComponent component, Action action) {
		if (component != null) {
			put(action.getName(), component, action);
		}
		
		return component;
	}
	
	@Override
	public UIComponent reportButton(UIComponent component,
										org.skyve.impl.metadata.view.widget.Button button,
										Action action) {
		if (component != null) {
			put(action.getName(), component, action);
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
			String listGridIdentifier = (step instanceof PushEditContext) ? modelName : step.getIdentifier(context);

			put(listGridIdentifier, component, listGrid);
			UIComponent potentialActionColumn = component.getChildren().get(component.getChildCount() - 1);
			UIComponent addButton = potentialActionColumn.getFacet("header");
			if (addButton instanceof Button) {
				put(listGridIdentifier + ".new", addButton, listGrid);
			}
			UIComponent zoomButton = potentialActionColumn.getChildren().get(0);
			if (zoomButton instanceof Button) {
				put(listGridIdentifier + ".zoom", zoomButton, listGrid);
			}
			put(listGridIdentifier + ".select", component.getChildren().get(0), listGrid);
		}
		
		return component;
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
			put(dataGridIdentifier, component, grid);
			UIComponent potentialActionColumn = component.getChildren().get(component.getChildCount() - 1);
			UIComponent addButton = potentialActionColumn.getFacet("header");
			if (addButton instanceof CommandButton) {
				put(dataGridIdentifier + ".new", addButton, grid);
			}
			List<UIComponent> potentialActionColumnChildren = potentialActionColumn.getChildren();
			if (potentialActionColumnChildren.size() > 0) {
				UIComponent zoomButton = potentialActionColumnChildren.get(0);
				if (zoomButton instanceof CommandButton) {
					put(dataGridIdentifier + ".zoom", zoomButton, grid);
				}
			}
			// child 1 is a spacer.
			if (potentialActionColumnChildren.size() > 2) {
				UIComponent removeButton = potentialActionColumnChildren.get(2);
				if (removeButton instanceof CommandButton) {
					put(dataGridIdentifier + ".remove", removeButton, grid);
				}
			}
			put(dataGridIdentifier + ".select", component.getChildren().get(0), grid);
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
			put(tab.getTitle() + " Tab", component, tab);
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
			put(bound.getBinding(), component, bound);
		}

		return component;
	}
	
	public static String clientId(UIComponent component, Integer row) {
		String id = clientId(component);
		int lastColonIndex = id.lastIndexOf(':');
		if (lastColonIndex > -1) {
			id = String.format("%s:%d%s", id.substring(0, lastColonIndex), row, id.substring(lastColonIndex));
		}
		else {
			id = String.format("%s:%d", id, row);
		}
		
		return id;
	}
	
	public static String clientId(UIComponent component) {
		StringBuilder result = new StringBuilder(32);
		clientId(component, result);

		UIComponent parent = component.getParent();
		while (parent != null) {
			if (parent instanceof NamingContainer) {
				clientId(parent, result);
			}
			parent = parent.getParent();
		}

		return result.toString();
	}
	
	private static void clientId(UIComponent component, StringBuilder clientId) {
		if (clientId.length() == 0) {
			clientId.append(component.getId());
		}
		else {
			clientId.insert(0, ':').insert(0, component.getId());
		}
	}
}
