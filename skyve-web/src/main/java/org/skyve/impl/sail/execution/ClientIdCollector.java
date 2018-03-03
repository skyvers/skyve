package org.skyve.impl.sail.execution;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import javax.faces.component.NamingContainer;
import javax.faces.component.UIComponent;

import org.primefaces.component.button.Button;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.web.faces.pipeline.component.NoOpComponentBuilder;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.sail.language.Step;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.model.list.ListModel;

public class ClientIdCollector extends NoOpComponentBuilder {
	private Step context;
	private Map<String, List<UIComponent>> components = new TreeMap<>();
	private Map<String, List<Object>> widgets = new TreeMap<>();
	
	public ClientIdCollector(Step context) {
		this.context = context;
	}
	
	private void put(String identifier, UIComponent component, Object widget) {
System.out.println(identifier + " -> " + clientId(component) + " & " + widget);
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
	
	public List<UIComponent> getFacesComponents(String identifier) {
		return components.get(identifier);
	}

	public List<Object> getSkyveWidgets(String identifier) {
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
			String listGridIdentifier = context.getIdentifier();
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
