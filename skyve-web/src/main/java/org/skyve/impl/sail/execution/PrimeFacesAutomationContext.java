package org.skyve.impl.sail.execution;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import javax.faces.component.NamingContainer;
import javax.faces.component.UIComponent;

import org.skyve.impl.web.faces.beans.FacesView;
import org.skyve.impl.web.faces.components.ListGrid;
import org.skyve.impl.web.faces.components.View;
import org.skyve.impl.web.faces.pipeline.component.ComponentBuilder;
import org.skyve.impl.web.faces.pipeline.component.ComponentBuilderChain;
import org.skyve.impl.web.faces.pipeline.layout.LayoutBuilder;
import org.skyve.impl.web.faces.pipeline.layout.LayoutBuilderChain;
import org.skyve.metadata.sail.language.step.context.PushEditContext;
import org.skyve.metadata.sail.language.step.context.PushListContext;

public class PrimeFacesAutomationContext extends AutomationContext {
	private Map<String, List<UIComponent>> components = new TreeMap<>();
	private Map<String, List<Object>> widgets = new TreeMap<>();

	public PrimeFacesAutomationContext() {
		// nothing to see here
	}

	void put(String identifier, UIComponent component, Object widget) {
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

	public void generate(PushListContext push, ComponentBuilder componentBuilder) {
		String moduleName = push.getModuleName();
		String documentName = push.getDocumentName();
		String queryName = push.getQueryName();
		String modelName = push.getModelName();

		ComponentCollectingComponentBuilder cccb = new ComponentCollectingComponentBuilder(this, push);
		ComponentBuilderChain cbc = new ComponentBuilderChain(componentBuilder, cccb);
		cbc.setSAILManagedBean(new FacesView<>());
		cbc.setManagedBeanName("skyve");
		cbc.setUserAgentType(getUserAgentType());
	
		ListGrid.generate(moduleName, 
							documentName, 
							queryName, 
							modelName, 
							Boolean.TRUE,
							false,
							Boolean.TRUE,
							false,
							Boolean.TRUE,
							cbc);
	}
	
	public void generate(PushEditContext push,
							ComponentBuilder componentBuilder,
							LayoutBuilder layoutBuilder) {
		ComponentCollectingComponentBuilder cccb = new ComponentCollectingComponentBuilder(this, push);
		ComponentBuilderChain cbc = new ComponentBuilderChain(componentBuilder, cccb);
		ComponentCollectingLayoutBuilder cclb = new ComponentCollectingLayoutBuilder(cccb);
		LayoutBuilderChain lbc = new LayoutBuilderChain(layoutBuilder, cclb);
		
		FacesView<?> managedBean = new FacesView<>();
		cbc.setSAILManagedBean(managedBean);
		lbc.setSAILManagedBean(managedBean);

		View.generate(push.getModuleName(), 
						push.getDocumentName(), 
						null,
						"skyve", 
						getUxui(), 
						getUserAgentType(),
						null,
						null,
						cbc,
						lbc);
	}
}
