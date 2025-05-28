package org.skyve.impl.sail.execution;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.metadata.sail.language.step.context.PushEditContext;
import org.skyve.metadata.sail.language.step.context.PushListContext;

public class SmartClientAutomationContext extends AutomationContext {
	private static int windowNumber = Integer.MIN_VALUE;
	
	private Map<String, List<String>> locators = new TreeMap<>();

	public SmartClientAutomationContext() {
		// nothing to see here
	}

	void put(String identifier, String locator) {
System.out.println(identifier + " -> " + locator);
		List<String> locatorList = locators.get(identifier);
		if (locatorList == null) {
			locatorList = new ArrayList<>();
			locators.put(identifier, locatorList);
		}
		locatorList.add(locator);
	}
	
	public List<String> getLocators(String identifier) {
		return locators.get(identifier);
	}
/*
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
*/
	public void generate(PushListContext push) {
		String moduleName = push.getModuleName();
		String documentName = push.getDocumentName();
		String queryName = push.getQueryName();
		String modelName = push.getModelName();

		// populate the locators
		if (windowNumber < 0) {
			
		}
		else {
			
		}
/*
		ComponentCollectingComponentBuilder cccb = new ComponentCollectingComponentBuilder(this, push);
		ComponentBuilderChain cbc = new ComponentBuilderChain(componentBuilder, cccb);
		cbc.setSAILManagedBean(new FacesView());
		cbc.setManagedBeanName("skyve");
		cbc.setUserAgentType(getUserAgentType());
		
		ListGrid.generate(moduleName, 
							documentName, 
							queryName, 
							modelName,
							getUxui(),
							Boolean.TRUE,
							false,
							Boolean.TRUE,
							false,
							Boolean.TRUE,
							cbc);
*/
	}
	
	public void generate(PushEditContext push) {
/* UNCOMMENT AND FIX THIS
		ComponentCollectingComponentBuilder cccb = new ComponentCollectingComponentBuilder(this, push);
		ComponentBuilderChain cbc = new ComponentBuilderChain(componentBuilder, cccb);
		ComponentCollectingLayoutBuilder cclb = new ComponentCollectingLayoutBuilder(cccb);
		LayoutBuilderChain lbc = new LayoutBuilderChain(layoutBuilder, cclb);
		
		FacesView managedBean = new FacesView();
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
*/
	}
}
