package org.skyve.impl.sail.execution.pf;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.web.faces.components.ListGrid;
import org.skyve.impl.web.faces.components.View;
import org.skyve.impl.web.faces.pipeline.component.ComponentBuilder;
import org.skyve.impl.web.faces.pipeline.component.ComponentBuilderChain;
import org.skyve.impl.web.faces.pipeline.layout.LayoutBuilder;
import org.skyve.impl.web.faces.pipeline.layout.LayoutBuilderChain;
import org.skyve.impl.web.faces.views.FacesView;
import org.skyve.metadata.sail.language.step.context.PushEditContext;
import org.skyve.metadata.sail.language.step.context.PushListContext;

import jakarta.faces.component.NamingContainer;
import jakarta.faces.component.UIComponent;

/**
 * Automation context implementation for PrimeFaces client, handling list and edit contexts specific to PrimeFaces UI components.
 * 
 * @author mike
 */
public class PrimeFacesAutomationContext extends AutomationContext<PrimeFacesGenerateListContext, PrimeFacesGenerateEditContext> {

	private Map<String, List<UIComponent>> components = new TreeMap<>();
	private Map<String, List<Object>> widgets = new TreeMap<>();

	/**
	 * Generates list-view components and widgets for the supplied PrimeFaces list context.
	 *
	 * @param listContext the list generation context
	 */
	@Override
	public void generate(PrimeFacesGenerateListContext listContext) {
		PushListContext push = listContext.pushListContext();
		ComponentBuilder componentBuilder = listContext.componentBuilder();

		String moduleName = push.getModuleName();
		String documentName = push.getDocumentName();
		String queryName = push.getQueryName();
		String modelName = push.getModelName();

		ComponentCollectingComponentBuilder cccb = new ComponentCollectingComponentBuilder(this, push);
		ComponentBuilderChain cbc = new ComponentBuilderChain(componentBuilder, cccb);
		cbc.setSAILManagedBean(new FacesView());
		cbc.setManagedBeanName("skyve");
		cbc.setUserAgentType(getUserAgentType());

		ListGrid.generate(
				moduleName,
				documentName,
				queryName,
				modelName,
				getUxui(),
				Boolean.TRUE,
				false,
				Boolean.TRUE,
				false,
				Boolean.TRUE,
				null,
				cbc);
	}

	/**
	 * Generates edit-view components and widgets for the supplied PrimeFaces edit context.
	 *
	 * @param editContext the edit generation context
	 */
	@Override
	public void generate(PrimeFacesGenerateEditContext editContext) {
		PushEditContext push = editContext.pushEditContext();
		ComponentBuilder componentBuilder = editContext.componentBuilder();
		LayoutBuilder layoutBuilder = editContext.layoutBuilder();

		ComponentCollectingComponentBuilder cccb = new ComponentCollectingComponentBuilder(this, push);
		ComponentBuilderChain cbc = new ComponentBuilderChain(componentBuilder, cccb);
		ComponentCollectingLayoutBuilder cclb = new ComponentCollectingLayoutBuilder(cccb);
		LayoutBuilderChain lbc = new LayoutBuilderChain(layoutBuilder, cclb);

		FacesView managedBean = new FacesView();
		cbc.setSAILManagedBean(managedBean);
		lbc.setSAILManagedBean(managedBean);

		View.generate(
				push.getModuleName(),
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

	/**
	 * Records a component and widget pair for the supplied logical identifier.
	 *
	 * @param identifier the logical identifier used by SAIL steps
	 * @param component the Faces component associated with the identifier
	 * @param widget the Skyve widget associated with the identifier
	 */
	void put(String identifier, UIComponent component, Object widget) {
		List<UIComponent> componentList = components.computeIfAbsent(identifier, key -> new ArrayList<>());
		componentList.add(component);

		List<Object> widgetList = widgets.computeIfAbsent(identifier, key -> new ArrayList<>());
		widgetList.add(widget);
	}
	
	/**
	 * Returns collected Faces components for a logical identifier.
	 *
	 * @param identifier the logical identifier to resolve
	 * @return the collected Faces components, or {@code null} when none are recorded
	 */
	public List<UIComponent> getFacesComponents(String identifier) {
		return components.get(identifier);
	}

	/**
	 * Returns collected Skyve widgets for a logical identifier.
	 *
	 * @param identifier the logical identifier to resolve
	 * @return the collected widgets, or {@code null} when none are recorded
	 */
	public List<Object> getSkyveWidgets(String identifier) {
		return widgets.get(identifier);
	}

	/**
	 * Resolves the client ID for a row-aware component reference.
	 *
	 * @param component the component whose client ID is required
	 * @param row the row index to inject before the terminal client-ID segment
	 * @return the row-aware client ID
	 */
	public static String clientId(UIComponent component, Integer row) {
		String id = clientId(component);

		int lastColonIndex = id.lastIndexOf(':');
		if (lastColonIndex > -1) {
			id = String.format("%s:%d%s", id.substring(0, lastColonIndex), row, id.substring(lastColonIndex));
		} else {
			id = String.format("%s:%d", id, row);
		}
		
		return id;
	}
	
	/**
	 * Resolves the full client ID for a Faces component by traversing naming containers.
	 *
	 * @param component the component whose client ID is required
	 * @return the resolved client ID
	 */
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
	
	/**
	 * Prepends a component ID segment to the accumulating client-ID builder.
	 *
	 * @param component the component providing the ID segment
	 * @param clientId the client-ID accumulator
	 */
	private static void clientId(UIComponent component, StringBuilder clientId) {
		if (clientId.isEmpty()) {
			clientId.append(component.getId());
		} else {
			clientId.insert(0, ':').insert(0, component.getId());
		}
	}
}
