package org.skyve.impl.sail.execution.pf;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import org.skyve.domain.Bean;
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
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.web.UserAgentType;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
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
	private Map<String, ListModel<Bean>> listModels = new TreeMap<>();

	/**
	 * Generates list-view components and widgets for the supplied PrimeFaces list context.
	 *
	 * @param listContext the list generation context
	 */
	@Override
	public void generate(@Nonnull PrimeFacesGenerateListContext listContext) {
		PushListContext push = listContext.pushListContext();
		ComponentBuilder componentBuilder = listContext.componentBuilder();

		String moduleName = push.getModuleName();
		String documentName = push.getDocumentName();
		String queryName = push.getQueryName();
		String modelName = push.getModelName();
		String uxui = getUxui();
		if (uxui == null) {
			throw new IllegalStateException("The automation context has no uxui - push it before generating");
		}

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
				uxui,
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
	public void generate(@Nonnull PrimeFacesGenerateEditContext editContext) {
		PushEditContext push = editContext.pushEditContext();
		ComponentBuilder componentBuilder = editContext.componentBuilder();
		LayoutBuilder layoutBuilder = editContext.layoutBuilder();
		String uxui = getUxui();
		UserAgentType userAgentType = getUserAgentType();
		if ((uxui == null) || (userAgentType == null)) {
			throw new IllegalStateException("The automation context has no uxui or user agent type - push it before generating");
		}

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
				uxui,
				userAgentType,
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
	void put(@Nonnull String identifier, @Nonnull UIComponent component, @Nonnull Object widget) {
		List<UIComponent> componentList = components.computeIfAbsent(identifier, key -> new ArrayList<>());
		componentList.add(component);

		List<Object> widgetList = widgets.computeIfAbsent(identifier, key -> new ArrayList<>());
		widgetList.add(widget);
	}
	
	/**
	 * Records the list model whose columns a list grid shows, under the grid's identifier.
	 *
	 * @param identifier the list grid's identifier, as list grid steps name it
	 * @param model the grid's list model
	 */
	void putListModel(@Nonnull String identifier, @Nonnull ListModel<Bean> model) {
		listModels.put(identifier, model);
	}

	/**
	 * Returns every logical identifier that has collected components, in identifier order.
	 *
	 * @return the identifiers; a live, unmodifiable view
	 */
	public @Nonnull Set<String> getIdentifiers() {
		return Collections.unmodifiableSet(components.keySet());
	}

	/**
	 * Returns the list model whose columns a list grid shows.
	 *
	 * @param identifier the list grid's identifier, as list grid steps name it
	 * @return the list model, or {@code null} when no list grid was collected under the identifier
	 */
	public @Nullable ListModel<Bean> getListModel(@Nonnull String identifier) {
		return listModels.get(identifier);
	}

	/**
	 * Returns collected Faces components for a logical identifier.
	 *
	 * @param identifier the logical identifier to resolve
	 * @return the collected Faces components, or {@code null} when none are recorded
	 */
	public @Nullable List<UIComponent> getFacesComponents(@Nonnull String identifier) {
		return components.get(identifier);
	}

	/**
	 * Returns collected Skyve widgets for a logical identifier.
	 *
	 * @param identifier the logical identifier to resolve
	 * @return the collected widgets, or {@code null} when none are recorded
	 */
	public @Nullable List<Object> getSkyveWidgets(@Nonnull String identifier) {
		return widgets.get(identifier);
	}

	/**
	 * Resolves the client ID for a row-aware component reference.
	 *
	 * @param component the component whose client ID is required
	 * @param row the row index to inject before the terminal client-ID segment
	 * @return the row-aware client ID
	 */
	public static @Nonnull String clientId(@Nonnull UIComponent component, @Nonnull Integer row) {
		String id = clientId(component);

		int lastColonIndex = id.lastIndexOf(':');
		if (lastColonIndex > -1) {
			id = String.format(Locale.ENGLISH, "%s:%d%s", id.substring(0, lastColonIndex), row, id.substring(lastColonIndex)); // ASCII digits
		} else {
			id = String.format(Locale.ENGLISH, "%s:%d", id, row);
		}
		
		return id;
	}
	
	/**
	 * Resolves the full client ID for a Faces component by traversing naming containers.
	 *
	 * @param component the component whose client ID is required
	 * @return the resolved client ID
	 */
	public static @Nonnull String clientId(@Nonnull UIComponent component) {
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
	private static void clientId(@Nonnull UIComponent component, @Nonnull StringBuilder clientId) {
		if (clientId.isEmpty()) {
			clientId.append(component.getId());
		} else {
			clientId.insert(0, ':').insert(0, component.getId());
		}
	}
}
