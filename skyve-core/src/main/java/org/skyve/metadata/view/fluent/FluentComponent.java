package org.skyve.metadata.view.fluent;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.view.component.Component;
import org.skyve.impl.metadata.view.component.ComponentTranslation;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Builds {@link Component} metadata instances using fluent mutation methods.
 */
public class FluentComponent extends FluentBoundWidget<FluentComponent> {
	private Component component = null;

	/**
	 * Creates a builder backed by a new {@link Component}.
	 */
	public FluentComponent() {
		component = new Component();
	}

	/**
	 * Creates a builder backed by the supplied {@link Component}.
	 *
	 * @param component
	 *            the metadata instance to mutate
	 */
	public FluentComponent(Component component) {
		this.component = component;
	}

	/**
	 * Copies all component state from runtime metadata into this builder.
	 *
	 * @param component
	 *            the source metadata to copy from
	 * @return this builder
	 */
	public FluentComponent from(@SuppressWarnings("hiding") Component component) {
		moduleName(component.getModuleName());
		documentName(component.getDocumentName());
		name(component.getName());
		widgetId(component.getWidgetId());

		BindUtil.copyAuthoredCondition(component.getXmlInvisibleCondition(), component.getXmlVisibleCondition(), this::invisibleCondition, this::visibleCondition);

		for (ComponentTranslation action : component.getActions()) {
			addAction(action.getFromComponent(), action.getMappedTo());
		}
		for (ComponentTranslation condition : component.getConditions()) {
			addCondition(condition.getFromComponent(), condition.getMappedTo());
		}

		super.from(component);
		
		component.getProperties().entrySet().forEach(p -> putProperty(p.getKey(), p.getValue()));
		
		return this;
	}

	/**
	 * Sets the owning module name for the referenced component.
	 *
	 * @param moduleName
	 *            the module containing the component definition
	 * @return this builder
	 */
	public FluentComponent moduleName(String moduleName) {
		component.setModuleName(moduleName);
		return this;
	}

	/**
	 * Sets the owning document name for the referenced component.
	 *
	 * @param documentName
	 *            the document containing the component definition
	 * @return this builder
	 */
	public FluentComponent documentName(String documentName) {
		component.setDocumentName(documentName);
		return this;
	}

	/**
	 * Sets the component name to resolve within the owning document.
	 *
	 * @param name
	 *            the component name
	 * @return this builder
	 */
	public FluentComponent name(String name) {
		component.setName(name);
		return this;
	}

	/**
	 * Sets the widget identifier used for rendered component instances.
	 *
	 * @param widgetId
	 *            the widget identifier
	 * @return this builder
	 */
	public FluentComponent widgetId(String widgetId) {
		component.setWidgetId(widgetId);
		return this;
	}

	/**
	 * Sets the {@code invisible} condition as authored; the opposite sense of {@link #visibleCondition(String)}.
	 *
	 * @param invisibleCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentComponent invisibleCondition(@Nullable String invisibleCondition) {
		component.setXmlInvisibleCondition(invisibleCondition);
		return this;
	}

	/**
	 * Sets the {@code visible} condition as authored; the opposite sense of {@link #invisibleCondition(String)}.
	 *
	 * @param visibleCondition
	 *            a condition name, an inline condition expression or a boolean literal
	 * @return this builder
	 */
	public @Nonnull FluentComponent visibleCondition(@Nullable String visibleCondition) {
		component.setXmlVisibleCondition(visibleCondition);
		return this;
	}

	/**
	 * Adds an action mapping, authored as an {@code <action/>} within {@code <actions>}.
	 *
	 * @param fromComponent
	 *            the action name used in the component view
	 * @param mappedTo
	 *            the action name to use in the containing view
	 * @return this builder
	 */
	public @Nonnull FluentComponent addAction(@Nonnull String fromComponent, @Nonnull String mappedTo) {
		component.getActions().add(newTranslation(fromComponent, mappedTo));
		return this;
	}

	/**
	 * Removes all action mappings.
	 *
	 * @return this builder
	 */
	public @Nonnull FluentComponent clearActions() {
		component.getActions().clear();
		return this;
	}

	/**
	 * Adds a condition mapping, authored as a {@code <condition/>} within {@code <conditions>}.
	 *
	 * @param fromComponent
	 *            the condition used in the component view: a condition name, or an inline condition expression
	 *            exactly as it is written there
	 * @param mappedTo
	 *            the condition to use in the containing view: a condition name or an inline condition expression
	 * @return this builder
	 */
	public @Nonnull FluentComponent addCondition(@Nonnull String fromComponent, @Nonnull String mappedTo) {
		component.getConditions().add(newTranslation(fromComponent, mappedTo));
		return this;
	}

	/**
	 * Removes all condition mappings.
	 *
	 * @return this builder
	 */
	public @Nonnull FluentComponent clearConditions() {
		component.getConditions().clear();
		return this;
	}

	private static @Nonnull ComponentTranslation newTranslation(@Nonnull String fromComponent, @Nonnull String mappedTo) {
		ComponentTranslation result = new ComponentTranslation();
		result.setFromComponent(fromComponent);
		result.setMappedTo(mappedTo);
		return result;
	}

	/**
	 * Adds or replaces a custom property on the wrapped metadata.
	 * Properties keep the order in which they were first added.
	 *
	 * @param k the property name
	 * @param v the property value
	 * @return this builder
	 */
	public @Nonnull FluentComponent putProperty(@Nonnull String k, @Nonnull String v) {
		component.getProperties().put(k, v);
		return this;
	}

	/**
	 * Returns the underlying mutable metadata.
	 *
	 * @return the wrapped component metadata
	 */
	@Override
	public Component get() {
		return component;
	}
}
