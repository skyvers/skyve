package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.component.Component;

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

		invisibleConditionName(component.getInvisibleConditionName());

		super.from(component);
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
	 * Sets the condition name controlling component visibility.
	 *
	 * @param invisibleConditionName
	 *            the condition name that hides the component when satisfied
	 * @return this builder
	 */
	public FluentComponent invisibleConditionName(String invisibleConditionName) {
		component.setInvisibleConditionName(invisibleConditionName);
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
