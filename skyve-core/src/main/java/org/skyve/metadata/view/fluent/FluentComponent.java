package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.component.Component;

public class FluentComponent extends FluentBoundWidget<FluentComponent> {
	private Component component = null;

	public FluentComponent() {
		component = new Component();
	}

	public FluentComponent(Component component) {
		this.component = component;
	}

	public FluentComponent from(@SuppressWarnings("hiding") Component component) {

		moduleName(component.getModuleName());
		documentName(component.getDocumentName());
		name(component.getName());
		widgetId(component.getWidgetId());

		invisibleConditionName(component.getInvisibleConditionName());

		super.from(component);
		return this;
	}

	public FluentComponent moduleName(String moduleName) {
		component.setModuleName(moduleName);
		return this;
	}

	public FluentComponent documentName(String documentName) {
		component.setDocumentName(documentName);
		return this;
	}

	public FluentComponent name(String name) {
		component.setName(name);
		return this;
	}

	public FluentComponent widgetId(String widgetId) {
		component.setWidgetId(widgetId);
		return this;
	}

	public FluentComponent invisibleConditionName(String invisibleConditionName) {
		component.setInvisibleConditionName(invisibleConditionName);
		return this;
	}

	@Override
	public Component get() {
		return component;
	}
}
