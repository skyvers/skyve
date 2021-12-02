package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.component.Component;

public class FluentComponent extends FluentBound<FluentComponent> {
	private Component component = null;
	
	public FluentComponent() {
		component = new Component();
	}

	public FluentComponent(Component component) {
		this.component = component;
	}

	public FluentComponent from(@SuppressWarnings("hiding") Component component) {
		super.from(component);
		return this;
	}

	@Override
	public Component get() {
		return component;
	}
}
