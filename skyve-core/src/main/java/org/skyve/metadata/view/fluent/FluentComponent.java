package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.component.Component;
import org.skyve.metadata.MetaData;

public class FluentComponent extends FluentWidget {
	private Component component = null;
	
	public FluentComponent() {
		component = new Component();
	}

	public FluentComponent(Component component) {
		this.component = component;
	}

	public FluentComponent from(@SuppressWarnings("hiding") Component component) {
		return this;
	}

	@Override
	public MetaData get() {
		return component;
	}
}
