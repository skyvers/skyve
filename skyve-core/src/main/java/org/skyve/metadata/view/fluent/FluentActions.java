package org.skyve.metadata.view.fluent;

import java.util.Collection;

import org.skyve.impl.metadata.repository.view.Actions;
import org.skyve.metadata.view.Action;

public class FluentActions {
	private Actions actions = null;
	
	public FluentActions() {
		actions = new Actions();
	}
	
	public FluentActions(Actions actions) {
		this.actions = actions;
	}
	
	public FluentActions from(String widgetId, @SuppressWarnings("hiding") Collection<Action> actions) {
		widgetId(widgetId);
		actions.forEach(a -> addAction(FluentAction.from(a)));
		return this;
	}
	
	public FluentActions widgetId(String widgetId) {
		actions.setWidgetId(widgetId);
		return this;
	}
	
	public FluentActions addAction(FluentAction<?> action) {
		actions.getActions().add(action.get());
		return this;
	}
	
	public Actions get() {
		return actions;
	}
}
