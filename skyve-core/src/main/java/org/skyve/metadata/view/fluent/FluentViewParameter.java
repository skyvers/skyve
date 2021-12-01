package org.skyve.metadata.view.fluent;

import org.skyve.metadata.view.View.ViewParameter;

public class FluentViewParameter {
	private ViewParameter parameter = null;
	
	public FluentViewParameter() {
		parameter = new ViewParameter();
	}

	public FluentViewParameter(ViewParameter parameter) {
		this.parameter = parameter;
	}
	
	public FluentViewParameter from(@SuppressWarnings("hiding") ViewParameter parameter) {
		fromBinding(parameter.getFromBinding());
		boundTo(parameter.getBoundTo());
		return this;
	}
	
	public FluentViewParameter fromBinding(String fromBinding) {
		parameter.setFromBinding(fromBinding);
		return this;
	}

	public FluentViewParameter boundTo(String boundTo) {
		parameter.setBoundTo(boundTo);
		return this;
	}
	
	public ViewParameter get() {
		return parameter;
	}
}
