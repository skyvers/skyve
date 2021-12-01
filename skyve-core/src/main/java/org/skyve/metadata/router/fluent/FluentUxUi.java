package org.skyve.metadata.router.fluent;

import org.skyve.impl.metadata.repository.router.UxUiMetadata;

public class FluentUxUi {
	private UxUiMetadata uxui = null;
	
	public FluentUxUi() {
		uxui = new UxUiMetadata();
	}

	public FluentUxUi(UxUiMetadata uxui) {
		this.uxui = uxui;
	}

	public FluentUxUi from(@SuppressWarnings("hiding") UxUiMetadata uxui) {
		name(uxui.getName());
		uxui.getRoutes().forEach(r -> addRoute(new FluentRoute().from(r)));
		return this;
	}
	
	public FluentUxUi name(String name) {
		uxui.setName(name);
		return this;
	}
	
	public FluentUxUi addRoute(FluentRoute route) {
		uxui.getRoutes().add(route.get());
		return this;
	}
	
	public UxUiMetadata get() {
		return uxui;
	}
}
