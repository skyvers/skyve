package org.skyve.metadata.router.fluent;

import org.skyve.impl.metadata.repository.router.UxUiMetadata;

public class FluentUxUi {
	private UxUiMetadata uxui = new UxUiMetadata();
	
	public FluentUxUi() {
		// nothing to see
	}

	public FluentUxUi(UxUiMetadata uxui) {
		name(uxui.getName());
		uxui.getRoutes().forEach(r -> addRoute(new FluentRoute(r)));
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
