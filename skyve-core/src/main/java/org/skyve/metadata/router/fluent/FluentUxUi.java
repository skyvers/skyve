package org.skyve.metadata.router.fluent;

import java.util.ArrayList;
import java.util.List;

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
	
	public FluentUxUi removeRoutes(String outcomeUrl) {
		uxui.getRoutes().removeIf(r -> outcomeUrl.equals(r.getOutcomeUrl()));
		return this;
	}

	public List<FluentRoute> findRoutes(String outcomeUrl) {
		List<FluentRoute> result = new ArrayList<>(10);
		uxui.getRoutes().stream()
						.filter(r -> outcomeUrl.equals(r.getOutcomeUrl()))
						.forEach(r -> result.add(new FluentRoute(r)));
		return result;
	}
	
	public FluentUxUi clearRoutes() {
		uxui.getRoutes().clear();
		return this;
	}

	public UxUiMetadata get() {
		return uxui;
	}
}
