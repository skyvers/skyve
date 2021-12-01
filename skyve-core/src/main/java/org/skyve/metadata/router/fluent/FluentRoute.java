package org.skyve.metadata.router.fluent;

import org.skyve.impl.metadata.repository.router.Route;

public class FluentRoute {
	private Route route = null;
	
	public FluentRoute() {
		route = new Route();
	}

	public FluentRoute(Route route) {
		this.route = route;
	}

	public FluentRoute from(@SuppressWarnings("hiding") Route route) {
		outcomeUrl(route.getOutcomeUrl());
		route.getCriteria().forEach(c -> addCriteria(new FluentRouteCriteria().from(c)));
		return this;
	}
	
	public FluentRoute outcomeUrl(String outcomeUrl) {
		route.setOutcomeUrl(outcomeUrl);
		return this;
	}

	public FluentRoute addCriteria(FluentRouteCriteria criteria) {
		route.getCriteria().add(criteria.get());
		return this;
	}

	public Route get() {
		return route;
	}
}
