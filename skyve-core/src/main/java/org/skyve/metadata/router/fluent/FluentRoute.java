package org.skyve.metadata.router.fluent;

import org.skyve.impl.metadata.repository.router.Route;

public class FluentRoute {
	private Route route = new Route();
	
	public FluentRoute() {
		// nothing to see
	}

	public FluentRoute(Route route) {
		outcomeUrl(route.getOutcomeUrl());
		route.getCriteria().forEach(c -> addCriteria(new FluentRouteCriteria(c)));
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
