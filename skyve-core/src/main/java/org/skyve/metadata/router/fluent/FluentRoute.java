package org.skyve.metadata.router.fluent;

import java.util.ArrayList;
import java.util.List;

import org.skyve.impl.metadata.repository.router.Route;
import org.skyve.impl.metadata.repository.router.RouteCriteria;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.web.WebAction;

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
	
	public FluentRoute removeCriteria(FluentRouteCriteria exampleCriteria) {
		RouteCriteria example = exampleCriteria.get();
		String customerName = example.getCustomerName();
		String dataGroupId = example.getDataGroupId();
		String documentName = example.getDocumentName();
		String moduleName = example.getModuleName();
		String queryName = example.getQueryName();
		String userId = example.getUserId();
		ViewType viewType = example.getViewType();
		WebAction webAction = example.getWebAction();
		route.getCriteria().removeIf(c -> ((moduleName == null) || moduleName.equals(c.getModuleName())) &&
											((documentName == null) || documentName.equals(c.getDocumentName())) &&
											((customerName == null) || customerName.equals(c.getCustomerName())) &&
											((queryName == null) || queryName.equals(c.getQueryName())) &&
											((viewType == null) || viewType.equals(c.getViewType())) &&
											((userId == null) || userId.equals(c.getUserId())) &&
											((dataGroupId == null) || dataGroupId.equals(c.getDataGroupId())) &&
											((webAction == null) || webAction.equals(c.getWebAction())));
		return this;
	}
	
	public List<FluentRouteCriteria> findCriteria(FluentRouteCriteria exampleCriteria) {
		RouteCriteria example = exampleCriteria.get();
		String customerName = example.getCustomerName();
		String dataGroupId = example.getDataGroupId();
		String documentName = example.getDocumentName();
		String moduleName = example.getModuleName();
		String queryName = example.getQueryName();
		String userId = example.getUserId();
		ViewType viewType = example.getViewType();
		WebAction webAction = example.getWebAction();
		List<FluentRouteCriteria> result = new ArrayList<>(10);
		route.getCriteria().stream()
							.filter(c -> ((moduleName == null) || moduleName.equals(c.getModuleName())) &&
											((documentName == null) || documentName.equals(c.getDocumentName())) &&
											((customerName == null) || customerName.equals(c.getCustomerName())) &&
											((queryName == null) || queryName.equals(c.getQueryName())) &&
											((viewType == null) || viewType.equals(c.getViewType())) &&
											((userId == null) || userId.equals(c.getUserId())) &&
											((dataGroupId == null) || dataGroupId.equals(c.getDataGroupId())) &&
											((webAction == null) || webAction.equals(c.getWebAction())))
							.forEach(c -> result.add(new FluentRouteCriteria(c)));
		return result;
	}

	public FluentRoute clearCriteria() {
		route.getCriteria().clear();
		return this;
	}

	public Route get() {
		return route;
	}
}
