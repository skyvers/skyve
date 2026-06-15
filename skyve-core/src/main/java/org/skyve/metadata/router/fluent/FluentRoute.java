package org.skyve.metadata.router.fluent;

import java.util.ArrayList;
import java.util.List;

import org.skyve.impl.metadata.repository.router.Route;
import org.skyve.impl.metadata.repository.router.RouteCriteria;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.web.WebAction;

/**
 * Builds one route outcome and its match criteria.
 */
public class FluentRoute {
	private Route route = null;
	
	/**
	 * Creates a builder with new empty route metadata.
	 */
	public FluentRoute() {
		route = new Route();
	}

	/**
	 * Creates a builder around existing route metadata.
	 *
	 * @param route backing route metadata
	 */
	public FluentRoute(Route route) {
		this.route = route;
	}

	/**
	 * Copies route metadata into this builder.
	 *
	 * @param route source route metadata
	 * @return this builder
	 */
	public FluentRoute from(@SuppressWarnings("hiding") Route route) {
		outcomeUrl(route.getOutcomeUrl());
		route.getCriteria().forEach(c -> addCriteria(new FluentRouteCriteria().from(c)));
		return this;
	}
	
	/**
	 * Sets the route outcome URL.
	 *
	 * @param outcomeUrl outcome URL
	 * @return this builder
	 */
	public FluentRoute outcomeUrl(String outcomeUrl) {
		route.setOutcomeUrl(outcomeUrl);
		return this;
	}

	/**
	 * Adds one route criteria block.
	 *
	 * @param criteria criteria wrapper
	 * @return this builder
	 */
	public FluentRoute addCriteria(FluentRouteCriteria criteria) {
		route.getCriteria().add(criteria.get());
		return this;
	}
	
	/**
	 * Removes criteria matching the non-null fields of an example criteria object.
	 *
	 * @param exampleCriteria criteria template; null fields are treated as wildcards
	 * @return this builder
	 */
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
	
	/**
	 * Finds criteria matching the non-null fields of an example criteria object.
	 *
	 * @param exampleCriteria criteria template; null fields are treated as wildcards
	 * @return matching criteria wrappers, possibly empty
	 */
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

	/**
	 * Removes all route criteria blocks.
	 *
	 * @return this builder
	 */
	public FluentRoute clearCriteria() {
		route.getCriteria().clear();
		return this;
	}

	/**
	 * Returns the mutable metadata instance being built.
	 *
	 * @return backing route metadata
	 */
	public Route get() {
		return route;
	}
}
