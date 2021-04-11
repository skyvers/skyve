package org.skyve.impl.metadata.repository.router;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class RouterMerger {
	/**
	 * Merges subsequent routers into the first router.
	 * If the route exists already then the route criteria are added.
	 * If the route does not exist already, it is inserted in the order defined at the top of the list.
	 * Thus the broadest routes should go in the global router and the module router should be mainly for public pages. 
	 * @param routersToMerge	List of routers to merge into router at element 0.
	 * @return	Element 0 from the routersToMerge list.
	 */
	@SuppressWarnings("static-method")
	public Router mergeRouters(List<Router> routersToMerge) {
		final Router mergedRouter = routersToMerge.get(0);
		for (Router routerToMerge : routersToMerge.subList(1, routersToMerge.size())) {
			final List<UxUiMetadata> uxuiMetadata = routerToMerge.getUxUis();
			final List<UxUiMetadata> newUxUis = new ArrayList<>();
			for (UxUiMetadata uxuiMetadatum : uxuiMetadata) {
				final UxUiMetadata existingUxuiMetadatum = mergedRouter.getUxUis().stream()
						.filter(uxui -> Objects.equals(uxui.getName(), uxuiMetadatum.getName()))
						.findFirst().orElse(null);
				if (existingUxuiMetadatum == null) {
					newUxUis.add(uxuiMetadatum);
				} else {
					for (int i = 0; i < uxuiMetadatum.getRoutes().size(); i++) {
						final Route routeToMerge = uxuiMetadatum.getRoutes().get(i);
						final Route existingRoute = existingUxuiMetadatum.getRoutes().stream()
								.filter(route -> Objects.equals(route.getOutcomeUrl(), routeToMerge.getOutcomeUrl()))
								.findFirst().orElse(null);
						if (existingRoute == null) {
							existingUxuiMetadatum.getRoutes().add(i, routeToMerge);
						} else {
							existingRoute.getCriteria().addAll(routeToMerge.getCriteria());
							existingRoute.getProperties().putAll(routeToMerge.getProperties());
						}
					}
				}
			}
			mergedRouter.getUxUis().addAll(newUxUis);

			mergedRouter.getUnsecuredUrlPrefixes().addAll(routerToMerge.getUnsecuredUrlPrefixes());
		}
		
		// Now, add any unsecured routes
		List<RouteCriteria> unsecuredRoutes = mergedRouter.getUnsecuredRoutes();
		for (UxUiMetadata uxui : mergedRouter.getUxUis()) {
			for (Route route : uxui.getRoutes()) {
				if (mergedRouter.isUnsecured(route.getOutcomeUrl())) {
					for (RouteCriteria criteria : route.getCriteria()) {
						if ((criteria.getUserId() == null) && (criteria.getDataGroupId() == null)) {
							unsecuredRoutes.add(criteria);
						}
					}
				}
			}
		}
		
		return mergedRouter;
	}
}
