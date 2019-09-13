package org.skyve.impl.metadata.repository.router;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class RouterMerger {
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
					for (Route routeToMerge : uxuiMetadatum.getRoutes()) {
						final Route existingRoute = existingUxuiMetadatum.getRoutes().stream()
								.filter(route -> Objects.equals(route.getOutcomeUrl(), routeToMerge.getOutcomeUrl()))
								.findFirst().orElse(null);
						if (existingRoute == null) {
							existingUxuiMetadatum.getRoutes().add(routeToMerge);
						} else {
							existingRoute.getCriteria().addAll(routeToMerge.getCriteria());
							existingRoute.getProperties().putAll(routeToMerge.getProperties());
						}
					}
					existingUxuiMetadatum.getRoutes().addAll(uxuiMetadatum.getRoutes());
				}
			}
			mergedRouter.getUxUis().addAll(newUxUis);
		}
		return mergedRouter;
	}
}
