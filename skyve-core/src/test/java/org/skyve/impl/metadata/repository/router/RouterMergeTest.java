package org.skyve.impl.metadata.repository.router;

import static org.hamcrest.CoreMatchers.hasItem;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.Test;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.web.WebAction;

public class RouterMergeTest {
	@Test
	public void testMerge() {
		final Router router = XMLMetaData.unmarshalRouterFile(this.getClass().getResource("router.xml").getPath());

		final Router routerToMerge = new Router();
		final UxUiMetadata newUxUi = new UxUiMetadata();
		newUxUi.setName("NewUxUi");
		routerToMerge.getUxUis().add(newUxUi);

		final UxUiMetadata existingDesktopUxUi = new UxUiMetadata();
		existingDesktopUxUi.setName("desktop");
		final Route newDesktopRoute = new Route();
		newDesktopRoute.setOutcomeUrl("/new/desktop/outcome");
		final RouteCriteria newRouteCriteria = new RouteCriteria();
		newRouteCriteria.setWebAction(WebAction.e);
		newDesktopRoute.getCriteria().add(newRouteCriteria);
		newDesktopRoute.getProperties().put("newPropertyKey", "newPropertyValue");
		existingDesktopUxUi.getRoutes().add(newDesktopRoute);
		routerToMerge.getUxUis().add(existingDesktopUxUi);

		router.merge(routerToMerge);
		assertThat(router.getUxuiSelectorClassName(), is("router.DefaultUxUiSelector"));
		assertThat(getUxUiFromRouter(router, newUxUi.getName()), is(newUxUi));
		assertThat(getUxUiFromRouter(router, existingDesktopUxUi.getName()).getRoutes(), hasItem(newDesktopRoute));
		assertThat(getRouteFromUxUi(getUxUiFromRouter(router, existingDesktopUxUi.getName()), newDesktopRoute.getOutcomeUrl()).getCriteria(), hasItem(newRouteCriteria));
		assertThat(getRouteFromUxUi(getUxUiFromRouter(router, existingDesktopUxUi.getName()), newDesktopRoute.getOutcomeUrl()).getProperties().get("newPropertyKey"), is("newPropertyValue"));
	}

	private static UxUiMetadata getUxUiFromRouter(Router router, String uxUiName) {
		return router.getUxUis().stream()
				.filter(uxui -> uxUiName.equals(uxui.getName()))
				.findFirst().orElse(null);
	}

	private static Route getRouteFromUxUi(UxUiMetadata uxUi, String routeOutcome) {
		return uxUi.getRoutes().stream()
				.filter(route -> routeOutcome.equals(route.getOutcomeUrl()))
				.findFirst().orElse(null);
	}
}
