package org.skyve.impl.metadata.repository.router;

import static org.hamcrest.CoreMatchers.hasItem;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

import java.util.Arrays;

import org.junit.Before;
import org.junit.Test;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.web.WebAction;

public class RouterMergerTest {

	private RouterMerger routerMerger;

	@Before
	public void setup() {
		routerMerger = new RouterMerger();
	}

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

		final Router mergedRouter = routerMerger.mergeRouters(Arrays.asList(router, routerToMerge));
		assertThat(mergedRouter.getUxuiSelectorClassName(), is("router.DefaultUxUiSelector"));
		assertThat(getUxUiFromRouter(mergedRouter, newUxUi.getName()), is(newUxUi));
		assertThat(getUxUiFromRouter(mergedRouter, existingDesktopUxUi.getName()).getRoutes(), hasItem(newDesktopRoute));
		assertThat(getRouteFromUxUi(getUxUiFromRouter(mergedRouter, existingDesktopUxUi.getName()), newDesktopRoute.getOutcomeUrl()).getCriteria(), hasItem(newRouteCriteria));
		assertThat(getRouteFromUxUi(getUxUiFromRouter(mergedRouter, existingDesktopUxUi.getName()), newDesktopRoute.getOutcomeUrl()).getProperties().get("newPropertyKey"), is("newPropertyValue"));
	}

	private UxUiMetadata getUxUiFromRouter(Router router, String uxUiName) {
		return router.getUxUis().stream()
				.filter(uxui -> uxUiName.equals(uxui.getName()))
				.findFirst().orElse(null);
	}

	private Route getRouteFromUxUi(UxUiMetadata uxUi, String routeOutcome) {
		return uxUi.getRoutes().stream()
				.filter(route -> routeOutcome.equals(route.getOutcomeUrl()))
				.findFirst().orElse(null);
	}
}
