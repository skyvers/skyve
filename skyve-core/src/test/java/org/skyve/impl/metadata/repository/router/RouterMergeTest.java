package org.skyve.impl.metadata.repository.router;

import static org.hamcrest.CoreMatchers.hasItem;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.web.WebAction;

class RouterMergeTest {
	private Router router;
	private Router routerToMerge = new Router();
	private UxUiMetadata existingDesktopUxUi = new UxUiMetadata();
	private UxUiMetadata newUxUi = new UxUiMetadata();
	private Route newDesktopRoute = new Route();	
	private RouteCriteria newRouteCriteria = new RouteCriteria();
	
	private void createTestData() {
		router = XMLMetaData.unmarshalRouterFile(this.getClass().getResource("router.xml").getPath());

		newUxUi.setName("NewUxUi");
		routerToMerge.getUxUis().add(newUxUi);

		existingDesktopUxUi.setName("desktop");
		newDesktopRoute.setOutcomeUrl("/new/desktop/outcome");
		
		newRouteCriteria.setWebAction(WebAction.e);
		newDesktopRoute.getCriteria().add(newRouteCriteria);
		newDesktopRoute.getProperties().put("newPropertyKey", "newPropertyValue");
		existingDesktopUxUi.getRoutes().add(newDesktopRoute);
		routerToMerge.getUxUis().add(existingDesktopUxUi);
	}
	
	
	@Test
	void testMerge() {
		createTestData();
		newRouteCriteria.setModuleName("admin");
		
		router.merge(routerToMerge);
		
		assertThat(router.getUxuiSelectorClassName(), is("router.DefaultUxUiSelector"));
		assertThat(getUxUiFromRouter(router, newUxUi.getName()), is(newUxUi));
		assertThat(getUxUiFromRouter(router, existingDesktopUxUi.getName()).getRoutes(), hasItem(newDesktopRoute));
		assertThat(getRouteFromUxUi(getUxUiFromRouter(router, existingDesktopUxUi.getName()), newDesktopRoute.getOutcomeUrl()).getCriteria(), hasItem(newRouteCriteria));
		assertThat(getRouteFromUxUi(getUxUiFromRouter(router, existingDesktopUxUi.getName()), newDesktopRoute.getOutcomeUrl()).getProperties().get("newPropertyKey"), is("newPropertyValue"));
	}

	@Test
	void testBadMergeWithNoModuleName() {
		createTestData();
		assertThrows(MetaDataException.class, () -> {
			router.merge(routerToMerge);
		});
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
