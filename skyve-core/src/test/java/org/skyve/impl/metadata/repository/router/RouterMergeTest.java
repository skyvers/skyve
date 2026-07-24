package org.skyve.impl.metadata.repository.router;

import static org.hamcrest.CoreMatchers.hasItem;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.router.Direct.DirectMatch;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.web.UserAgentType;
import org.skyve.web.WebAction;

@SuppressWarnings({ "java:S1192", "java:S5960" }) // Repeated route fixtures and assertions are test-only.
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
		assertThrows(MetaDataException.class, () -> router.merge(routerToMerge));
	}

	@Test
	@SuppressWarnings("static-method")
	void directsUseRouteStyleGlobalAndModuleMergeOrder() {
		Router effective = new Router();
		effective.getDirects().add(direct("/target.xhtml", "global"));
		effective.convert("global");

		Router earlierModule = new Router();
		earlierModule.getDirects().add(direct("/target.xhtml", "earlier-first"));
		earlierModule.getDirects().add(direct("/target.xhtml", "earlier-second"));
		earlierModule.convert("earlier module");
		effective.merge(earlierModule);

		Router laterModule = new Router();
		laterModule.getDirects().add(direct("/target.xhtml", "later-first"));
		laterModule.getDirects().add(direct("/target.xhtml", "later-second"));
		laterModule.convert("later module");
		effective.merge(laterModule);

		assertEquals(5, effective.getDirects().size());
		assertEquals("later-first", effective.getDirects().get(0).getUxui());
		assertEquals("later-second", effective.getDirects().get(1).getUxui());
		assertEquals("earlier-first", effective.getDirects().get(2).getUxui());
		assertEquals("earlier-second", effective.getDirects().get(3).getUxui());
		assertEquals("global", effective.getDirects().get(4).getUxui());
	}

	@Test
	@SuppressWarnings("static-method")
	void prependedModulePrefixCanMaskGlobalExactDeclaration() {
		Router effective = new Router();
		effective.getDirects().add(direct("/external/startup.xhtml", "global-exact"));
		effective.convert("global");

		Router module = new Router();
		Direct prefix = direct("/external/", "module-prefix");
		prefix.setMatch(DirectMatch.prefix);
		module.getDirects().add(prefix);
		module.convert("module");
		effective.merge(module);

		assertEquals("module-prefix",
				effective.selectDirect("/external/startup.xhtml", UserAgentType.desktop));
	}

	private static Direct direct(String path, String uxui) {
		Direct result = new Direct();
		result.setPath(path);
		result.setUxui(uxui);
		return result;
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
