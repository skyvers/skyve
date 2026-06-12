package org.skyve.impl.metadata.repository.router;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.MetaDataException;

class RouterTest implements TaggingUxUiSelector {

	// ---- isUnsecured ----

	@Test
	@SuppressWarnings("static-method")
	void isUnsecuredReturnsFalseForNullUrl() {
		Router router = new Router();
		assertFalse(router.isUnsecured(null));
	}

	@Test
	@SuppressWarnings("static-method")
	void isUnsecuredReturnsFalseWhenNoUnsecuredPrefixesDefined() {
		Router router = new Router();
		assertFalse(router.isUnsecured("/login"));
	}

	@Test
	@SuppressWarnings("static-method")
	void isUnsecuredReturnsTrueForMatchingPrefix() {
		Router router = new Router();
		router.getUnsecuredUrlPrefixes().add("/public/");
		assertTrue(router.isUnsecured("/public/login"));
	}

	@Test
	@SuppressWarnings("static-method")
	void isUnsecuredReturnsFalseForNonMatchingPrefix() {
		Router router = new Router();
		router.getUnsecuredUrlPrefixes().add("/public/");
		assertFalse(router.isUnsecured("/secure/login"));
	}

	@Test
	@SuppressWarnings("static-method")
	void isUnsecuredMatchesExactPrefix() {
		Router router = new Router();
		router.getUnsecuredUrlPrefixes().add("/api");
		assertTrue(router.isUnsecured("/api/v1/foo"));
	}

	// ---- getUnsecuredUrlPrefixes ----

	@Test
	@SuppressWarnings("static-method")
	void getUnsecuredUrlPrefixesInitiallyEmpty() {
		Router router = new Router();
		assertNotNull(router.getUnsecuredUrlPrefixes());
		assertEquals(0, router.getUnsecuredUrlPrefixes().size());
	}

	// ---- properties ----

	@Test
	@SuppressWarnings("static-method")
	void getPropertiesInitiallyEmpty() {
		Router router = new Router();
		assertNotNull(router.getProperties());
		assertEquals(0, router.getProperties().size());
	}

	@Test
	@SuppressWarnings("static-method")
	void getPropertiesCanAddEntries() {
		Router router = new Router();
		router.getProperties().put("key", "value");
		assertThat(router.getProperties().get("key"), is("value"));
	}

	// ---- uxuiSelectorClassName ----

	@Test
	@SuppressWarnings("static-method")
	void getUxuiSelectorClassNameNullByDefault() {
		Router router = new Router();
		assertNull(router.getUxuiSelectorClassName());
	}

	@Test
	@SuppressWarnings("static-method")
	void setUxuiSelectorClassNameAndGet() {
		Router router = new Router();
		router.setUxuiSelectorClassName("com.example.MySelector");
		assertThat(router.getUxuiSelectorClassName(), is("com.example.MySelector"));
	}

	// ---- lastModifiedMillis and lastCheckedMillis ----

	@Test
	@SuppressWarnings("static-method")
	void lastModifiedMillisDefaultIsMaxLong() {
		Router router = new Router();
		assertEquals(Long.MAX_VALUE, router.getLastModifiedMillis());
	}

	@Test
	@SuppressWarnings("static-method")
	void setLastModifiedMillisAndGet() {
		Router router = new Router();
		router.setLastModifiedMillis(12345L);
		assertEquals(12345L, router.getLastModifiedMillis());
	}

	@Test
	@SuppressWarnings("static-method")
	void setLastCheckedMillisAndGet() {
		Router router = new Router();
		router.setLastCheckedMillis(99999L);
		assertEquals(99999L, router.getLastCheckedMillis());
	}

	// ---- convert and selectRoute ----

	@Test
	@SuppressWarnings("static-method")
	void selectOutcomeUrlReturnsNullForUnknownUxUi() {
		Router router = new Router();
		router.convert("test");
		RouteCriteria criteria = new RouteCriteria();
		assertNull(router.selectOutcomeUrl("desktop", criteria));
	}

	@Test
	@SuppressWarnings("static-method")
	void convertPopulatesUxUiMapAndSelectsMatchingRoute() {
		Router router = new Router();

		// Create a UX/UI with one route that has no criteria (matches all)
		UxUiMetadata uxui = new UxUiMetadata();
		uxui.setName("desktop");
		Route route = new Route();
		route.setOutcomeUrl("/desktop/home");
		uxui.getRoutes().add(route);
		router.getUxUis().add(uxui);

		router.convert("test");

		RouteCriteria criteria = new RouteCriteria();
		assertThat(router.selectOutcomeUrl("desktop", criteria), is("/desktop/home"));
	}

	@Test
	@SuppressWarnings("static-method")
	void selectRouteReturnsNullWhenNoCriteriaMatch() {
		Router router = new Router();

		UxUiMetadata uxui = new UxUiMetadata();
		uxui.setName("mobile");

		// Route with module criteria that won't match
		Route route = new Route();
		route.setOutcomeUrl("/mobile/home");
		RouteCriteria rc = new RouteCriteria();
		rc.setModuleName("admin");
		route.getCriteria().add(rc);
		uxui.getRoutes().add(route);
		router.getUxUis().add(uxui);

		router.convert("test");

		// criteria for different module
		RouteCriteria criteria = new RouteCriteria();
		criteria.setModuleName("contacts");
		assertNull(router.selectRoute("mobile", criteria));
	}

	// ---- merge ----

	@Test
	@SuppressWarnings("static-method")
	void mergeAddsMissingUxUi() {
		Router router = new Router();

		Router routerToMerge = new Router();
		routerToMerge.setLastModifiedMillis(1000L);
		UxUiMetadata uxui = new UxUiMetadata();
		uxui.setName("tablet");
		routerToMerge.getUxUis().add(uxui);

		router.merge(routerToMerge);

		assertEquals(1, router.getUxUis().size());
		assertThat(router.getUxUis().get(0).getName(), is("tablet"));
	}

	@Test
	@SuppressWarnings("static-method")
	void mergeUpdatesLastModifiedToMax() {
		Router router = new Router();
		router.setLastModifiedMillis(500L);

		Router routerToMerge = new Router();
		routerToMerge.setLastModifiedMillis(1000L);

		router.merge(routerToMerge);

		assertEquals(1000L, router.getLastModifiedMillis());
	}

	@Test
	@SuppressWarnings("static-method")
	void mergeAddsUnsecuredUrlPrefixes() {
		Router router = new Router();
		router.getUnsecuredUrlPrefixes().add("/public/");

		Router routerToMerge = new Router();
		routerToMerge.getUnsecuredUrlPrefixes().add("/api/");

		router.merge(routerToMerge);

		assertTrue(router.getUnsecuredUrlPrefixes().contains("/public/"));
		assertTrue(router.getUnsecuredUrlPrefixes().contains("/api/"));
	}

	@Test
	@SuppressWarnings("static-method")
	void selectRouteWithMatchingCriteriaReturnsRoute() {
		Router router = new Router();

		UxUiMetadata uxui = new UxUiMetadata();
		uxui.setName("desktop");

		Route route = new Route();
		route.setOutcomeUrl("/desktop/admin");
		RouteCriteria rc = new RouteCriteria();
		rc.setModuleName("admin");
		route.getCriteria().add(rc);
		uxui.getRoutes().add(route);
		router.getUxUis().add(uxui);
		router.convert("test");

		RouteCriteria criteria = new RouteCriteria();
		criteria.setModuleName("admin");
		Route found = router.selectRoute("desktop", criteria);
		assertNotNull(found);
		assertThat(found.getOutcomeUrl(), is("/desktop/admin"));
	}

	@Test
	@SuppressWarnings("static-method")
	void selectOutcomeUrlReturnsRouteUrlWhenMatched() {
		Router router = new Router();

		UxUiMetadata uxui = new UxUiMetadata();
		uxui.setName("mobile");

		Route route = new Route();
		route.setOutcomeUrl("/mobile/home");
		uxui.getRoutes().add(route);
		router.getUxUis().add(uxui);
		router.convert("test");

		RouteCriteria criteria = new RouteCriteria();
		assertThat(router.selectOutcomeUrl("mobile", criteria), is("/mobile/home"));
	}

	@Test
	@SuppressWarnings("static-method")
	void mergeWithLowerLastModifiedKeepsHigher() {
		Router router = new Router();
		router.setLastModifiedMillis(2000L);

		Router routerToMerge = new Router();
		routerToMerge.setLastModifiedMillis(100L);

		router.merge(routerToMerge);

		assertEquals(2000L, router.getLastModifiedMillis());
	}

	@Test
	@SuppressWarnings("static-method")
	void getUxuiSelectorLoadsAndReturnsSelectorInstance() {
		Router router = new Router();
		router.setUxuiSelectorClassName(RouterTest.class.getName());
		TaggingUxUiSelector selector = router.getUxuiSelector();
		assertNotNull(selector);
		assertInstanceOf(RouterTest.class, selector);
	}

	@Test
	@SuppressWarnings("static-method")
	void getUxuiSelectorThrowsMetaDataExceptionForInvalidClassName() {
		Router router = new Router();
		router.setUxuiSelectorClassName("com.example.NonExistentClass");
		assertThrows(MetaDataException.class, router::getUxuiSelector);
	}
}
