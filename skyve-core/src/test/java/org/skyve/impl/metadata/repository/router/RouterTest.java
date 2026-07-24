package org.skyve.impl.metadata.repository.router;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.router.Direct.DirectMatch;
import org.skyve.metadata.MetaDataException;
import org.skyve.web.UserAgentType;

class RouterTest implements TaggingUxUiSelector {
	private static final long serialVersionUID = -534783648720934893L;

	@Test
	@SuppressWarnings("static-method")
	void convertNormalizesDirectAndDoesNotValidateEffectiveTarget() {
		Router router = new Router();
		Direct direct = direct("  /public/register.xhtml  ", "  supplied-after-assembly  ");
		router.getDirects().add(direct);

		router.convert("test router");

		assertEquals("/public/register.xhtml", direct.getPath());
		assertEquals("supplied-after-assembly", direct.getUxui());
	}

	@Test
	@SuppressWarnings("static-method")
	void convertRejectsInvalidDirectDeclarations() {
		assertInvalid(direct(null, "desktop"), "path is required");
		assertInvalid(direct("external/home.xhtml", "desktop"), "begin with '/'");
		assertInvalid(direct("/external/home.xhtml?mode=1", "desktop"), "query or fragment");
		assertInvalid(direct("/external/home.xhtml#content", "desktop"), "query or fragment");

		Direct prefix = direct("/external", "desktop");
		prefix.setMatch(DirectMatch.prefix);
		assertInvalid(prefix, "must end with '/'");
		assertInvalid(direct("/external/home.xhtml", "  "), "target is required");
	}

	@Test
	@SuppressWarnings("static-method")
	void selectDirectUsesDefaultExactAndExplicitPrefixMatching() {
		Router router = new Router();
		router.getDirects().add(direct("/external/startup.xhtml", "startup"));
		Direct prefix = direct("/external/", "external");
		prefix.setMatch(DirectMatch.prefix);
		router.getDirects().add(prefix);
		router.convert("test");

		assertEquals("startup", router.selectDirect("/external/startup.xhtml", UserAgentType.desktop));
		assertEquals("external", router.selectDirect("/external/home.xhtml", UserAgentType.desktop));
		assertNull(router.selectDirect("/other.xhtml", UserAgentType.desktop));
	}

	@Test
	@SuppressWarnings("static-method")
	void selectDirectUsesFirstDeclarationAcrossDuplicatesAndOverlaps() {
		Router router = new Router();
		Direct prefix = direct("/external/", "broad-first");
		prefix.setMatch(DirectMatch.prefix);
		router.getDirects().add(prefix);
		router.getDirects().add(direct("/external/startup.xhtml", "exact-second"));
		router.getDirects().add(direct("/external/startup.xhtml", "duplicate-third"));
		router.convert("test");

		assertEquals("broad-first", router.selectDirect("/external/startup.xhtml", UserAgentType.desktop));
	}

	@Test
	@SuppressWarnings("static-method")
	void selectDirectAppliesOrderedUserAgentConditionsWithUnconditionalFallback() {
		Router router = new Router();
		Direct phone = direct("/register.xhtml", "phone");
		phone.setUserAgentType(UserAgentType.phone);
		router.getDirects().add(phone);
		router.getDirects().add(direct("/register.xhtml", "fallback"));
		router.convert("test");

		assertEquals("phone", router.selectDirect("/register.xhtml", UserAgentType.phone));
		assertEquals("fallback", router.selectDirect("/register.xhtml", UserAgentType.tablet));
	}

	@Test
	@SuppressWarnings("static-method")
	void validateDirectTargetsRunsOnlyAfterCompleteAssembly() {
		Router global = routerWithUxUi("global");
		Router declaringModule = new Router();
		declaringModule.getDirects().add(direct("/global.xhtml", "global"));
		declaringModule.convert("declaring module");

		global.merge(declaringModule);

		global.validateDirectTargets();
	}

	@Test
	@SuppressWarnings("static-method")
	void validateDirectTargetsAcceptsSameAndOtherModuleTargetsRegardlessOfMergeOrder() {
		Router effective = routerWithUxUi("global");
		Router firstModule = routerWithUxUi("first");
		firstModule.getDirects().add(direct("/same.xhtml", "first"));
		firstModule.getDirects().add(direct("/later.xhtml", "later"));
		firstModule.convert("first module");
		Router laterModule = routerWithUxUi("later");
		laterModule.getDirects().add(direct("/earlier.xhtml", "first"));
		laterModule.convert("later module");

		effective.merge(firstModule);
		effective.merge(laterModule);

		effective.validateDirectTargets();
	}

	@Test
	@SuppressWarnings("static-method")
	void validateDirectTargetsRejectsUnknownEffectiveTarget() {
		Router router = routerWithUxUi("desktop");
		router.getDirects().add(direct("/missing.xhtml", "missing"));

		MetaDataException exception = assertThrows(MetaDataException.class,
				() -> router.validateDirectTargets());

		assertTrue(exception.getMessage().contains("unknown UX/UI missing"));
	}

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

	private static Direct direct(String path, String uxui) {
		Direct result = new Direct();
		result.setPath(path);
		result.setUxui(uxui);
		return result;
	}

	private static Router routerWithUxUi(String name) {
		Router result = new Router();
		UxUiMetadata uxui = new UxUiMetadata();
		uxui.setName(name);
		result.getUxUis().add(uxui);
		return result.convert(name + " router");
	}

	private static void assertInvalid(Direct direct, String expectedMessage) {
		Router router = new Router();
		router.getDirects().add(direct);
		MetaDataException exception = assertThrows(MetaDataException.class, () -> router.convert("test router"));
		assertTrue(exception.getMessage().contains(expectedMessage));
	}
}
