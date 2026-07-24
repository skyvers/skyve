package org.skyve.impl.metadata.repository.router;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.router.Direct.DirectMatch;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.web.UserAgentType;
import org.skyve.web.WebAction;

@SuppressWarnings("static-method")
class RouterMetaDataTest {
	@Test
	void directAttributesNormalizeAndRoundTrip() {
		Direct direct = new Direct();
		direct.setPath("  /external/  ");
		direct.setUxui("  external  ");
		direct.setMatch(DirectMatch.prefix);
		direct.setUserAgentType(UserAgentType.phone);

		assertThat(direct.getPath(), is("/external/"));
		assertThat(direct.getUxui(), is("external"));
		assertThat(direct.getMatch(), is(DirectMatch.prefix));
		assertThat(direct.getUserAgentType(), is(UserAgentType.phone));
	}

	@Test
	void directDefaultsToExactWithWildcardConditions() {
		Direct direct = new Direct();

		assertThat(direct.getMatch(), is(DirectMatch.exact));
		assertThat(direct.getUserAgentType(), nullValue());
	}

	// Route

	@Test
	void routeOutcomeUrlRoundTrips() {
		Route r = new Route();
		r.setOutcomeUrl("/faces/skyve/edit.xhtml");
		assertThat(r.getOutcomeUrl(), is("/faces/skyve/edit.xhtml"));
	}

	@Test
	void routeOutcomeUrlBlankBecomesNull() {
		Route r = new Route();
		r.setOutcomeUrl("  ");
		assertThat(r.getOutcomeUrl(), nullValue());
	}

	@Test
	void routeCriteriaListNotNull() {
		Route r = new Route();
		assertThat(r.getCriteria(), notNullValue());
	}

	@Test
	void routeCanAddCriteria() {
		Route r = new Route();
		RouteCriteria rc = new RouteCriteria();
		rc.setModuleName("admin");
		r.getCriteria().add(rc);
		assertEquals(1, r.getCriteria().size());
	}

	@Test
	void routePropertiesNotNull() {
		Route r = new Route();
		assertThat(r.getProperties(), notNullValue());
	}

	@Test
	void routePropertiesCanBePopulated() {
		Route r = new Route();
		r.getProperties().put("theme", "dark");
		assertThat(r.getProperties().get("theme"), is("dark"));
	}

	// RouteCriteria - setters/getters

	@Test
	void routeCriteriaViewTypeRoundTrips() {
		RouteCriteria rc = new RouteCriteria();
		rc.setViewType(ViewType.edit);
		assertThat(rc.getViewType(), is(ViewType.edit));
	}

	@Test
	void routeCriteriaWebActionRoundTrips() {
		RouteCriteria rc = new RouteCriteria();
		rc.setWebAction(WebAction.e);
		assertThat(rc.getWebAction(), is(WebAction.e));
	}

	@Test
	void routeCriteriaModuleNameRoundTrips() {
		RouteCriteria rc = new RouteCriteria();
		rc.setModuleName("admin");
		assertThat(rc.getModuleName(), is("admin"));
	}

	@Test
	void routeCriteriaDocumentNameRoundTrips() {
		RouteCriteria rc = new RouteCriteria();
		rc.setDocumentName("Contact");
		assertThat(rc.getDocumentName(), is("Contact"));
	}

	@Test
	void routeCriteriaQueryNameRoundTrips() {
		RouteCriteria rc = new RouteCriteria();
		rc.setQueryName("qContacts");
		assertThat(rc.getQueryName(), is("qContacts"));
	}

	@Test
	void routeCriteriaCustomerNameRoundTrips() {
		RouteCriteria rc = new RouteCriteria();
		rc.setCustomerName("skyve");
		assertThat(rc.getCustomerName(), is("skyve"));
	}

	@Test
	void routeCriteriaDataGroupIdRoundTrips() {
		RouteCriteria rc = new RouteCriteria();
		rc.setDataGroupId("group1");
		assertThat(rc.getDataGroupId(), is("group1"));
	}

	@Test
	void routeCriteriaUserIdRoundTrips() {
		RouteCriteria rc = new RouteCriteria();
		rc.setUserId("user1");
		assertThat(rc.getUserId(), is("user1"));
	}

	@Test
	void routeCriteriaBlankModuleNameBecomesNull() {
		RouteCriteria rc = new RouteCriteria();
		rc.setModuleName("  ");
		assertThat(rc.getModuleName(), nullValue());
	}

	@Test
	void routeCriteriaDefaultsAreNull() {
		RouteCriteria rc = new RouteCriteria();
		assertThat(rc.getViewType(), nullValue());
		assertThat(rc.getWebAction(), nullValue());
		assertThat(rc.getModuleName(), nullValue());
		assertThat(rc.getDocumentName(), nullValue());
	}

	// RouteCriteria.matches()

	@Test
	void routeCriteriaMatchesWhenAllNullMatchesEverything() {
		RouteCriteria template = new RouteCriteria();
		RouteCriteria candidate = new RouteCriteria();
		candidate.setModuleName("admin");
		candidate.setDocumentName("Contact");
		assertTrue(template.matches(candidate));
	}

	@Test
	void routeCriteriaMatchesWhenModuleMatches() {
		RouteCriteria template = new RouteCriteria();
		template.setModuleName("admin");
		RouteCriteria candidate = new RouteCriteria();
		candidate.setModuleName("admin");
		assertTrue(template.matches(candidate));
	}

	@Test
	void routeCriteriaDoesNotMatchWhenModuleDiffers() {
		RouteCriteria template = new RouteCriteria();
		template.setModuleName("admin");
		RouteCriteria candidate = new RouteCriteria();
		candidate.setModuleName("contacts");
		assertFalse(template.matches(candidate));
	}

	@Test
	void routeCriteriaMatchesWhenDocumentMatches() {
		RouteCriteria template = new RouteCriteria();
		template.setDocumentName("Contact");
		RouteCriteria candidate = new RouteCriteria();
		candidate.setDocumentName("Contact");
		assertTrue(template.matches(candidate));
	}

	@Test
	void routeCriteriaDoesNotMatchWhenDocumentDiffers() {
		RouteCriteria template = new RouteCriteria();
		template.setDocumentName("Contact");
		RouteCriteria candidate = new RouteCriteria();
		candidate.setDocumentName("Staff");
		assertFalse(template.matches(candidate));
	}

	@Test
	void routeCriteriaMatchesWhenViewTypeMatches() {
		RouteCriteria template = new RouteCriteria();
		template.setViewType(ViewType.edit);
		RouteCriteria candidate = new RouteCriteria();
		candidate.setViewType(ViewType.edit);
		assertTrue(template.matches(candidate));
	}

	@Test
	void routeCriteriaDoesNotMatchWhenViewTypeDiffers() {
		RouteCriteria template = new RouteCriteria();
		template.setViewType(ViewType.edit);
		RouteCriteria candidate = new RouteCriteria();
		candidate.setViewType(ViewType.list);
		assertFalse(template.matches(candidate));
	}

	@Test
	void routeCriteriaMatchesWhenWebActionMatches() {
		RouteCriteria template = new RouteCriteria();
		template.setWebAction(WebAction.e);
		RouteCriteria candidate = new RouteCriteria();
		candidate.setWebAction(WebAction.e);
		assertTrue(template.matches(candidate));
	}

	@Test
	void routeCriteriaDoesNotMatchWhenWebActionDiffers() {
		RouteCriteria template = new RouteCriteria();
		template.setWebAction(WebAction.e);
		RouteCriteria candidate = new RouteCriteria();
		candidate.setWebAction(WebAction.l);
		assertFalse(template.matches(candidate));
	}

	@Test
	void routeCriteriaMatchesWhenCustomerMatches() {
		RouteCriteria template = new RouteCriteria();
		template.setCustomerName("skyve");
		RouteCriteria candidate = new RouteCriteria();
		candidate.setCustomerName("skyve");
		assertTrue(template.matches(candidate));
	}

	@Test
	void routeCriteriaDoesNotMatchWhenCustomerDiffers() {
		RouteCriteria template = new RouteCriteria();
		template.setCustomerName("skyve");
		RouteCriteria candidate = new RouteCriteria();
		candidate.setCustomerName("other");
		assertFalse(template.matches(candidate));
	}

	@Test
	void routeCriteriaMatchesWhenQueryMatches() {
		RouteCriteria template = new RouteCriteria();
		template.setQueryName("qAll");
		RouteCriteria candidate = new RouteCriteria();
		candidate.setQueryName("qAll");
		assertTrue(template.matches(candidate));
	}

	@Test
	void routeCriteriaDoesNotMatchWhenQueryDiffers() {
		RouteCriteria template = new RouteCriteria();
		template.setQueryName("qAll");
		RouteCriteria candidate = new RouteCriteria();
		candidate.setQueryName("qFiltered");
		assertFalse(template.matches(candidate));
	}

	// RouteCriteria.toString()

	@Test
	void routeCriteriaToStringEmptyBraces() {
		RouteCriteria rc = new RouteCriteria();
		assertThat(rc.toString(), is("{}"));
	}

	@Test
	void routeCriteriaToStringIncludesModuleName() {
		RouteCriteria rc = new RouteCriteria();
		rc.setModuleName("admin");
		String s = rc.toString();
		assertTrue(s.contains("moduleName=admin"));
	}

	@Test
	void routeCriteriaToStringIncludesDocumentName() {
		RouteCriteria rc = new RouteCriteria();
		rc.setDocumentName("Contact");
		String s = rc.toString();
		assertTrue(s.contains("documentName=Contact"));
	}

	@Test
	void routeCriteriaToStringIncludesWebAction() {
		RouteCriteria rc = new RouteCriteria();
		rc.setWebAction(WebAction.e);
		String s = rc.toString();
		assertTrue(s.contains("webAction=e"));
	}

	// UxUiMetadata

	@Test
	void uxuiMetadataNameRoundTrips() {
		UxUiMetadata u = new UxUiMetadata();
		u.setName("desktop");
		assertThat(u.getName(), is("desktop"));
	}

	@Test
	void uxuiMetadataRoutesNotNull() {
		UxUiMetadata u = new UxUiMetadata();
		assertThat(u.getRoutes(), notNullValue());
	}

	@Test
	void uxuiMetadataCanAddRoute() {
		UxUiMetadata u = new UxUiMetadata();
		Route r = new Route();
		r.setOutcomeUrl("/faces/skyve/edit.xhtml");
		u.getRoutes().add(r);
		assertEquals(1, u.getRoutes().size());
		assertThat(u.getRoutes().get(0).getOutcomeUrl(), is("/faces/skyve/edit.xhtml"));
	}

	@Test
	void uxuiMetadataPropertiesNotNull() {
		UxUiMetadata u = new UxUiMetadata();
		assertThat(u.getProperties(), notNullValue());
	}
}
