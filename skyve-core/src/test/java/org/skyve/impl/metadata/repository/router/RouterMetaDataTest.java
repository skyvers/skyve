package org.skyve.impl.metadata.repository.router;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.web.WebAction;

@SuppressWarnings("static-method")
class RouterMetaDataTest {

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
		assertThat(r.getCriteria().size(), is(1));
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
		assertThat(template.matches(candidate), is(true));
	}

	@Test
	void routeCriteriaMatchesWhenModuleMatches() {
		RouteCriteria template = new RouteCriteria();
		template.setModuleName("admin");
		RouteCriteria candidate = new RouteCriteria();
		candidate.setModuleName("admin");
		assertThat(template.matches(candidate), is(true));
	}

	@Test
	void routeCriteriaDoesNotMatchWhenModuleDiffers() {
		RouteCriteria template = new RouteCriteria();
		template.setModuleName("admin");
		RouteCriteria candidate = new RouteCriteria();
		candidate.setModuleName("contacts");
		assertThat(template.matches(candidate), is(false));
	}

	@Test
	void routeCriteriaMatchesWhenDocumentMatches() {
		RouteCriteria template = new RouteCriteria();
		template.setDocumentName("Contact");
		RouteCriteria candidate = new RouteCriteria();
		candidate.setDocumentName("Contact");
		assertThat(template.matches(candidate), is(true));
	}

	@Test
	void routeCriteriaDoesNotMatchWhenDocumentDiffers() {
		RouteCriteria template = new RouteCriteria();
		template.setDocumentName("Contact");
		RouteCriteria candidate = new RouteCriteria();
		candidate.setDocumentName("Staff");
		assertThat(template.matches(candidate), is(false));
	}

	@Test
	void routeCriteriaMatchesWhenViewTypeMatches() {
		RouteCriteria template = new RouteCriteria();
		template.setViewType(ViewType.edit);
		RouteCriteria candidate = new RouteCriteria();
		candidate.setViewType(ViewType.edit);
		assertThat(template.matches(candidate), is(true));
	}

	@Test
	void routeCriteriaDoesNotMatchWhenViewTypeDiffers() {
		RouteCriteria template = new RouteCriteria();
		template.setViewType(ViewType.edit);
		RouteCriteria candidate = new RouteCriteria();
		candidate.setViewType(ViewType.list);
		assertThat(template.matches(candidate), is(false));
	}

	@Test
	void routeCriteriaMatchesWhenWebActionMatches() {
		RouteCriteria template = new RouteCriteria();
		template.setWebAction(WebAction.e);
		RouteCriteria candidate = new RouteCriteria();
		candidate.setWebAction(WebAction.e);
		assertThat(template.matches(candidate), is(true));
	}

	@Test
	void routeCriteriaDoesNotMatchWhenWebActionDiffers() {
		RouteCriteria template = new RouteCriteria();
		template.setWebAction(WebAction.e);
		RouteCriteria candidate = new RouteCriteria();
		candidate.setWebAction(WebAction.l);
		assertThat(template.matches(candidate), is(false));
	}

	@Test
	void routeCriteriaMatchesWhenCustomerMatches() {
		RouteCriteria template = new RouteCriteria();
		template.setCustomerName("skyve");
		RouteCriteria candidate = new RouteCriteria();
		candidate.setCustomerName("skyve");
		assertThat(template.matches(candidate), is(true));
	}

	@Test
	void routeCriteriaDoesNotMatchWhenCustomerDiffers() {
		RouteCriteria template = new RouteCriteria();
		template.setCustomerName("skyve");
		RouteCriteria candidate = new RouteCriteria();
		candidate.setCustomerName("other");
		assertThat(template.matches(candidate), is(false));
	}

	@Test
	void routeCriteriaMatchesWhenQueryMatches() {
		RouteCriteria template = new RouteCriteria();
		template.setQueryName("qAll");
		RouteCriteria candidate = new RouteCriteria();
		candidate.setQueryName("qAll");
		assertThat(template.matches(candidate), is(true));
	}

	@Test
	void routeCriteriaDoesNotMatchWhenQueryDiffers() {
		RouteCriteria template = new RouteCriteria();
		template.setQueryName("qAll");
		RouteCriteria candidate = new RouteCriteria();
		candidate.setQueryName("qFiltered");
		assertThat(template.matches(candidate), is(false));
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
		assertThat(s.contains("moduleName=admin"), is(true));
	}

	@Test
	void routeCriteriaToStringIncludesDocumentName() {
		RouteCriteria rc = new RouteCriteria();
		rc.setDocumentName("Contact");
		String s = rc.toString();
		assertThat(s.contains("documentName=Contact"), is(true));
	}

	@Test
	void routeCriteriaToStringIncludesWebAction() {
		RouteCriteria rc = new RouteCriteria();
		rc.setWebAction(WebAction.e);
		String s = rc.toString();
		assertThat(s.contains("webAction=e"), is(true));
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
		assertThat(u.getRoutes().size(), is(1));
		assertThat(u.getRoutes().get(0).getOutcomeUrl(), is("/faces/skyve/edit.xhtml"));
	}

	@Test
	void uxuiMetadataPropertiesNotNull() {
		UxUiMetadata u = new UxUiMetadata();
		assertThat(u.getProperties(), notNullValue());
	}
}
