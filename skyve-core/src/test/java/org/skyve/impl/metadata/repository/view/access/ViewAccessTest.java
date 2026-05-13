package org.skyve.impl.metadata.repository.view.access;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.user.UserAccess;

@SuppressWarnings("static-method")
class ViewAccessTest {

	// ---- ViewDocumentAggregateUserAccessMetaData ----

	@Test
	void documentAggregateGetSetDocumentName() {
		ViewDocumentAggregateUserAccessMetaData md = new ViewDocumentAggregateUserAccessMetaData();
		md.setDocumentName("Contact");
		assertThat(md.getDocumentName(), is("Contact"));
	}

	@Test
	void documentAggregateDocumentNameNullByDefault() {
		ViewDocumentAggregateUserAccessMetaData md = new ViewDocumentAggregateUserAccessMetaData();
		assertThat(md.getDocumentName(), is(nullValue()));
	}

	@Test
	void documentAggregateToUserAccess() {
		ViewDocumentAggregateUserAccessMetaData md = new ViewDocumentAggregateUserAccessMetaData();
		md.setDocumentName("Contact");
		UserAccess ua = md.toUserAccess("Admin", "unused");
		assertThat(ua, is(notNullValue()));
		assertThat(ua.isDocumentAggregate(), is(true));
	}

	@Test
	void documentAggregateToUserAccessModuleName() {
		ViewDocumentAggregateUserAccessMetaData md = new ViewDocumentAggregateUserAccessMetaData();
		md.setDocumentName("Contact");
		UserAccess ua = md.toUserAccess("Admin", "unused");
		assertThat(ua.getModuleName(), is("Admin"));
	}

	@Test
	void documentAggregateUxuisNotNull() {
		ViewDocumentAggregateUserAccessMetaData md = new ViewDocumentAggregateUserAccessMetaData();
		assertThat(md.getUxuis(), is(notNullValue()));
	}

	// ---- ViewSingularUserAccessMetaData ----

	@Test
	void singularToUserAccess() {
		ViewSingularUserAccessMetaData md = new ViewSingularUserAccessMetaData();
		md.setDocumentName("Contact");
		UserAccess ua = md.toUserAccess("Admin", "unused");
		assertThat(ua, is(notNullValue()));
		assertThat(ua.isSingular(), is(true));
	}

	@Test
	void singularToUserAccessDocumentName() {
		ViewSingularUserAccessMetaData md = new ViewSingularUserAccessMetaData();
		md.setDocumentName("Contact");
		UserAccess ua = md.toUserAccess("Admin", "unused");
		assertThat(ua.getDocumentName(), is("Contact"));
	}

	// ---- ViewQueryAggregateUserAccessMetaData ----

	@Test
	void queryAggregateGetSetQueryName() {
		ViewQueryAggregateUserAccessMetaData md = new ViewQueryAggregateUserAccessMetaData();
		md.setQueryName("qryContacts");
		assertThat(md.getQueryName(), is("qryContacts"));
	}

	@Test
	void queryAggregateQueryNameNullByDefault() {
		ViewQueryAggregateUserAccessMetaData md = new ViewQueryAggregateUserAccessMetaData();
		assertThat(md.getQueryName(), is(nullValue()));
	}

	@Test
	void queryAggregateToUserAccess() {
		ViewQueryAggregateUserAccessMetaData md = new ViewQueryAggregateUserAccessMetaData();
		md.setQueryName("qryContacts");
		UserAccess ua = md.toUserAccess("Admin", null);
		assertThat(ua, is(notNullValue()));
		assertThat(ua.isQueryAggregate(), is(true));
	}

	@Test
	void queryAggregateToUserAccessComponent() {
		ViewQueryAggregateUserAccessMetaData md = new ViewQueryAggregateUserAccessMetaData();
		md.setQueryName("myQuery");
		UserAccess ua = md.toUserAccess("Admin", null);
		assertThat(ua.getComponent(), is("myQuery"));
	}

	// ---- ViewContentUserAccessMetaData ----

	@Test
	void contentGetSetBinding() {
		ViewContentUserAccessMetaData md = new ViewContentUserAccessMetaData();
		md.setBinding("attachment");
		assertThat(md.getBinding(), is("attachment"));
	}

	@Test
	void contentBindingNullByDefault() {
		ViewContentUserAccessMetaData md = new ViewContentUserAccessMetaData();
		assertThat(md.getBinding(), is(nullValue()));
	}

	@Test
	void contentToUserAccess() {
		ViewContentUserAccessMetaData md = new ViewContentUserAccessMetaData();
		md.setBinding("attachment");
		UserAccess ua = md.toUserAccess("Admin", "Contact");
		assertThat(ua, is(notNullValue()));
		assertThat(ua.isContent(), is(true));
	}

	@Test
	void contentToUserAccessComponent() {
		ViewContentUserAccessMetaData md = new ViewContentUserAccessMetaData();
		md.setBinding("resume");
		UserAccess ua = md.toUserAccess("Admin", "Contact");
		assertThat(ua.getComponent(), is("resume"));
	}

	// ---- ViewUserAccessUxUiMetadata ----

	@Test
	void uxuiMetadataGetSetName() {
		ViewUserAccessUxUiMetadata md = new ViewUserAccessUxUiMetadata();
		md.setName("desktop");
		assertThat(md.getName(), is("desktop"));
	}
}
