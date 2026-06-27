package org.skyve.impl.metadata.repository.view.access;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

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
		assertTrue(ua.isDocumentAggregate());
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
		assertTrue(ua.isSingular());
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
		assertTrue(ua.isQueryAggregate());
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
		assertTrue(ua.isContent());
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

	// ---- ViewDynamicImageUserAccessMetaData ----

	@Test
	void dynamicImageGetSetImageName() {
		ViewDynamicImageUserAccessMetaData md = new ViewDynamicImageUserAccessMetaData();
		md.setImageName("profilePhoto");
		assertThat(md.getImageName(), is("profilePhoto"));
	}

	@Test
	void dynamicImageImageNameBlankBecomesNull() {
		ViewDynamicImageUserAccessMetaData md = new ViewDynamicImageUserAccessMetaData();
		md.setImageName("  ");
		assertThat(md.getImageName(), is(nullValue()));
	}

	@Test
	void dynamicImageToUserAccess() {
		ViewDynamicImageUserAccessMetaData md = new ViewDynamicImageUserAccessMetaData();
		md.setImageName("profilePhoto");
		UserAccess ua = md.toUserAccess("admin", "Contact");
		assertThat(ua, is(notNullValue()));
		assertTrue(ua.isDynamicImage());
	}

	@Test
	void dynamicImageToUserAccessComponent() {
		ViewDynamicImageUserAccessMetaData md = new ViewDynamicImageUserAccessMetaData();
		md.setImageName("profilePhoto");
		UserAccess ua = md.toUserAccess("admin", "Contact");
		assertThat(ua.getComponent(), is("profilePhoto"));
	}

	// ---- ViewModelAggregateUserAccessMetaData ----

	@Test
	void modelAggregateGetSetModelName() {
		ViewModelAggregateUserAccessMetaData md = new ViewModelAggregateUserAccessMetaData();
		md.setModelName("ContactList");
		assertThat(md.getModelName(), is("ContactList"));
	}

	@Test
	void modelAggregateModelNameBlankBecomesNull() {
		ViewModelAggregateUserAccessMetaData md = new ViewModelAggregateUserAccessMetaData();
		md.setModelName("  ");
		assertThat(md.getModelName(), is(nullValue()));
	}

	@Test
	void modelAggregateToUserAccess() {
		ViewModelAggregateUserAccessMetaData md = new ViewModelAggregateUserAccessMetaData();
		md.setModelName("ContactList");
		UserAccess ua = md.toUserAccess("admin", "Contact");
		assertThat(ua, is(notNullValue()));
		assertTrue(ua.isModelAggregate());
	}

	@Test
	void modelAggregateToUserAccessComponent() {
		ViewModelAggregateUserAccessMetaData md = new ViewModelAggregateUserAccessMetaData();
		md.setModelName("ContactList");
		UserAccess ua = md.toUserAccess("admin", "Contact");
		assertThat(ua.getComponent(), is("ContactList"));
	}

	// ---- ViewPreviousCompleteUserAccessMetaData ----

	@Test
	void previousCompleteGetSetBinding() {
		ViewPreviousCompleteUserAccessMetaData md = new ViewPreviousCompleteUserAccessMetaData();
		md.setBinding("approvalStep");
		assertThat(md.getBinding(), is("approvalStep"));
	}

	@Test
	void previousCompleteBindingBlankBecomesNull() {
		ViewPreviousCompleteUserAccessMetaData md = new ViewPreviousCompleteUserAccessMetaData();
		md.setBinding("  ");
		assertThat(md.getBinding(), is(nullValue()));
	}

	@Test
	void previousCompleteToUserAccess() {
		ViewPreviousCompleteUserAccessMetaData md = new ViewPreviousCompleteUserAccessMetaData();
		md.setBinding("approvalStep");
		UserAccess ua = md.toUserAccess("admin", "Contact");
		assertThat(ua, is(notNullValue()));
		assertTrue(ua.isPreviousComplete());
	}

	@Test
	void previousCompleteToUserAccessComponent() {
		ViewPreviousCompleteUserAccessMetaData md = new ViewPreviousCompleteUserAccessMetaData();
		md.setBinding("approvalStep");
		UserAccess ua = md.toUserAccess("admin", "Contact");
		assertThat(ua.getComponent(), is("approvalStep"));
	}

	// ---- ViewUserAccessesMetaData ----

	@Test
	void accessesListIsEmptyByDefault() {
		ViewUserAccessesMetaData md = new ViewUserAccessesMetaData();
		assertThat(md.getAccesses(), is(notNullValue()));
		assertTrue(md.getAccesses().isEmpty());
	}

	@Test
	void generateIsTrueByDefault() {
		ViewUserAccessesMetaData md = new ViewUserAccessesMetaData();
		assertTrue(md.isGenerate());
	}

	@Test
	void setGenerateFalseAndGet() {
		ViewUserAccessesMetaData md = new ViewUserAccessesMetaData();
		md.setGenerate(false);
		assertFalse(md.isGenerate());
	}
}
