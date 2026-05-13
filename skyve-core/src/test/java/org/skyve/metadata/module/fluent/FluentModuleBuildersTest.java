package org.skyve.metadata.module.fluent;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.container.form.FormLabelLayout;
import org.skyve.metadata.view.View.ViewType;

/** Unit tests for fluent module builder classes. */
@SuppressWarnings("static-method")
class FluentModuleBuildersTest {

	// ---- FluentModule ----

	@Test
	void moduleDefaultConstructorCreatesInstance() {
		FluentModule m = new FluentModule();
		assertThat(m.get(), is(notNullValue()));
	}

	@Test
	void moduleNameSetsValue() {
		FluentModule m = new FluentModule();
		m.name("admin");
		assertThat(m.get().getName(), is("admin"));
	}

	@Test
	void moduleTitleSetsValue() {
		FluentModule m = new FluentModule();
		m.title("Administration");
		assertThat(m.get().getTitle(), is("Administration"));
	}

	@Test
	void moduleDocumentationSetsValue() {
		FluentModule m = new FluentModule();
		m.documentation("Module docs");
		assertThat(m.get().getDocumentation(), is("Module docs"));
	}

	@Test
	void modulePrototypeSetsTrue() {
		FluentModule m = new FluentModule();
		m.prototype(true);
		assertThat(m.get().getPrototype(), is(Boolean.TRUE));
	}

	@Test
	void modulePrototypeSetsFalse() {
		FluentModule m = new FluentModule();
		m.prototype(false);
		assertThat(m.get().getPrototype(), is(Boolean.FALSE));
	}

	@Test
	void moduleFormLabelLayoutSetsValue() {
		FluentModule m = new FluentModule();
		m.formLabelLayout(FormLabelLayout.top);
		assertThat(m.get().getFormLabelLayout(), is(FormLabelLayout.top));
	}

	@Test
	void moduleHomeRefSetsValue() {
		FluentModule m = new FluentModule();
		m.homeRef(ViewType.edit);
		assertThat(m.get().getHomeRef(), is(ViewType.edit));
	}

	@Test
	void moduleHomeDocumentSetsValue() {
		FluentModule m = new FluentModule();
		m.homeDocument("Contact");
		assertThat(m.get().getHomeDocument(), is("Contact"));
	}

	@Test
	void moduleAddDocumentAddsDocument() {
		FluentModule m = new FluentModule();
		m.addDocument(new FluentModuleDocument().ref("Contact"));
		assertThat(m.get().getDocuments().size(), is(1));
	}

	@Test
	void moduleAddJobAddsJob() {
		FluentModule m = new FluentModule();
		m.addJob(new FluentJob().name("SendEmail").className("modules.admin.SendEmailJob"));
		assertThat(m.get().getJobs().size(), is(1));
	}

	@Test
	void moduleAddRoleAddsRole() {
		FluentModule m = new FluentModule();
		m.addRole(new FluentModuleRole().name("BasicUser"));
		assertThat(m.get().getRoles().size(), is(1));
	}

	// ---- FluentModuleDocument ----

	@Test
	void moduleDocumentDefaultConstructorCreatesInstance() {
		FluentModuleDocument d = new FluentModuleDocument();
		assertThat(d.get(), is(notNullValue()));
	}

	@Test
	void moduleDocumentRefSetsValue() {
		FluentModuleDocument d = new FluentModuleDocument();
		d.ref("Contact");
		assertThat(d.get().getRef(), is("Contact"));
	}

	@Test
	void moduleDocumentDefaultQueryNameSetsValue() {
		FluentModuleDocument d = new FluentModuleDocument();
		d.defaultQueryName("qContacts");
		assertThat(d.get().getDefaultQueryName(), is("qContacts"));
	}

	// ---- FluentJob ----

	@Test
	void jobDefaultConstructorCreatesInstance() {
		FluentJob j = new FluentJob();
		assertThat(j.get(), is(notNullValue()));
	}

	@Test
	void jobNameSetsValue() {
		FluentJob j = new FluentJob();
		j.name("SendEmail");
		assertThat(j.get().getName(), is("SendEmail"));
	}

	@Test
	void jobClassNameSetsValue() {
		FluentJob j = new FluentJob();
		j.className("modules.admin.SendEmailJob");
		assertThat(j.get().getClassName(), is("modules.admin.SendEmailJob"));
	}

	@Test
	void jobDisplayNameSetsValue() {
		FluentJob j = new FluentJob();
		j.displayName("Send Email Job");
		assertThat(j.get().getDisplayName(), is("Send Email Job"));
	}

	// ---- FluentMetaDataQuery ----

	@Test
	void metaDataQueryDefaultConstructorCreatesInstance() {
		FluentMetaDataQuery q = new FluentMetaDataQuery();
		assertThat(q.get(), is(notNullValue()));
	}

	@Test
	void metaDataQueryNameSetsValue() {
		FluentMetaDataQuery q = new FluentMetaDataQuery();
		q.name("qContacts");
		assertThat(q.get().getName(), is("qContacts"));
	}

	@Test
	void metaDataQueryDocumentNameSetsValue() {
		FluentMetaDataQuery q = new FluentMetaDataQuery();
		q.documentName("Contact");
		assertThat(q.get().getDocumentName(), is("Contact"));
	}

	@Test
	void metaDataQueryPolymorphicSetsTrue() {
		FluentMetaDataQuery q = new FluentMetaDataQuery();
		q.polymorphic(true);
		assertThat(q.get().getPolymorphic(), is(Boolean.TRUE));
	}

	@Test
	void metaDataQueryAggregateSetsTrue() {
		FluentMetaDataQuery q = new FluentMetaDataQuery();
		q.aggregate(true);
		assertThat(q.get().getAggregate(), is(Boolean.TRUE));
	}

	@Test
	void metaDataQueryFromSetsValue() {
		FluentMetaDataQuery q = new FluentMetaDataQuery();
		q.from("Contact c");
		assertThat(q.get().getFrom(), is("Contact c"));
	}

	@Test
	void metaDataQueryFilterSetsValue() {
		FluentMetaDataQuery q = new FluentMetaDataQuery();
		q.filter("c.active = true");
		assertThat(q.get().getFilter(), is("c.active = true"));
	}

	@Test
	void metaDataQueryGroupingSetsValue() {
		FluentMetaDataQuery q = new FluentMetaDataQuery();
		q.grouping("c.name");
		assertThat(q.get().getGrouping(), is("c.name"));
	}

	@Test
	void metaDataQueryOrderingSetsValue() {
		FluentMetaDataQuery q = new FluentMetaDataQuery();
		q.ordering("c.name asc");
		assertThat(q.get().getOrdering(), is("c.name asc"));
	}

	@Test
	void metaDataQueryAddProjectedColumnAddsColumn() {
		FluentMetaDataQuery q = new FluentMetaDataQuery();
		q.addProjectedColumn(new FluentMetaDataQueryProjectedColumn().binding("name"));
		assertThat(q.get().getColumns().size(), is(1));
	}

	// ---- FluentMetaDataQueryProjectedColumn ----

	@Test
	void metaDataQueryProjectedColumnDefaultConstructorCreatesInstance() {
		FluentMetaDataQueryProjectedColumn col = new FluentMetaDataQueryProjectedColumn();
		assertThat(col.get(), is(notNullValue()));
	}

	@Test
	void metaDataQueryProjectedColumnBindingSetsValue() {
		FluentMetaDataQueryProjectedColumn col = new FluentMetaDataQueryProjectedColumn();
		col.binding("name");
		assertThat(col.get().getBinding(), is("name"));
	}

	@Test
	void metaDataQueryProjectedColumnDisplayNameSetsValue() {
		FluentMetaDataQueryProjectedColumn col = new FluentMetaDataQueryProjectedColumn();
		col.displayName("Name");
		assertThat(col.get().getDisplayName(), is("Name"));
	}

	@Test
	void metaDataQueryProjectedColumnProjectedSetsFalse() {
		FluentMetaDataQueryProjectedColumn col = new FluentMetaDataQueryProjectedColumn();
		col.projected(false);
		org.skyve.impl.metadata.repository.module.MetaDataQueryProjectedColumnMetaData projected =
			(org.skyve.impl.metadata.repository.module.MetaDataQueryProjectedColumnMetaData) col.get();
		assertThat(projected.getProjected(), is(Boolean.FALSE));
	}

	@Test
	void metaDataQueryProjectedColumnSortableSetsTrue() {
		FluentMetaDataQueryProjectedColumn col = new FluentMetaDataQueryProjectedColumn();
		col.sortable(true);
		org.skyve.impl.metadata.repository.module.MetaDataQueryProjectedColumnMetaData projected =
			(org.skyve.impl.metadata.repository.module.MetaDataQueryProjectedColumnMetaData) col.get();
		assertThat(projected.getSortable(), is(Boolean.TRUE));
	}

	@Test
	void metaDataQueryProjectedColumnFilterableSetsTrue() {
		FluentMetaDataQueryProjectedColumn col = new FluentMetaDataQueryProjectedColumn();
		col.filterable(true);
		org.skyve.impl.metadata.repository.module.MetaDataQueryProjectedColumnMetaData projected =
			(org.skyve.impl.metadata.repository.module.MetaDataQueryProjectedColumnMetaData) col.get();
		assertThat(projected.getFilterable(), is(Boolean.TRUE));
	}

	// ---- FluentMenu / FluentMenuGroup ----

	@Test
	void menuDefaultConstructorCreatesInstance() {
		FluentMenu menu = new FluentMenu();
		assertThat(menu.get(), is(notNullValue()));
	}

	@Test
	void menuGroupDefaultConstructorCreatesInstance() {
		FluentMenuGroup g = new FluentMenuGroup();
		assertThat(g.get(), is(notNullValue()));
	}

	@Test
	void menuGroupNameSetsValue() {
		FluentMenuGroup g = new FluentMenuGroup();
		g.name("AdminGroup");
		assertThat(g.get().getName(), is("AdminGroup"));
	}

	// ---- FluentEditItem ----

	@Test
	void editItemDefaultConstructorCreatesInstance() {
		FluentEditItem ei = new FluentEditItem();
		assertThat(ei.get(), is(notNullValue()));
	}

	@Test
	void editItemNameSetsValue() {
		FluentEditItem ei = new FluentEditItem();
		ei.name("EditContact");
		assertThat(ei.get().getName(), is("EditContact"));
	}

	@Test
	void editItemDocumentNameSetsValue() {
		FluentEditItem ei = new FluentEditItem();
		ei.documentName("Contact");
		assertThat(ei.get().getDocumentName(), is("Contact"));
	}

	// ---- FluentListItem ----

	@Test
	void listItemDefaultConstructorCreatesInstance() {
		FluentListItem li = new FluentListItem();
		assertThat(li.get(), is(notNullValue()));
	}

	@Test
	void listItemNameSetsValue() {
		FluentListItem li = new FluentListItem();
		li.name("Contacts");
		assertThat(li.get().getName(), is("Contacts"));
	}

	@Test
	void listItemQueryNameSetsValue() {
		FluentListItem li = new FluentListItem();
		li.queryName("qContacts");
		assertThat(li.get().getQueryName(), is("qContacts"));
	}

	// ---- FluentModuleRole ----

	@Test
	void moduleRoleDefaultConstructorCreatesInstance() {
		FluentModuleRole r = new FluentModuleRole();
		assertThat(r.get(), is(notNullValue()));
	}

	@Test
	void moduleRoleNameSetsValue() {
		FluentModuleRole r = new FluentModuleRole();
		r.name("BasicUser");
		assertThat(r.get().getName(), is("BasicUser"));
	}

	@Test
	void moduleRoleDocumentationSetsValue() {
		FluentModuleRole r = new FluentModuleRole();
		r.documentation("Role docs");
		assertThat(r.get().getDocumentation(), is("Role docs"));
	}

	@Test
	void moduleRoleDescriptionSetsValue() {
		FluentModuleRole r = new FluentModuleRole();
		r.description("Basic user role");
		assertThat(r.get().getDescription(), is("Basic user role"));
	}
}
