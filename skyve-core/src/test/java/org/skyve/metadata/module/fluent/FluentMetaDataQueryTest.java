package org.skyve.metadata.module.fluent;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.module.query.MetaDataQueryContentColumnImpl;
import org.skyve.impl.metadata.module.query.MetaDataQueryDefinitionImpl;
import org.skyve.impl.metadata.module.query.MetaDataQueryProjectedColumnImpl;
import org.skyve.impl.metadata.repository.module.MetaDataQueryMetaData;

/**
 * Tests for {@link FluentMetaDataQuery} setters, column management, and round-trip.
 */
@SuppressWarnings("static-method")
class FluentMetaDataQueryTest {

	@Test
	void defaultConstructorCreatesInstance() {
		assertThat(new FluentMetaDataQuery().get(), is(notNullValue()));
	}

	@Test
	void wrappingConstructorPreservesInstance() {
		MetaDataQueryMetaData q = new MetaDataQueryMetaData();
		assertThat(new FluentMetaDataQuery(q).get(), is(q));
	}

	// ---- property setters ----

	@Test
	void nameSetsValue() {
		assertThat(new FluentMetaDataQuery().name("q1").get().getName(), is("q1"));
	}

	@Test
	void descriptionSetsValue() {
		assertThat(new FluentMetaDataQuery().description("All contacts").get().getDescription(), is("All contacts"));
	}

	@Test
	void documentNameSetsValue() {
		assertThat(new FluentMetaDataQuery().documentName("Contact").get().getDocumentName(), is("Contact"));
	}

	@Test
	void polymorphicSetsTrue() {
		assertThat(new FluentMetaDataQuery().polymorphic(true).get().getPolymorphic(), is(Boolean.TRUE));
	}

	@Test
	void polymorphicSetsFalse() {
		assertThat(new FluentMetaDataQuery().polymorphic(false).get().getPolymorphic(), is(Boolean.FALSE));
	}

	@Test
	void aggregateSetsTrue() {
		assertThat(new FluentMetaDataQuery().aggregate(true).get().getAggregate(), is(Boolean.TRUE));
	}

	@Test
	void fromClauseSetsValue() {
		assertThat(new FluentMetaDataQuery().from("bean in {admin.User}").get().getFrom(), is("bean in {admin.User}"));
	}

	@Test
	void filterClauseSetsValue() {
		assertThat(new FluentMetaDataQuery().filter("bean.active = true").get().getFilter(), is("bean.active = true"));
	}

	@Test
	void groupingClauseSetsValue() {
		assertThat(new FluentMetaDataQuery().grouping("bean.country").get().getGrouping(), is("bean.country"));
	}

	@Test
	void orderingClauseSetsValue() {
		assertThat(new FluentMetaDataQuery().ordering("bean.name asc").get().getOrdering(), is("bean.name asc"));
	}

	@Test
	void timeoutSetsValue() {
		assertThat(new FluentMetaDataQuery().timeoutInSeconds(30).get().getTimeoutInSeconds(), is(Integer.valueOf(30)));
	}

	// ---- projected column management ----

	@Test
	void addProjectedColumnAddsColumn() {
		FluentMetaDataQuery q = new FluentMetaDataQuery()
				.addProjectedColumn(new FluentMetaDataQueryProjectedColumn().name("col1"));
		assertEquals(1, q.get().getColumns().size());
	}

	@Test
	void findProjectedColumnByNameReturnsMatch() {
		FluentMetaDataQuery q = new FluentMetaDataQuery()
				.addProjectedColumn(new FluentMetaDataQueryProjectedColumn().name("col1"));
		assertThat(q.findProjectedColumnByName("col1"), is(notNullValue()));
	}

	@Test
	void findProjectedColumnByNameReturnsNullWhenMissing() {
		assertThat(new FluentMetaDataQuery().findProjectedColumnByName("missing"), is(nullValue()));
	}

	@Test
	void findProjectedColumnByBindingReturnsMatch() {
		FluentMetaDataQuery q = new FluentMetaDataQuery()
				.addProjectedColumn(new FluentMetaDataQueryProjectedColumn().binding("firstName"));
		assertThat(q.findProjectedColumnByBinding("firstName"), is(notNullValue()));
	}

	@Test
	void findProjectedColumnByBindingReturnsNullWhenMissing() {
		assertThat(new FluentMetaDataQuery().findProjectedColumnByBinding("missing"), is(nullValue()));
	}

	@Test
	void findProjectedColumnByDisplayNameReturnsMatch() {
		FluentMetaDataQuery q = new FluentMetaDataQuery()
				.addProjectedColumn(new FluentMetaDataQueryProjectedColumn().displayName("First Name"));
		assertThat(q.findProjectedColumnByDisplayName("First Name"), is(notNullValue()));
	}

	@Test
	void findProjectedColumnByDisplayNameReturnsNullWhenMissing() {
		assertThat(new FluentMetaDataQuery().findProjectedColumnByDisplayName("missing"), is(nullValue()));
	}

	// ---- content column management ----

	@Test
	void addContentColumnAddsColumn() {
		FluentMetaDataQuery q = new FluentMetaDataQuery()
				.addContentColumn(new FluentMetaDataQueryContentColumn().name("photo"));
		assertEquals(1, q.get().getColumns().size());
	}

	@Test
	void findContentColumnByNameReturnsMatch() {
		FluentMetaDataQuery q = new FluentMetaDataQuery()
				.addContentColumn(new FluentMetaDataQueryContentColumn().name("photo"));
		assertThat(q.findContentColumnByName("photo"), is(notNullValue()));
	}

	@Test
	void findContentColumnByNameReturnsNullWhenMissing() {
		assertThat(new FluentMetaDataQuery().findContentColumnByName("missing"), is(nullValue()));
	}

	@Test
	void findContentColumnByBindingReturnsMatch() {
		FluentMetaDataQuery q = new FluentMetaDataQuery()
				.addContentColumn(new FluentMetaDataQueryContentColumn().binding("avatar"));
		assertThat(q.findContentColumnByBinding("avatar"), is(notNullValue()));
	}

	@Test
	void findContentColumnByBindingReturnsNullWhenMissing() {
		assertThat(new FluentMetaDataQuery().findContentColumnByBinding("missing"), is(nullValue()));
	}

	@Test
	void findContentColumnByDisplayNameReturnsMatch() {
		FluentMetaDataQuery q = new FluentMetaDataQuery()
				.addContentColumn(new FluentMetaDataQueryContentColumn().displayName("Photo"));
		assertThat(q.findContentColumnByDisplayName("Photo"), is(notNullValue()));
	}

	@Test
	void findContentColumnByDisplayNameReturnsNullWhenMissing() {
		assertThat(new FluentMetaDataQuery().findContentColumnByDisplayName("missing"), is(nullValue()));
	}

	// ---- remove / clear ----

	@Test
	void removeColumnByNameRemovesIt() {
		FluentMetaDataQuery q = new FluentMetaDataQuery()
				.addProjectedColumn(new FluentMetaDataQueryProjectedColumn().name("col1"));
		q.removeColumnByName("col1");
		assertEquals(0, q.get().getColumns().size());
	}

	@Test
	void removeColumnByBindingRemovesIt() {
		FluentMetaDataQuery q = new FluentMetaDataQuery()
				.addProjectedColumn(new FluentMetaDataQueryProjectedColumn().binding("firstName"));
		q.removeColumnByBinding("firstName");
		assertEquals(0, q.get().getColumns().size());
	}

	@Test
	void removeColumnByDisplayNameRemovesIt() {
		FluentMetaDataQuery q = new FluentMetaDataQuery()
				.addProjectedColumn(new FluentMetaDataQueryProjectedColumn().displayName("First Name"));
		q.removeColumnByDisplayName("First Name");
		assertEquals(0, q.get().getColumns().size());
	}

	@Test
	void clearColumnsRemovesAll() {
		FluentMetaDataQuery q = new FluentMetaDataQuery()
				.addProjectedColumn(new FluentMetaDataQueryProjectedColumn().name("c1"))
				.addContentColumn(new FluentMetaDataQueryContentColumn().name("c2"));
		q.clearColumns();
		assertEquals(0, q.get().getColumns().size());
	}

	// ---- from(MetaDataQueryDefinition) ----

	@Test
	void fromMetaDataQueryDefinitionCopiesFields() {
		MetaDataQueryDefinitionImpl source = new MetaDataQueryDefinitionImpl();
		source.setName("qContacts");
		source.setDocumentName("Contact");
		source.setFromClause("bean in {admin.Contact}");
		source.setFilterClause("bean.active = true");
		source.setGroupClause("bean.country");
		source.setOrderClause("bean.name asc");

		FluentMetaDataQuery q = new FluentMetaDataQuery().from(source);

		assertThat(q.get().getName(), is("qContacts"));
		assertThat(q.get().getDocumentName(), is("Contact"));
		assertThat(q.get().getFrom(), is("bean in {admin.Contact}"));
		assertThat(q.get().getFilter(), is("bean.active = true"));
		assertThat(q.get().getGrouping(), is("bean.country"));
		assertThat(q.get().getOrdering(), is("bean.name asc"));
	}

	@Test
	void fromCopiesProjectedColumn() {
		MetaDataQueryDefinitionImpl source = new MetaDataQueryDefinitionImpl();
		source.getColumns().add(new MetaDataQueryProjectedColumnImpl());
		FluentMetaDataQuery q = new FluentMetaDataQuery().from(source);
		assertEquals(1, q.get().getColumns().size());
	}

	@Test
	void fromCopiesContentColumn() {
		MetaDataQueryDefinitionImpl source = new MetaDataQueryDefinitionImpl();
		source.getColumns().add(new MetaDataQueryContentColumnImpl());
		FluentMetaDataQuery q = new FluentMetaDataQuery().from(source);
		assertEquals(1, q.get().getColumns().size());
	}
}
