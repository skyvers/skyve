package org.skyve.impl.metadata.module;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.model.document.AssociationImpl;
import org.skyve.impl.metadata.model.document.field.Content;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.metadata.module.query.BizQLDefinitionImpl;
import org.skyve.impl.metadata.module.query.MetaDataQueryDefinitionImpl;
import org.skyve.impl.metadata.module.query.SQLDefinitionImpl;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.view.container.form.FormLabelLayout;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.document.Association.AssociationType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.module.query.QueryDefinition;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.view.View.ViewType;

@SuppressWarnings("static-method")
class ModuleImplTest {

	private static ModuleImpl newModule(String name) {
		ModuleImpl m = new ModuleImpl();
		m.setName(name);
		return m;
	}

	@Test
	void getNameReturnsSetValue() {
		ModuleImpl m = newModule("Admin");
		assertThat(m.getName(), is("Admin"));
	}

	@Test
	void getTitleReturnsSetValue() {
		ModuleImpl m = newModule("Admin");
		m.setTitle("Administration");
		assertThat(m.getTitle(), is("Administration"));
	}

	@Test
	void isPrototypeFalseByDefault() {
		ModuleImpl m = newModule("M");
		assertFalse(m.isPrototype());
	}

	@Test
	void setPrototypeTrue() {
		ModuleImpl m = newModule("M");
		m.setPrototype(true);
		assertTrue(m.isPrototype());
	}

	@Test
	void getFormLabelLayoutNullByDefault() {
		ModuleImpl m = newModule("M");
		assertThat(m.getFormLabelLayout(), is(nullValue()));
	}

	@Test
	void setFormLabelLayout() {
		ModuleImpl m = newModule("M");
		m.setFormLabelLayout(FormLabelLayout.side);
		assertThat(m.getFormLabelLayout(), is(FormLabelLayout.side));
	}

	@Test
	void getDocumentRefsNotNull() {
		ModuleImpl m = newModule("M");
		assertThat(m.getDocumentRefs(), is(notNullValue()));
	}

	@Test
	void getHomeRefNullByDefault() {
		ModuleImpl m = newModule("M");
		assertThat(m.getHomeRef(), is(nullValue()));
	}

	@Test
	void setHomeRef() {
		ModuleImpl m = newModule("M");
		m.setHomeRef(ViewType.list);
		assertThat(m.getHomeRef(), is(ViewType.list));
	}

	@Test
	void getHomeDocumentNameNullByDefault() {
		ModuleImpl m = newModule("M");
		assertThat(m.getHomeDocumentName(), is(nullValue()));
	}

	@Test
	void setHomeDocumentName() {
		ModuleImpl m = newModule("M");
		m.setHomeDocumentName("Contact");
		assertThat(m.getHomeDocumentName(), is("Contact"));
	}

	@Test
	void getMenuNullByDefault() {
		ModuleImpl m = newModule("M");
		assertThat(m.getMenu(), is(nullValue()));
	}

	@Test
	void getDocumentationNullByDefault() {
		ModuleImpl m = newModule("M");
		assertThat(m.getDocumentation(), is(nullValue()));
	}

	@Test
	void setDocumentation() {
		ModuleImpl m = newModule("M");
		m.setDocumentation("some docs");
		assertThat(m.getDocumentation(), is("some docs"));
	}

	@Test
	void getPropertiesNotNull() {
		ModuleImpl m = newModule("M");
		assertThat(m.getProperties(), is(notNullValue()));
	}

	@Test
	void lastModifiedMillisDefaultIsMaxValue() {
		ModuleImpl m = newModule("M");
		assertEquals(Long.MAX_VALUE, m.getLastModifiedMillis());
	}

	@Test
	void setLastModifiedMillis() {
		ModuleImpl m = newModule("M");
		m.setLastModifiedMillis(12345L);
		assertEquals(12345L, m.getLastModifiedMillis());
	}

	@Test
	void setLastCheckedMillis() {
		ModuleImpl m = newModule("M");
		m.setLastCheckedMillis(99L);
		assertEquals(99L, m.getLastCheckedMillis());
	}

	@Test
	void putAndGetBizQLQuery() {
		ModuleImpl m = newModule("M");
		BizQLDefinitionImpl q = new BizQLDefinitionImpl();
		q.setName("myBizQL");
		q.setDescription("desc");
		m.putQuery(q);
		assertThat(m.getBizQL("myBizQL"), is(q));
	}

	@Test
	void putAndGetSQLQuery() {
		ModuleImpl m = newModule("M");
		SQLDefinitionImpl q = new SQLDefinitionImpl();
		q.setName("mySQL");
		q.setDescription("desc");
		m.putQuery(q);
		assertThat(m.getSQL("mySQL"), is(q));
	}

	@Test
	void putAndGetMetaDataQuery() {
		ModuleImpl m = newModule("M");
		MetaDataQueryDefinitionImpl q = new MetaDataQueryDefinitionImpl();
		q.setName("mdq");
		q.setDescription("d");
		q.setDocumentName("Contact");
		m.putQuery(q);
		assertThat(m.getMetaDataQuery("mdq"), is(q));
	}

	@Test
	void getMetaDataQueryReturnsNullWhenNotFound() {
		ModuleImpl m = newModule("M");
		assertThat(m.getMetaDataQuery("nonExistent"), is(nullValue()));
	}

	@Test
	void getSQLReturnsNullWhenNotFound() {
		ModuleImpl m = newModule("M");
		assertThat(m.getSQL("nonExistent"), is(nullValue()));
	}

	@Test
	void getBizQLReturnsNullWhenNotFound() {
		ModuleImpl m = newModule("M");
		assertThat(m.getBizQL("nonExistent"), is(nullValue()));
	}

	@Test
	void getMetadataQueriesReturnsAllQueries() {
		ModuleImpl m = newModule("M");
		BizQLDefinitionImpl q1 = new BizQLDefinitionImpl();
		q1.setName("q1");
		q1.setDescription("d1");
		BizQLDefinitionImpl q2 = new BizQLDefinitionImpl();
		q2.setName("q2");
		q2.setDescription("d2");
		m.putQuery(q1);
		m.putQuery(q2);
		List<QueryDefinition> queries = m.getMetadataQueries();
		assertEquals(2, queries.size());
	}

	@Test
	void putJobAndGetJob() {
		ModuleImpl m = newModule("M");
		JobMetaDataImpl job = new JobMetaDataImpl();
		job.setName("myJob");
		job.setDisplayName("My Job");
		job.setClassName("com.example.Job");
		m.putJob(job);
		assertThat(m.getJob("myJob"), is((JobMetaData) job));
	}

	@Test
	void getJobThrowsWhenNotFound() {
		ModuleImpl m = newModule("M");
		assertThrows(Exception.class, () -> m.getJob("unknown"));
	}

	@Test
	void getJobsEmptyByDefault() {
		ModuleImpl m = newModule("M");
		assertTrue(m.getJobs().isEmpty());
	}

	@Test
	void getRolesEmptyByDefault() {
		ModuleImpl m = newModule("M");
		assertTrue(m.getRoles().isEmpty());
	}

	@Test
	void getRoleReturnsNullWhenNotFound() {
		ModuleImpl m = newModule("M");
		assertThat(m.getRole("noRole"), is(nullValue()));
	}

	@Test
	void toStringIncludesName() {
		ModuleImpl m = newModule("TestMod");
		assertTrue(m.toString().contains("TestMod"));
	}

	@Test
	void putQueryDuplicateNameThrows() {
		ModuleImpl m = newModule("M");
		BizQLDefinitionImpl q1 = new BizQLDefinitionImpl();
		q1.setName("dup");
		q1.setDescription("d");
		m.putQuery(q1);
		BizQLDefinitionImpl q2 = new BizQLDefinitionImpl();
		q2.setName("dup");
		q2.setDescription("d2");
		assertThrows(Exception.class, () -> m.putQuery(q2));
	}

	@Test
	void lastCheckedMillisDefaultIsPositive() {
		assertTrue(newModule("M").getLastCheckedMillis() > 0L);
	}

	@Test
	void lastCheckedMillisRoundTrip() {
		ModuleImpl m = newModule("M");
		m.setLastCheckedMillis(99999L);
		assertEquals(99999L, m.getLastCheckedMillis());
	}

	@Test
	void menuRoundTrip() {
		ModuleImpl m = newModule("M");
		org.skyve.impl.metadata.module.menu.MenuImpl menu = new org.skyve.impl.metadata.module.menu.MenuImpl();
		m.setMenu(menu);
		assertNotNull(m.getMenu());
	}

	@Test
	void putRoleAndGetRole() {
		ModuleImpl m = newModule("M");
		org.skyve.impl.metadata.user.RoleImpl role = new org.skyve.impl.metadata.user.RoleImpl();
		role.setName("viewer");
		m.putRole(role);
		assertEquals(role, m.getRole("viewer"));
	}

	@Test
	void getRolesContainsPutRole() {
		ModuleImpl m = newModule("M");
		org.skyve.impl.metadata.user.RoleImpl role = new org.skyve.impl.metadata.user.RoleImpl();
		role.setName("admin");
		m.putRole(role);
		assertEquals(1, m.getRoles().size());
	}

	// ---- JobMetaDataImpl supplemental tests ----

	@Test
	void jobMetaDataImplOwningModuleNameRoundtrip() {
		JobMetaDataImpl job = new JobMetaDataImpl();
		job.setOwningModuleName("admin");
		assertEquals("admin", job.getOwningModuleName());
	}

	@Test
	void jobMetaDataImplPropertiesNotNull() {
		JobMetaDataImpl job = new JobMetaDataImpl();
		assertNotNull(job.getProperties());
	}

	@Test
	void jobMetaDataImplToStringContainsName() {
		JobMetaDataImpl job = new JobMetaDataImpl();
		job.setName("myJob");
		assertNotNull(job.toString());
		assertTrue(job.toString().contains("myJob"));
	}

	// ---- Module getLocalisedTitle ----

	@Test
	void getLocalisedTitleReturnsNonNullForSetTitle() {
		ModuleImpl m = newModule("M");
		m.setTitle("My Module Title");
		assertNotNull(m.getLocalisedTitle());
	}

	// ---- Module getNullSafeMetaDataQuery ----

	@Test
	void getNullSafeMetaDataQueryReturnsQueryWhenFound() {
		ModuleImpl m = newModule("M");
		MetaDataQueryDefinitionImpl query = new MetaDataQueryDefinitionImpl();
		query.setName("myQuery");
		m.putQuery(query);
		assertNotNull(m.getNullSafeMetaDataQuery("myQuery"));
	}

	@Test
	void getNullSafeMetaDataQueryThrowsWhenNotFound() {
		ModuleImpl m = newModule("M");
		assertThrows(org.skyve.metadata.MetaDataException.class,
				() -> m.getNullSafeMetaDataQuery("missing"));
	}

	// ---- Module getNullSafeRole ----

	@Test
	void getNullSafeRoleReturnsRoleWhenFound() {
		ModuleImpl m = newModule("M");
		org.skyve.impl.metadata.user.RoleImpl role = new org.skyve.impl.metadata.user.RoleImpl();
		role.setName("BasicUser");
		m.putRole(role);
		assertNotNull(m.getNullSafeRole("BasicUser"));
	}

	@Test
	void getNullSafeRoleThrowsWhenNotFound() {
		ModuleImpl m = newModule("M");
		assertThrows(org.skyve.metadata.MetaDataException.class,
				() -> m.getNullSafeRole("ghost"));
	}

	// ---- getDocumentDefaultQuery branches ----

	@Test
	void getDocumentDefaultQueryThrowsWhenNoDocumentRef() {
		ModuleImpl m = newModule("M");
		// No document refs — should throw MetaDataException
		assertThrows(org.skyve.metadata.MetaDataException.class,
				() -> m.getDocumentDefaultQuery(null, "NoDoc"));
	}

	@Test
	void getDocumentDefaultQueryWithNamedQueryReturnsQuery() {
		ModuleImpl m = newModule("M");
		// Add a query to the module
		MetaDataQueryDefinitionImpl query = new MetaDataQueryDefinitionImpl();
		query.setName("AllFoo");
		query.setDocumentName("Foo");
		m.putQuery(query);
		// Add a DocumentRef pointing to that query
		org.skyve.metadata.module.Module.DocumentRef ref = new org.skyve.metadata.module.Module.DocumentRef();
		ref.setDefaultQueryName("AllFoo");
		m.getDocumentRefs().put("Foo", ref);
		// Should return the named query
		org.skyve.metadata.module.query.MetaDataQueryDefinition result = m.getDocumentDefaultQuery(null, "Foo");
		assertNotNull(result);
		assertEquals("AllFoo", result.getName());
	}

	@Test
	void getDocumentDefaultQueryThrowsWhenNamedQueryMissing() {
		ModuleImpl m = newModule("M");
		// Add a DocumentRef pointing to a non-existent query
		org.skyve.metadata.module.Module.DocumentRef ref = new org.skyve.metadata.module.Module.DocumentRef();
		ref.setDefaultQueryName("NonExistentQuery");
		m.getDocumentRefs().put("Bar", ref);
		assertThrows(org.skyve.metadata.MetaDataException.class,
				() -> m.getDocumentDefaultQuery(null, "Bar"));
	}

	@Test
	void getDocumentReturnsRepositoryDocumentWhenFound() {
		ModuleImpl m = newModule("M");
		ProvidedRepository originalRepository = getRepository();
		ProvidedRepository repository = mock(ProvidedRepository.class);
		Customer customer = mock(Customer.class);
		Document document = mock(Document.class);
		when(repository.getDocument(customer, m, "Contact")).thenReturn(document);
		setRepository(repository);
		try {
			assertEquals(document, m.getDocument(customer, "Contact"));
		}
		finally {
			setRepository(originalRepository);
		}
	}

	@Test
	void getDocumentThrowsWhenRepositoryReturnsNull() {
		ModuleImpl m = newModule("M");
		ProvidedRepository originalRepository = getRepository();
		ProvidedRepository repository = mock(ProvidedRepository.class);
		Customer customer = mock(Customer.class);
		when(repository.getDocument(customer, m, "MissingDoc")).thenReturn(null);
		setRepository(repository);
		try {
			assertThrows(IllegalStateException.class, () -> m.getDocument(customer, "MissingDoc"));
		}
		finally {
			setRepository(originalRepository);
		}
	}

	@Test
	@SuppressWarnings("boxing")
	void getDocumentDefaultQueryThrowsForTransientDocumentWithoutDefaultQueryName() {
		ModuleImpl m = newModule("M");
		org.skyve.metadata.module.Module.DocumentRef ref = new org.skyve.metadata.module.Module.DocumentRef();
		m.getDocumentRefs().put("TransientDoc", ref);

		ProvidedRepository originalRepository = getRepository();
		ProvidedRepository repository = mock(ProvidedRepository.class);
		Customer customer = mock(Customer.class);
		Document document = mock(Document.class);
		when(repository.getDocument(customer, m, "TransientDoc")).thenReturn(document);
		doReturn(false).when(document).isPersistable();
		when(document.getOwningModuleName()).thenReturn("M");
		when(document.getName()).thenReturn("TransientDoc");
		setRepository(repository);
		try {
			assertThrows(org.skyve.metadata.MetaDataException.class,
					() -> m.getDocumentDefaultQuery(customer, "TransientDoc", true));
		}
		finally {
			setRepository(originalRepository);
		}
	}

	@Test
	@SuppressWarnings("boxing")
	void getDocumentDefaultQueryBuildsColumnsForFieldContentAndAssociation() {
		ModuleImpl m = newModule("M");
		org.skyve.metadata.module.Module.DocumentRef ref = new org.skyve.metadata.module.Module.DocumentRef();
		m.getDocumentRefs().put("Invoice", ref);

		ProvidedRepository originalRepository = getRepository();
		ProvidedRepository repository = mock(ProvidedRepository.class);
		Customer customer = mock(Customer.class);
		Document baseDocument = mock(Document.class);
		Document document = mock(Document.class);

		Text inheritedText = new Text();
		inheritedText.setName("inheritedName");
		inheritedText.setPersistent(true);
		inheritedText.setDeprecated(false);

		Text text = new Text();
		text.setName("number");
		text.setPersistent(true);
		text.setDeprecated(false);

		Content content = new Content();
		content.setName("attachment");
		content.setPersistent(true);
		content.setDeprecated(false);

		AssociationImpl embeddedAssociation = new AssociationImpl();
		embeddedAssociation.setName("embeddedRef");
		embeddedAssociation.setPersistent(true);
		embeddedAssociation.setDeprecated(false);
		embeddedAssociation.setType(AssociationType.embedded);

		AssociationImpl association = new AssociationImpl();
		association.setName("customer");
		association.setPersistent(true);
		association.setDeprecated(false);
		association.setType(AssociationType.aggregation);

		Extends inherits = new Extends();
		inherits.setDocumentName("BaseInvoice");

		when(repository.getDocument(customer, m, "Invoice")).thenReturn(document);
		when(repository.getDocument(customer, m, "BaseInvoice")).thenReturn(baseDocument);
		doReturn(true).when(document).isPersistable();
		when(document.getLocalisedPluralAlias()).thenReturn("Invoices");
		when(document.getExtends()).thenReturn(inherits);
		List<Attribute> documentAttributes = List.of(text, content, embeddedAssociation, association);
		doReturn(documentAttributes).when(document).getAttributes();
		when(baseDocument.getExtends()).thenReturn(null);
		List<Attribute> baseAttributes = List.of(inheritedText);
		doReturn(baseAttributes).when(baseDocument).getAttributes();

		setRepository(repository);
		try {
			MetaDataQueryDefinitionImpl query = (MetaDataQueryDefinitionImpl) m.getDocumentDefaultQuery(customer, "Invoice", true);
			assertEquals("Invoice", query.getName());
			assertEquals("Invoice", query.getDocumentName());
			assertEquals("All Invoices", query.getDescription());
			assertEquals(m, query.getOwningModule());

			List<String> bindings = new ArrayList<>();
			query.getColumns().forEach(c -> bindings.add(c.getBinding()));
			assertTrue(bindings.contains("inheritedName"));
			assertTrue(bindings.contains("number"));
			assertTrue(bindings.contains("attachment"));
			assertTrue(bindings.contains("customer.bizKey"));
			assertFalse(bindings.contains("embeddedRef.bizKey"));

			assertEquals(SortDirection.ascending, query.getColumns().get(0).getSortOrder());
		}
		finally {
			setRepository(originalRepository);
		}
	}

	private static ProvidedRepository getRepository() {
		try {
			Field field = ProvidedRepositoryFactory.class.getDeclaredField("repository");
			field.setAccessible(true);
			return (ProvidedRepository) field.get(null);
		}
		catch (ReflectiveOperationException e) {
			throw new RuntimeException(e);
		}
	}

	private static void setRepository(ProvidedRepository repository) {
		try {
			Field field = ProvidedRepositoryFactory.class.getDeclaredField("repository");
			field.setAccessible(true);
			field.set(null, repository);
		}
		catch (ReflectiveOperationException e) {
			throw new RuntimeException(e);
		}
	}
}
