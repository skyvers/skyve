package org.skyve.metadata.module.fluent;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

/**
 * Tests for {@link FluentModule} remove/clear/find methods and
 * SQL/BizQL/menu management not covered in {@link FluentModuleBuildersTest}.
 */
@SuppressWarnings("static-method")
class FluentModuleMutationTest {

	// ---- wrapping constructor ----

	@Test
	void wrappingConstructorPreservesInstance() {
		org.skyve.impl.metadata.repository.module.ModuleMetaData meta =
				new org.skyve.impl.metadata.repository.module.ModuleMetaData();
		meta.setName("existing");
		FluentModule m = new FluentModule(meta);
		assertThat(m.get(), is(meta));
		assertThat(m.get().getName(), is("existing"));
	}

	// ---- Job find / remove / clear ----

	@Test
	void findJobReturnsMatch() {
		FluentModule m = new FluentModule();
		m.addJob(new FluentJob().name("SendEmail").className("jobs.SendEmail"));
		assertThat(m.findJob("SendEmail"), is(notNullValue()));
	}

	@Test
	void findJobReturnsNullWhenNotFound() {
		FluentModule m = new FluentModule();
		assertThat(m.findJob("missing"), is(nullValue()));
	}

	@Test
	void removeJobRemovesEntry() {
		FluentModule m = new FluentModule();
		m.addJob(new FluentJob().name("Job1"));
		m.addJob(new FluentJob().name("Job2"));
		m.removeJob("Job1");
		assertEquals(1, m.get().getJobs().size());
		assertThat(m.findJob("Job1"), is(nullValue()));
	}

	@Test
	void clearJobsRemovesAll() {
		FluentModule m = new FluentModule();
		m.addJob(new FluentJob().name("Job1"));
		m.addJob(new FluentJob().name("Job2"));
		m.clearJobs();
		assertEquals(0, m.get().getJobs().size());
	}

	// ---- Document find / remove / clear ----

	@Test
	void findDocumentReturnsMatch() {
		FluentModule m = new FluentModule();
		m.addDocument(new FluentModuleDocument().ref("Contact"));
		assertThat(m.findDocument("Contact"), is(notNullValue()));
	}

	@Test
	void findDocumentReturnsNullWhenNotFound() {
		FluentModule m = new FluentModule();
		assertThat(m.findDocument("missing"), is(nullValue()));
	}

	@Test
	void removeDocumentRemovesEntry() {
		FluentModule m = new FluentModule();
		m.addDocument(new FluentModuleDocument().ref("Contact"));
		m.addDocument(new FluentModuleDocument().ref("User"));
		m.removeDocument("Contact");
		assertEquals(1, m.get().getDocuments().size());
		assertThat(m.findDocument("Contact"), is(nullValue()));
	}

	@Test
	void clearDocumentsRemovesAll() {
		FluentModule m = new FluentModule();
		m.addDocument(new FluentModuleDocument().ref("Contact"));
		m.addDocument(new FluentModuleDocument().ref("User"));
		m.clearDocuments();
		assertEquals(0, m.get().getDocuments().size());
	}

	// ---- SQL query add / find ----

	@Test
	void addSQLAddsQuery() {
		FluentModule m = new FluentModule();
		m.addSQL(new FluentSQL().name("qSql").query("SELECT 1"));
		assertEquals(1, m.get().getQueries().size());
	}

	@Test
	void findSQLReturnsMatch() {
		FluentModule m = new FluentModule();
		m.addSQL(new FluentSQL().name("qSql"));
		assertThat(m.findSQL("qSql"), is(notNullValue()));
	}

	@Test
	void findSQLReturnsNullWhenNotFound() {
		FluentModule m = new FluentModule();
		assertThat(m.findSQL("missing"), is(nullValue()));
	}

	// ---- BizQL query add / find ----

	@Test
	void addBizQLAddsQuery() {
		FluentModule m = new FluentModule();
		m.addBizQL(new FluentBizQL().name("qBizQL").query("SELECT bean FROM {Contact} bean"));
		assertEquals(1, m.get().getQueries().size());
	}

	@Test
	void findBizQLReturnsMatch() {
		FluentModule m = new FluentModule();
		m.addBizQL(new FluentBizQL().name("qBizQL"));
		assertThat(m.findBizQL("qBizQL"), is(notNullValue()));
	}

	@Test
	void findBizQLReturnsNullWhenNotFound() {
		FluentModule m = new FluentModule();
		assertThat(m.findBizQL("missing"), is(nullValue()));
	}

	// ---- MetaDataQuery find ----

	@Test
	void findMetaDataQueryReturnsMatch() {
		FluentModule m = new FluentModule();
		m.addMetaDataQuery(new FluentMetaDataQuery().name("qContacts").documentName("Contact"));
		assertThat(m.findMetaDataQuery("qContacts"), is(notNullValue()));
	}

	@Test
	void findMetaDataQueryReturnsNullWhenNotFound() {
		FluentModule m = new FluentModule();
		assertThat(m.findMetaDataQuery("missing"), is(nullValue()));
	}

	// ---- Query remove / clear ----

	@Test
	void removeQueryRemovesEntry() {
		FluentModule m = new FluentModule();
		m.addMetaDataQuery(new FluentMetaDataQuery().name("q1").documentName("Contact"));
		m.addSQL(new FluentSQL().name("q2"));
		m.removeQuery("q1");
		assertEquals(1, m.get().getQueries().size());
	}

	@Test
	void clearQueriesRemovesAll() {
		FluentModule m = new FluentModule();
		m.addMetaDataQuery(new FluentMetaDataQuery().name("q1").documentName("Contact"));
		m.addSQL(new FluentSQL().name("q2"));
		m.clearQueries();
		assertEquals(0, m.get().getQueries().size());
	}

	// ---- Role find / remove / clear ----

	@Test
	void findRoleReturnsMatch() {
		FluentModule m = new FluentModule();
		m.addRole(new FluentModuleRole().name("Admin"));
		assertThat(m.findRole("Admin"), is(notNullValue()));
	}

	@Test
	void findRoleReturnsNullWhenNotFound() {
		FluentModule m = new FluentModule();
		assertThat(m.findRole("missing"), is(nullValue()));
	}

	@Test
	void removeRoleRemovesEntry() {
		FluentModule m = new FluentModule();
		m.addRole(new FluentModuleRole().name("Admin"));
		m.addRole(new FluentModuleRole().name("User"));
		m.removeRole("Admin");
		assertEquals(1, m.get().getRoles().size());
		assertThat(m.findRole("Admin"), is(nullValue()));
	}

	@Test
	void clearRolesRemovesAll() {
		FluentModule m = new FluentModule();
		m.addRole(new FluentModuleRole().name("Admin"));
		m.addRole(new FluentModuleRole().name("User"));
		m.clearRoles();
		assertEquals(0, m.get().getRoles().size());
	}

	// ---- menu ----

	@Test
	void menuSetsMenuInstance() {
		FluentModule m = new FluentModule();
		FluentMenu menu = new FluentMenu();
		menu.addEditItem(new FluentEditItem().name("EditContact"));
		m.menu(menu);
		assertThat(m.get().getMenu(), is(menu.get()));
	}
}
