package org.skyve.metadata.module.fluent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.container.form.FormLabelLayout;
import org.skyve.metadata.view.View.ViewType;

/**
 * Tests for {@link FluentModule} builder methods.
 */
@SuppressWarnings("static-method")
class FluentModuleTest {

	// ---- constructors --------------------------------------------------------

	@Test
	void defaultConstructorCreatesInstance() {
		assertNotNull(new FluentModule().get());
	}

	@Test
	void wrappingConstructorPreservesInstance() {
		org.skyve.impl.metadata.repository.module.ModuleMetaData mod =
				new org.skyve.impl.metadata.repository.module.ModuleMetaData();
		assertNotNull(new FluentModule(mod).get());
	}

	// ---- scalar setters ------------------------------------------------------

	@Test
	void nameSetsValue() {
		assertEquals("admin", new FluentModule().name("admin").get().getName());
	}

	@Test
	void titleSetsValue() {
		assertEquals("Admin", new FluentModule().title("Admin").get().getTitle());
	}

	@Test
	void documentationSetsValue() {
		assertEquals("docs", new FluentModule().documentation("docs").get().getDocumentation());
	}

	@Test
	void homeDocumentSetsValue() {
		assertEquals("User", new FluentModule().homeDocument("User").get().getHomeDocument());
	}

	@Test
	void prototypeTrueSetsTrue() {
		assertTrue(Boolean.TRUE.equals(new FluentModule().prototype(true).get().getPrototype()));
	}

	@Test
	void prototypeFlaseSetsFalse() {
		assertFalse(Boolean.TRUE.equals(new FluentModule().prototype(false).get().getPrototype()));
	}

	@Test
	void formLabelLayoutSetsValue() {
		assertEquals(FormLabelLayout.top,
				new FluentModule().formLabelLayout(FormLabelLayout.top).get().getFormLabelLayout());
	}

	@Test
	void homeRefSetsValue() {
		assertEquals(ViewType.edit,
				new FluentModule().homeRef(ViewType.edit).get().getHomeRef());
	}

	// ---- jobs ----------------------------------------------------------------

	@Test
	void addJobAndFindJobByName() {
		FluentJob job = new FluentJob().name("myJob");
		FluentModule fm = new FluentModule().addJob(job);
		assertNotNull(fm.findJob("myJob"));
	}

	@Test
	void findJobReturnsNullWhenAbsent() {
		assertNull(new FluentModule().findJob("missing"));
	}

	@Test
	void removeJobRemovesIt() {
		FluentModule fm = new FluentModule().addJob(new FluentJob().name("myJob"));
		fm.removeJob("myJob");
		assertNull(fm.findJob("myJob"));
	}

	@Test
	void clearJobsEmptiesAll() {
		FluentModule fm = new FluentModule()
				.addJob(new FluentJob().name("j1"))
				.addJob(new FluentJob().name("j2"));
		fm.clearJobs();
		assertTrue(fm.get().getJobs().isEmpty());
	}

	// ---- documents -----------------------------------------------------------

	@Test
	void addDocumentAndFindDocumentByRef() {
		FluentModuleDocument doc = new FluentModuleDocument().ref("User");
		FluentModule fm = new FluentModule().addDocument(doc);
		assertNotNull(fm.findDocument("User"));
	}

	@Test
	void findDocumentReturnsNullWhenAbsent() {
		assertNull(new FluentModule().findDocument("missing"));
	}

	@Test
	void removeDocumentRemovesIt() {
		FluentModule fm = new FluentModule().addDocument(new FluentModuleDocument().ref("User"));
		fm.removeDocument("User");
		assertNull(fm.findDocument("User"));
	}

	@Test
	void clearDocumentsEmptiesAll() {
		FluentModule fm = new FluentModule()
				.addDocument(new FluentModuleDocument().ref("User"))
				.addDocument(new FluentModuleDocument().ref("Group"));
		fm.clearDocuments();
		assertTrue(fm.get().getDocuments().isEmpty());
	}

	// ---- metadata queries ----------------------------------------------------

	@Test
	void addMetaDataQueryAndFindByName() {
		FluentMetaDataQuery q = new FluentMetaDataQuery().name("qUser");
		FluentModule fm = new FluentModule().addMetaDataQuery(q);
		assertNotNull(fm.findMetaDataQuery("qUser"));
	}

	@Test
	void findMetaDataQueryReturnsNullWhenAbsent() {
		assertNull(new FluentModule().findMetaDataQuery("missing"));
	}

	// ---- SQL queries ---------------------------------------------------------

	@Test
	void addSQLAndFindSQLByName() {
		FluentSQL sql = new FluentSQL().name("qSQL");
		FluentModule fm = new FluentModule().addSQL(sql);
		assertNotNull(fm.findSQL("qSQL"));
	}

	@Test
	void findSQLReturnsNullWhenAbsent() {
		assertNull(new FluentModule().findSQL("missing"));
	}

	// ---- BizQL queries -------------------------------------------------------

	@Test
	void addBizQLAndFindBizQLByName() {
		FluentBizQL bql = new FluentBizQL().name("qBizQL");
		FluentModule fm = new FluentModule().addBizQL(bql);
		assertNotNull(fm.findBizQL("qBizQL"));
	}

	@Test
	void findBizQLReturnsNullWhenAbsent() {
		assertNull(new FluentModule().findBizQL("missing"));
	}

	// ---- removeQuery / clearQueries ------------------------------------------

	@Test
	void removeQueryByNameRemovesIt() {
		FluentModule fm = new FluentModule().addMetaDataQuery(new FluentMetaDataQuery().name("q1"));
		fm.removeQuery("q1");
		assertNull(fm.findMetaDataQuery("q1"));
	}

	@Test
	void clearQueriesEmptiesAll() {
		FluentModule fm = new FluentModule()
				.addMetaDataQuery(new FluentMetaDataQuery().name("q1"))
				.addSQL(new FluentSQL().name("q2"));
		fm.clearQueries();
		assertTrue(fm.get().getQueries().isEmpty());
	}

	// ---- roles ---------------------------------------------------------------

	@Test
	void addRoleAndFindRoleByName() {
		FluentModuleRole role = new FluentModuleRole().name("DataEntry");
		FluentModule fm = new FluentModule().addRole(role);
		assertNotNull(fm.findRole("DataEntry"));
	}

	@Test
	void findRoleReturnsNullWhenAbsent() {
		assertNull(new FluentModule().findRole("missing"));
	}

	@Test
	void removeRoleRemovesIt() {
		FluentModule fm = new FluentModule().addRole(new FluentModuleRole().name("DataEntry"));
		fm.removeRole("DataEntry");
		assertNull(fm.findRole("DataEntry"));
	}

	@Test
	void clearRolesEmptiesAll() {
		FluentModule fm = new FluentModule()
				.addRole(new FluentModuleRole().name("r1"))
				.addRole(new FluentModuleRole().name("r2"));
		fm.clearRoles();
		assertTrue(fm.get().getRoles().isEmpty());
	}

	// ---- menu ----------------------------------------------------------------

	@Test
	void menuSetsMenu() {
		FluentModule fm = new FluentModule().menu(new FluentMenu());
		assertNotNull(fm.get().getMenu());
	}

	// ---- from ----------------------------------------------------------------

	@Test
	void fromModuleImplCopiesBasicFields() {
		ModuleImpl mod = new ModuleImpl();
		mod.setName("testModule");
		mod.setTitle("Test Module");
		mod.setDocumentation("Module docs");
		mod.setPrototype(true);
		mod.setFormLabelLayout(FormLabelLayout.side);
		mod.setHomeRef(ViewType.list);
		mod.setHomeDocumentName("Contact");
		// menu is required by from() - set an empty menu
		mod.setMenu(new org.skyve.impl.metadata.module.menu.MenuImpl());

		FluentModule fm = new FluentModule().from(mod);

		assertEquals("testModule", fm.get().getName());
		assertEquals("Test Module", fm.get().getTitle());
		assertEquals("Module docs", fm.get().getDocumentation());
		assertTrue(Boolean.TRUE.equals(fm.get().getPrototype()));
		assertEquals(FormLabelLayout.side, fm.get().getFormLabelLayout());
		assertEquals(ViewType.list, fm.get().getHomeRef());
		assertEquals("Contact", fm.get().getHomeDocument());
	}
}
