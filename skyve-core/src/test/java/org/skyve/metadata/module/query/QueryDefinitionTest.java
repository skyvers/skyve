package org.skyve.metadata.module.query;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.module.query.BizQLDefinitionImpl;
import org.skyve.impl.metadata.module.query.MetaDataQueryDefinitionImpl;
import org.skyve.impl.metadata.module.query.MetaDataQueryProjectedColumnImpl;
import org.skyve.impl.metadata.module.query.SQLDefinitionImpl;

@SuppressWarnings("static-method")
class QueryDefinitionTest {

	// ---- BizQLDefinitionImpl ----

	@Test
	void bizQLGetSetQuery() {
		BizQLDefinitionImpl q = new BizQLDefinitionImpl();
		q.setQuery("FROM Admin.User");
		assertThat(q.getQuery(), is("FROM Admin.User"));
	}

	@Test
	void bizQLGetSetName() {
		BizQLDefinitionImpl q = new BizQLDefinitionImpl();
		q.setName("myQuery");
		assertThat(q.getName(), is("myQuery"));
	}

	@Test
	void bizQLGetSetDescription() {
		BizQLDefinitionImpl q = new BizQLDefinitionImpl();
		q.setDescription("desc");
		assertThat(q.getDescription(), is("desc"));
	}

	@Test
	void bizQLGetSetDocumentation() {
		BizQLDefinitionImpl q = new BizQLDefinitionImpl();
		q.setDocumentation("docs");
		assertThat(q.getDocumentation(), is("docs"));
	}

	@Test
	void bizQLDocumentationNullByDefault() {
		BizQLDefinitionImpl q = new BizQLDefinitionImpl();
		assertThat(q.getDocumentation(), is(nullValue()));
	}

	@Test
	void bizQLGetSetTimeout() {
		BizQLDefinitionImpl q = new BizQLDefinitionImpl();
		q.setTimeoutInSeconds(30);
		assertEquals(30, q.getTimeoutInSeconds());
	}

	@Test
	void bizQLDefaultTimeout() {
		BizQLDefinitionImpl q = new BizQLDefinitionImpl();
		assertEquals(0, q.getTimeoutInSeconds());
	}

	@Test
	void bizQLSetOwningModule() {
		BizQLDefinitionImpl q = new BizQLDefinitionImpl();
		ModuleImpl module = new ModuleImpl();
		module.setName("Admin");
		q.setOwningModule(module);
		assertThat(q.getOwningModule(), is(module));
	}

	@Test
	void bizQLToStringIncludesName() {
		BizQLDefinitionImpl q = new BizQLDefinitionImpl();
		q.setName("qName");
		assertTrue(q.toString().contains("qName"));
	}

	@Test
	void bizQLPropertiesNotNull() {
		BizQLDefinitionImpl q = new BizQLDefinitionImpl();
		assertThat(q.getProperties(), is(org.hamcrest.CoreMatchers.notNullValue()));
	}

	// ---- SQLDefinitionImpl ----

	@Test
	void sqlGetSetQuery() {
		SQLDefinitionImpl q = new SQLDefinitionImpl();
		q.setQuery("SELECT * FROM dual");
		assertThat(q.getQuery(), is("SELECT * FROM dual"));
	}

	@Test
	void sqlGetSetName() {
		SQLDefinitionImpl q = new SQLDefinitionImpl();
		q.setName("sqlQ");
		assertThat(q.getName(), is("sqlQ"));
	}

	@Test
	void sqlDefaultTimeout() {
		SQLDefinitionImpl q = new SQLDefinitionImpl();
		assertEquals(0, q.getTimeoutInSeconds());
	}

	@Test
	void sqlSetTimeout() {
		SQLDefinitionImpl q = new SQLDefinitionImpl();
		q.setTimeoutInSeconds(60);
		assertEquals(60, q.getTimeoutInSeconds());
	}

	// ---- MetaDataQueryDefinitionImpl ----

	@Test
	void metaDataQueryGetSetDocumentName() {
		MetaDataQueryDefinitionImpl q = new MetaDataQueryDefinitionImpl();
		q.setDocumentName("Contact");
		assertThat(q.getDocumentName(), is("Contact"));
	}

	@Test
	void metaDataQueryColumnsNotNull() {
		MetaDataQueryDefinitionImpl q = new MetaDataQueryDefinitionImpl();
		assertThat(q.getColumns(), is(org.hamcrest.CoreMatchers.notNullValue()));
	}

	@Test
	void metaDataQueryGetSetName() {
		MetaDataQueryDefinitionImpl q = new MetaDataQueryDefinitionImpl();
		q.setName("myMetaQuery");
		assertThat(q.getName(), is("myMetaQuery"));
	}

	// ---- MetaDataQueryProjectedColumnImpl ----

	@Test
	void projectedColumnGetSetBinding() {
		MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
		col.setBinding("firstName");
		assertThat(col.getBinding(), is("firstName"));
	}

	@Test
	void projectedColumnGetSetDisplayName() {
		MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
		col.setDisplayName("First Name");
		assertThat(col.getDisplayName(), is("First Name"));
	}

	// ---- QueryDefinitionImpl abstract base ----

	@Test
	void queryDefinitionImplSetDescription() {
		BizQLDefinitionImpl q = new BizQLDefinitionImpl();
		q.setDescription("A description");
		assertThat(q.getDescription(), is("A description"));
	}

	@Test
	void getLocalisedDescriptionFallsBackToDescription() {
		// Without an i18n bundle entry for the key, nullSafeI18n returns the key itself
		BizQLDefinitionImpl q = new BizQLDefinitionImpl();
		q.setDescription("My Query");
		assertThat(q.getLocalisedDescription(), is("My Query"));
	}
}
