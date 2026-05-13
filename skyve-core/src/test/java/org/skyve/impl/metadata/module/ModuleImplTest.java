package org.skyve.impl.metadata.module;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.module.query.BizQLDefinitionImpl;
import org.skyve.impl.metadata.module.query.MetaDataQueryDefinitionImpl;
import org.skyve.impl.metadata.module.query.SQLDefinitionImpl;
import org.skyve.impl.metadata.view.container.form.FormLabelLayout;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.module.query.QueryDefinition;
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
		assertThat(m.isPrototype(), is(false));
	}

	@Test
	void setPrototypeTrue() {
		ModuleImpl m = newModule("M");
		m.setPrototype(true);
		assertThat(m.isPrototype(), is(true));
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
		assertThat(m.getLastModifiedMillis(), is(Long.MAX_VALUE));
	}

	@Test
	void setLastModifiedMillis() {
		ModuleImpl m = newModule("M");
		m.setLastModifiedMillis(12345L);
		assertThat(m.getLastModifiedMillis(), is(12345L));
	}

	@Test
	void setLastCheckedMillis() {
		ModuleImpl m = newModule("M");
		m.setLastCheckedMillis(99L);
		assertThat(m.getLastCheckedMillis(), is(99L));
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
		assertThat(queries.size(), is(2));
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
		assertThat(m.getJobs().isEmpty(), is(true));
	}

	@Test
	void getRolesEmptyByDefault() {
		ModuleImpl m = newModule("M");
		assertThat(m.getRoles().isEmpty(), is(true));
	}

	@Test
	void getRoleReturnsNullWhenNotFound() {
		ModuleImpl m = newModule("M");
		assertThat(m.getRole("noRole"), is(nullValue()));
	}

	@Test
	void toStringIncludesName() {
		ModuleImpl m = newModule("TestMod");
		assertThat(m.toString().contains("TestMod"), is(true));
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
}
